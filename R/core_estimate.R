# ===== FUNCTION FOR ESTIMATING THE PARAMETERS OF A MODEL

#' Estimate the parameters of a drift_dm model
#'
#' @description
#' Find the 'best' parameter settings by fitting a [dRiftDM::drift_dm] models'
#' predicted probability density functions to the observed data
#' stored within the respective object. The fitting procedure is done by
#' minimizing the negative log-likelihood of the model.
#'
#' Users have three options:
#' * Estimate the parameters via Differential Evolution (Default)
#' * Estimate the parameters via (bounded) Nelder-Mead
#' * Use Differential Evolution followed by Nelder-Mead.
#'
#' More detailed information on how to use this function can be found in
#' [vignette("use_ddm_models", "dRiftDM")]
#'
#'
#' @param drift_dm_obj an object inheriting from [dRiftDM::drift_dm]
#'
#' @param lower,upper numeric vectors specifying the lower and upper bounds on
#'  each parameter to be optimized. Their length have to match the number of
#'  `free_prms` in the model. Note that the i-th `lower` and `upper` element
#'  refers to the i-th parameter listed in `free_prms` of `drift_dm_obj`.
#' @param verbose logical, indicating if the parameter values and the negative
#'  log-likelihood should be printed at each evaluation of the model. Default is
#'  `FALSE`.
#' @param use_de_optim logical, indicating whether Differential Evolution via
#'  [DEoptim::DEoptim] should be used. Default is `TRUE`
#' @param use_nmkb logical, indicating whether Nelder-Mead via
#'  [dfoptim::nmkb] should be used. Default is `FALSE`. If both `use_de_optim`
#'  and `use_nmkb` are `TRUE`, then Nelder-Mead follows Differential Evolution
#' @param seed a single numeric, providing a seed for the Differential Evolution
#'  algorithm
#' @param de_n_cores a single numeric, indicating the number of cores to use.
#'  Run [parallel::detectCores()] to see how many cores are available on your
#'  machine. Note that it is generally not recommended to use all of your cores
#'  as this will drastically slow down your machine for any additional task.
#' @param de_control,nmkb_control lists of additional control parameters passed
#'  to [DEoptim::DEoptim] and [dfoptim::nmkb]. Default settings will lead
#'  [DEoptim::DEoptim] to stop if the algorithm is unable to reduce the
#'  negative log-likelihood by a factor of `reltol * (abs(val) + reltol)`
#'  after `steptol = 50` steps, with `reltol = 1e-9` (or if the default itermax
#'  of 200 steps is reached).
#'  Similarly, [dfoptim::nmkb]
#'  will stop if the absolute difference of the log-likelihood between
#'  successive iterations is below `tol = 1e-6`.
#'  See [DEoptim::DEoptim.control] and the details of [dfoptim::nmkb] for
#'  further information.
#'
#' @export
estimate_model <- function(drift_dm_obj, lower, upper, verbose = FALSE,
                           use_de_optim = TRUE, use_nmkb = FALSE, seed = NULL,
                           de_n_cores = 1,
                           de_control = list(reltol = 1e-9, steptol = 50,
                                             itermax  = 200),
                           nmkb_control = list(tol = 1e-6)) {
  # user input checks
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }
  if (is.null(drift_dm_obj$obs_data)) {
    warning("No data set, passing back unmodified object")
  }
  if (!is.numeric(lower) | !is.numeric(upper)) {
    stop("lower or upper are not of type numeric")
  }
  if (length(lower) != length(upper)) {
    stop("length of lower and upper parameters are not equal!")
  }
  if (length(lower) != length(drift_dm_obj$free_prms)) {
    stop("number of parameters in lower/upper don't match the number of",
         " free_prms")
  }
  if (!is.logical(verbose) | length(verbose) != 1)
    stop("verbose must be logical of length 1")
  if (!is.logical(use_de_optim) | length(use_de_optim) != 1)
    stop("use_de_optim must be logical of length 1")
  if (!is.logical(use_nmkb) | length(use_nmkb) != 1)
    stop("use_nmkb must be logical")
  if (!use_de_optim & !use_nmkb) {
    warning("No estimation done, passing back unmodified object")
    return(drift_dm_obj)
  }
  if (!is.null(seed)) {
    if (!is.numeric(seed) | length(seed) != 1) {
      stop("seed must be a single numeric")
    }
    withr::local_seed(seed)
  }
  if (!is.numeric(de_n_cores) | de_n_cores <= 0) {
    stop("de_n_cores must be a numeric > 0")
  }
  if (!is.list(de_control))
    stop("de_control must be a list")

  if (!is.list(nmkb_control))
    stop("nmkb_control must be a list")


  # objective function to minimize
  goal_wrapper <- function(new_model_prms, drift_dm_obj, verbose) {
    drift_dm_obj <- set_model_prms(
      drift_dm_obj = drift_dm_obj,
      new_model_prms = new_model_prms,
      eval_model = T
    )
    if (verbose) {
      current_prms <- prms_to_str(
        prms = drift_dm_obj$prms_model,
        names_prms = names(drift_dm_obj$prms_model)
      )
      cat(
        "\n\033[33m", "INFO: Parameters", current_prms,
        "\n\tgave -log_like_val of", -drift_dm_obj$log_like_val, "\033[0m\n"
      )
    }
    return(-drift_dm_obj$log_like_val)
  }

  # Run DEoptim
  if (use_de_optim) {
    cl <- NULL
    if (de_n_cores > 1) {
      cl <- parallel::makeCluster(de_n_cores)
      parallel::clusterExport(
        cl = cl,
        varlist = list("goal_wrapper", "set_model_prms",
                       "prms_to_str"),
        envir = environment()
      )
    }

    controls <- DEoptim::DEoptim.control(cluster = cl)
    controls <- utils::modifyList(controls, de_control)

    de_out =
      tryCatch(
        expr = {
          cat("\nINFO: Running differential evolution\n")
          DEoptim::DEoptim(
            fn = goal_wrapper, lower = lower, upper = upper,
            control = controls, drift_dm_obj = drift_dm_obj,
            verbose = verbose
          )
        },
        error = function(e) {
          if (!is.null(cl)) parallel::stopCluster(cl)
          warning("an error occured: ", e, "\n returning unmodified object")
          return(NULL)
        }
      )
    if (!is.null(cl)) parallel::stopCluster(cl)
    if (is.null(de_out)) {
      return(drift_dm_obj)
    }
  }

  # choose "new" starting values
  if (use_de_optim) {
    start_vals <- as.numeric(de_out$optim$bestmem)
  } else {
    start_vals <- drift_dm_obj$prms_model[names(drift_dm_obj$prms_model) %in%
      drift_dm_obj$free_prms]
    start_vals <- as.numeric(start_vals)
  }

  # Run nmkb
  if (use_nmkb) {
    cat("\nINFO: Running bounded Nelder-Mead")

    nmkb_out <- dfoptim::nmkb(
      par = start_vals, fn = goal_wrapper, lower = lower,
      upper = upper, control = nmkb_control,
      drift_dm_obj = drift_dm_obj, verbose = verbose
    )
  }

  # choose final parameters
  if (use_nmkb) {
    final_vals <- as.numeric(nmkb_out$par)
  } else {
    final_vals <- as.numeric(start_vals)
  }

  # set parameters
  drift_dm_obj <- set_model_prms(
    drift_dm_obj = drift_dm_obj,
    new_model_prms = final_vals,
    eval_model = T
  )

  final_vals <- prms_to_str(
    prms = drift_dm_obj$prms_model,
    names_prms = names(drift_dm_obj$prms_model)
  )
  cat(
    "\n\033[33m", "INFO: Estimation gave", final_vals,
    "\n\tgave -log_like_val of", -drift_dm_obj$log_like_val, "\033[0m\n"
  )

  return(drift_dm_obj)
}
