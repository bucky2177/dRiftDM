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
#' \code{vignette("use_ddm_models", "dRiftDM")}
#'
#'
#' @param drift_dm_obj an object inheriting from [dRiftDM::drift_dm]
#'
#' @param lower,upper numeric vectors specifying the lower and upper bounds on
#'  each parameter to be optimized. Their length have to match the number of
#'  `free_prms` in the model. Note that the i-th `lower` and `upper` element
#'  refers to the i-th parameter listed in `free_prms` of `drift_dm_obj`.
#' @param verbose numeric, indicating the amount of information displayed.
#'  If 0, no information is displayed. If 1 (default), basic information about
#'  the start of Differential Evolution or Nelder-Mead and the final
#'  estimation result is given. If 2, each evaluation of the log-likelihood
#'  function is shown. Note that `verbose` is independent of the information
#'  displayed by [DEoptim::DEoptim].
#' @param use_de_optim logical, indicating whether Differential Evolution via
#'  [DEoptim::DEoptim] should be used. Default is `TRUE`
#' @param use_nmkb logical, indicating whether Nelder-Mead via
#'  [dfoptim::nmkb] should be used. Default is `FALSE`. If both `use_de_optim`
#'  and `use_nmkb` are `TRUE`, then Nelder-Mead follows Differential Evolution.
#'  Note that Nelder-Mead requires a set of starting parameters for which either
#'  the parameter values of `drift_dm_obj` or the estimated parameter values by
#'  Differential Evolution are used.
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
#'  after `steptol = 50` steps, with `reltol = 1e-8` (or if the default itermax
#'  of 200 steps is reached).
#'  Similarly, [dfoptim::nmkb]
#'  will stop if the absolute difference of the log-likelihood between
#'  successive iterations is below `tol = 1e-6`.
#'  See [DEoptim::DEoptim.control] and the details of [dfoptim::nmkb] for
#'  further information.
#'
#' @export
estimate_model <- function(drift_dm_obj, lower, upper, verbose = 0,
                           use_de_optim = TRUE, use_nmkb = FALSE, seed = NULL,
                           de_n_cores = 1,
                           de_control = list(
                             reltol = 1e-8, steptol = 50,
                             itermax = 200, trace = FALSE
                           ),
                           nmkb_control = list(tol = 1e-6)) {
  # user input checks
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }
  if (is.null(drift_dm_obj$obs_data)) {
    warning("No data set, passing back unmodified object")
    return(drift_dm_obj)
  }

  if (!is.numeric(lower) | !is.numeric(upper)) {
    stop("lower or upper are not of type numeric")
  }

  if (length(lower) != length(upper)) {
    stop("length of lower and upper parameters are not equal!")
  }

  if (length(lower) != length(drift_dm_obj$free_prms)) {
    stop(
      "number of parameters in lower/upper don't match the number of",
      " free_prms"
    )
  }

  if (!is.null(names(lower)) | !is.null(names(upper))) {
    if (is.null(names(lower)) & !is.null(names(upper))) {
      stop("upper is a named numeric vector, but lower isn't")
    }
    if (!is.null(names(lower)) & is.null(names(upper))) {
      stop("lower is a named numeric vector, but upper isn't")
    }

    check_if_named_numeric_vector(
      x = lower, var_name = "lower",
      labels = drift_dm_obj$free_prms
    )
    check_if_named_numeric_vector(
      x = upper, var_name = "upper",
      labels = drift_dm_obj$free_prms
    )

    lower <- lower[drift_dm_obj$free_prms]
    lower <- unname(lower)
    upper <- upper[drift_dm_obj$free_prms]
    upper <- unname(upper)
  }

  if (!is.numeric(verbose) | length(verbose) != 1 | !(verbose %in% c(0, 1, 2))) {
    stop("verbose must be numeric of either 0, 1, or 2")
  }

  if (!is.logical(use_de_optim) | length(use_de_optim) != 1) {
    stop("use_de_optim must be logical of length 1")
  }

  if (!is.logical(use_nmkb) | length(use_nmkb) != 1) {
    stop("use_nmkb must be logical")
  }

  if (!use_de_optim & !use_nmkb) {
    warning(
      "use_de_optim and use_nmkb are set to FALSE.",
      " No estimation done, passing back unmodified object"
    )
    return(drift_dm_obj)
  }

  if (!is.null(seed)) {
    if (!is.numeric(seed) | length(seed) != 1) {
      stop("seed must be a single numeric")
    }
    withr::local_preserve_seed()
    set.seed(seed)
  }

  if (!is.numeric(de_n_cores) | de_n_cores <= 0) {
    stop("de_n_cores must be a numeric > 0")
  }

  if (!is.list(de_control)) {
    stop("de_control must be a list")
  }

  if (!is.list(nmkb_control)) {
    stop("nmkb_control must be a list")
  }


  # objective function to minimize
  goal_wrapper <- function(new_model_prms, drift_dm_obj, verbose) {
    drift_dm_obj$prms_model[drift_dm_obj$free_prms] <- new_model_prms
    drift_dm_obj <- re_evaluate_model(drift_dm_obj = drift_dm_obj, eval_model = T)

    if (verbose == 2) {
      current_prms <- prms_to_str(
        prms = drift_dm_obj$prms_model,
        names_prms = names(drift_dm_obj$prms_model)
      )
      cat(
        "\033[33mINFO: Parameters", current_prms,
        "\n\tgave -log_like_val of", -drift_dm_obj$log_like_val, "\033[0m\n",
        sep = " "
      )
    }
    return(-drift_dm_obj$log_like_val)
  }

  # Run DEoptim
  if (use_de_optim) {
    cl <- NULL
    if (de_n_cores > 1) {
      all_funs <- c("goal_wrapper", "re_evaluate_model", "prms_to_str")
      cl <- parallel::makeCluster(de_n_cores)
      parallel::clusterExport(
        cl = cl,
        varlist = as.list(all_funs),
        envir = environment()
      )
    }

    controls <- DEoptim::DEoptim.control(cluster = cl)
    controls <- utils::modifyList(controls, de_control)

    de_out <-
      tryCatch(
        expr = {
          if (verbose >= 1) {
            cat("INFO: Running differential evolution\n")
          }
          DEoptim::DEoptim(
            fn = goal_wrapper, lower = lower, upper = upper,
            control = controls, drift_dm_obj = drift_dm_obj,
            verbose = verbose
          )
        },
        error = function(e) {
          if (!is.null(cl)) parallel::stopCluster(cl)
          warning("an error occured: ", e, "\n returning NULL")
          return(e)
        }
      )
    if (!is.null(cl)) parallel::stopCluster(cl)
    if (is.null(de_out)) {
      return(NULL)
    }
  }

  # choose "new" starting values
  if (use_de_optim) {
    start_vals <- as.numeric(de_out$optim$bestmem)
  } else {
    start_vals <- drift_dm_obj$prms_model[drift_dm_obj$free_prms]
    start_vals <- as.numeric(start_vals)
  }

  # check if nmkb is requested, but the parameter space is univariate
  if (use_nmkb & length(start_vals) == 1) {
    warning(
      "Nelder-Mead is not applicable for univariate optimization.",
      " Skipping Nelder-Mead."
    )
    use_nmkb <- F
  }

  # run nmkb
  if (use_nmkb) {
    if (verbose >= 1) {
      cat("INFO: Running bounded Nelder-Mead\n")
    }

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
  names(final_vals) <- drift_dm_obj$free_prms

  # set parameters an evaluate fully
  drift_dm_obj <- set_model_prms(
    drift_dm_obj = drift_dm_obj,
    new_prm_vals = final_vals,
    eval_model = T
  )

  final_vals <- prms_to_str(
    prms = drift_dm_obj$prms_model,
    names_prms = names(drift_dm_obj$prms_model)
  )
  if (verbose >= 1) {
    cat(
      "\033[33mINFO: Estimation gave", final_vals,
      "\n\twith -log_like_val of", -drift_dm_obj$log_like_val, "\033[0m\n",
      sep = " "
    )
  }

  return(drift_dm_obj)
}
