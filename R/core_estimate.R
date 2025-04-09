# FUNCTIONS FOR ESTIMATING THE PARAMETERS OF A MODEL -----------------------

#' Estimate the Parameters of a drift_dm Model
#'
#' @description
#' Find the 'best' parameter settings by fitting a [dRiftDM::drift_dm] models'
#' predicted probability density functions (PDFs) to the observed data
#' stored within the respective object. The fitting procedure is done by
#' minimizing the negative log-likelihood of the model.
#'
#' Users have three options:
#' * Estimate the parameters via Differential Evolution (Default)
#' * Estimate the parameters via (bounded) Nelder-Mead
#' * Use Differential Evolution followed by Nelder-Mead.
#'
#' See also \code{vignette("dRiftDM", "dRiftDM")}
#'
#'
#' @param drift_dm_obj an object inheriting from [dRiftDM::drift_dm]
#'
#' @param lower,upper numeric vectors or lists, specifying the lower and upper
#'  bounds on each parameter to be optimized (see Details).
#' @param verbose numeric, indicating the amount of information displayed.
#'  If 0, no information is displayed (default). If 1, basic information about
#'  the start of Differential Evolution or Nelder-Mead and the final
#'  estimation result is given. If 2, each evaluation of the log-likelihood
#'  function is shown. Note that `verbose` is independent of the information
#'  displayed by [DEoptim::DEoptim].
#' @param use_de_optim logical, indicating whether Differential Evolution via
#'  [DEoptim::DEoptim] should be used. Default is `TRUE`
#' @param use_nmkb logical, indicating whether Nelder-Mead via
#'  [dfoptim::nmkb] should be used. Default is `FALSE`.
#' @param seed a single numeric, providing a seed for the Differential Evolution
#'  algorithm
#' @param de_n_cores a single numeric, indicating the number of cores to use.
#'  Run [parallel::detectCores()] to see how many cores are available on your
#'  machine. Note that it is generally not recommended to use all of your cores
#'  as this will drastically slow down your machine for any additional task.
#' @param de_control,nmkb_control lists of additional control parameters passed
#'  to [DEoptim::DEoptim] and [dfoptim::nmkb].
#'
#' @returns the updated `drift_dm_obj` (with the estimated parameter values,
#'  log-likelihood, and probability density functions of the first passage time)
#'
#' @details
#'
#' ## Specifying lower/upper
#'
#' the function `estimate_model` provides a flexible way of specifying the
#' search space; identical to specifying the parameter simulation space in
#' [dRiftDM::simulate_data.drift_dm].
#'
#' Users have three options to specify the simulation space:
#'
#' * Plain numeric vectors (not very much recommended). In this case,
#' `lower/upper` must be sorted in accordance with the parameters in the
#' `flex_prms_obj` object that vary for at least one condition
#' (call `print(drift_dm_obj)` and have a look at the `Unique Parameters`
#' output)
#'
#' * Named numeric vectors. In this case `lower/upper` have to provide labels
#' in accordance with the parameters that are considered "free" at least once
#' across conditions.
#'
#' * The most flexible way is when `lower/upper` are lists. In this case, the
#' list requires an entry called "default_values" which specifies the named or
#' plain numeric vectors as above. If the list only contains this entry, then
#' the behavior is as if `lower/upper` were already numeric vectors. However,
#' the `lower/upper` lists can also provide entries labeled as specific
#' conditions, which contain named (!) numeric vectors with parameter labels.
#' This will modify the value for the upper/lower parameter space with respect
#' to the specified parameters in the respective condition.
#'
#' ## Details on Nelder-Mead and Differential Evolution
#'
#' If both `use_de_optim` and `use_nmkb` are `TRUE`, then Nelder-Mead follows
#' Differential Evolution. Note that Nelder-Mead requires a set of starting
#' parameters for which either the parameter values of `drift_dm_obj` or the
#' estimated parameter values by Differential Evolution are used.
#'
#' Default settings will lead [DEoptim::DEoptim] to stop if the algorithm is
#' unable to reduce the negative log-likelihood by a factor of
#' `reltol * (abs(val) + reltol)`after `steptol = 50` steps, with
#' `reltol = 1e-8` (or if the default itermax of 200 steps is reached).
#' Similarly, [dfoptim::nmkb] will stop if the absolute difference of the
#' log-likelihood between successive iterations is below `tol = 1e-6`.See
#' [DEoptim::DEoptim.control] and the details of [dfoptim::nmkb] for
#' further information.
#'
#' @examples
#'
#' # the example uses a simple model and the Nelder-Mead minimization
#' # routine to ensure that it runs in a couple of seconds.
#'
#' # get a model and attach data to the model
#' my_model <- ratcliff_dm(t_max = 1.5, dx = .005, dt = .005)
#' obs_data(my_model) <- ratcliff_synth_data # this data set comes with dRiftDM
#'
#'
#' # set the search space
#' lower <- c(muc = 1, b = 0.2, non_dec = 0.1)
#' upper <- c(muc = 7, b = 1.0, non_dec = 0.6)
#'
#' # then fit the data to the model using Nelder-Mead after setting some start
#' # values
#' coef(my_model) <- c(muc = 2, b = 0.5, non_dec = 0.4)
#' my_model <- estimate_model(
#'   drift_dm_obj = my_model, # (starting values are those set to the model)
#'   lower = lower, # lower and upper parameter ranges
#'   upper = upper,
#'   use_de_optim = FALSE, # don't use the default diff. evol. algorithm
#'   use_nmkb = TRUE # but Nelder-Mead (faster, but way less robust)
#' )
#'
#' # show the result
#' print(my_model)
#'
#' @seealso [dRiftDM::estimate_model_ids]
#'
#'
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



  # get lower upper vectors
  lower_upper <- get_lower_upper_smart(
    drift_dm_obj = drift_dm_obj,
    lower = lower,
    upper = upper,
    labels = FALSE
  )
  lower <- lower_upper$lower
  upper <- lower_upper$upper

  # continue checks
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
    # set (must re_evaluate! Did not use set_x_2_flex_prms to circumpass
    # validate_drift_dm)
    drift_dm_obj$flex_prms_obj <- x2prms_vals(
      x = new_model_prms,
      flex_prms_obj = drift_dm_obj$flex_prms_obj
    )

    # evaluate
    tryCatch(
      {
        drift_dm_obj <- re_evaluate_model(drift_dm_obj = drift_dm_obj,
                                          eval_model = TRUE)
      },
      error = function(e) {
        prms <- prms_to_str(drift_dm_obj)
        stop("Evaluation of the model failed, tried values: \n", prms)
      }
    )

    # maybe print
    if (verbose == 2) {
      prms <- prms_to_str(x = drift_dm_obj)
      cat(
        "\033[33mINFO: Parameters\n", prms,
        "\n==> gave -log_like_val of ", -drift_dm_obj$log_like_val, "\033[0m\n",
        sep = ""
      )
    }
    return(-drift_dm_obj$log_like_val)
  }

  # Run DEoptim
  if (use_de_optim) {
    # create clusters
    cl <- NULL
    if (de_n_cores > 1) {
      all_funs <- c(
        "goal_wrapper", "re_evaluate_model", "prms_to_str",
        "x2prms_vals", "update_special_values",
        "drift_dm_default_rounding", "is_numeric"
      )
      cl <- parallel::makeCluster(de_n_cores)
      parallel::clusterExport(
        cl = cl,
        varlist = as.list(all_funs),
        envir = environment()
      )
    }

    # set control parameters
    controls <- DEoptim::DEoptim.control(cluster = cl)
    controls <- utils::modifyList(controls, de_control)

    # actually run the function
    if (verbose >= 1) {
      cat("INFO: Running differential evolution\n")
    }
    de_out <-
      tryCatch(
        expr = {
          DEoptim::DEoptim(
            fn = goal_wrapper, lower = lower, upper = upper,
            control = controls, drift_dm_obj = drift_dm_obj,
            verbose = verbose
          )
        },
        error = function(e) {
          if (!is.null(cl)) parallel::stopCluster(cl)
          warning("an error occured: ", e, "\n returning NULL")
          return(NULL)
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
    start_vals <- as.numeric(coef(drift_dm_obj, select_unique = TRUE))
  }

  # check if nmkb is requested, but the parameter space is univariate
  if (use_nmkb & length(start_vals) == 1) {
    warning(
      "Nelder-Mead is not applicable for univariate optimization.",
      " Skipping Nelder-Mead."
    )
    use_nmkb <- FALSE
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
    final_vals <- start_vals
  }


  # set parameters an evaluate fully
  coef(drift_dm_obj, eval_model = TRUE) <- final_vals

  if (verbose >= 1) {
    prms <- prms_to_str(drift_dm_obj)
    cat(
      "\033[33mINFO: Parameters\n", prms,
      "\n==> gave -log_like_val of ", -drift_dm_obj$log_like_val, "\033[0m\n",
      sep = ""
    )
  }

  return(drift_dm_obj)
}
