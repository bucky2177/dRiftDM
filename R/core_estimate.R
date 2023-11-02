# ===== FUNCTION FOR ESTIMATING THE PARAMETERS OF A MODEL

estimate_model <- function(drift_dm_obj, lower, upper, verbose = FALSE,
                           use_de_optim = TRUE, polish = TRUE, seed = NULL,
                           de_n_cores = 1, de_control = list(trace = F),
                           nmkb_control = list()) {
  # user input checks
  if (!is.numeric(lower) | !is.numeric(upper)) {
    stop("lower or upper are not of type numeric")
  }
  if (length(lower) != length(upper)) {
    stop("length of lower and upper parameters are not equal!")
  }

  if (!is.logical(use_de_optim)) stop("use_de_optim must be logical")
  if (!is.logical(polish)) stop("polish must be logical")
  if (length(lower) != length(drift_dm_obj$free_prms)) {
    stop("number of parameters in lower don't match the number of free_prms")
  }

  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1) {
      stop("seed must be a single numeric")
    }
    withr::local_seed(seed)
  }

  if (!use_de_optim & !polish) {
    warning("No estimation done, passing back unmodified object")
    return(drift_dm_obj)
  }

  if (is.null(drift_dm_obj$obs_data)) {
    warning("No data set, passing back unmodified object")
  }


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
    cat("\nINFO: Running differential evolution\n")

    cl <- NULL
    if (de_n_cores > 1) {
      cl <- parallel::makeCluster(de_n_cores)
      parallel::clusterExport(
        cl = cl,
        varlist = c(utils::lsf.str("package:dRiftDM")),
        envir = environment()
      )
    }

    controls <- DEoptim::DEoptim.control(cluster = cl)
    controls <- utils::modifyList(controls, de_control)
    de_out =
    tryCatch(
      expr = {
        de_out <- DEoptim::DEoptim(
          fn = goal_wrapper, lower = lower, upper = upper,
          control = controls, drift_dm_obj = drift_dm_obj,
          verbose = verbose
        )
        if (!is.null(cl)) parallel::stopCluster(cl)
      },
      error = function(e) {
        if (!is.null(cl)) parallel::stopCluster(cl)
        warning("an error occured: ", e, "\n returning unmodified object")
        return(NULL)
      }
    )
    if (is.null(de_out)) return(drift_dm_obj)
  }

  # choose "new" starting values
  if (use_de_optim) {
    start_vals <- as.numeric(de_out$optim$bestmem)
  } else {
    start_vals = drift_dm_obj$prms_model[names(drift_dm_obj$prms_model) %in% drift_dm_obj$free_prms]
    start_vals <- as.numeric(start_vals)
  }

  # Run nmkb
  if (polish) {
    cat("\nINFO: Running bounded Nelder-Mead")

    nmkb_out <- dfoptim::nmkb(
      par = start_vals, fn = goal_wrapper, lower = lower,
      upper = upper, control = nmkb_control,
      drift_dm_obj = drift_dm_obj, verbose = verbose
    )
  }

  # choose final parameters
  if (polish) {
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
