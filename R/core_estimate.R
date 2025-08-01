

# Ich will
# - Schätzung eines Modells, so wie immer
# - Aggregate Schätzung
# - separate Schätzung

#' @param drift_dm_obj
#'
#' @param obs_data
#' @param approach
#' @param framework
#' @param optimizer
#' @param control
#' @param n_cores
#' @param lower
#' @param upper
#' @param start_vals
#' @param mean
#' @param sd
#' @param shape
#' @param rate
#' @param seed
#' @param n_chains
#' @param burn_in
#' @param samples
#' @param prob_migration
#' @param prob_re_eval
#' @param progress
#' @param messaging
#' @param parallelization_strategy
#' @param ...
#' @param x a `mcmc_dm` object as returned by `estimate_bayesian()`.
#' @param round_digits an integer, defining the number of digits for rounding
#' the output.
#'
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
#' @examples
#' # We'll provide a somewhat unrealistic example, trimmed for speed.
#' # In practice, users likely employ more complex models and more individuals.
#' # However, a more realistic example would take minutes (and maybe even hours)
#' # and is therefore not suitable for an example.
#'
#' # Fit the Ratcliff model to synthetic data --------------------------------
#' # get the model (pre-built by dRiftDM)
#' model <- ratcliff_dm(t_max = 2.0, dx = .005, dt = .005)
#'
#' # define an upper and lower boundary for the parameter space
#' lower <- c(muc = 1, b = 0.2, non_dec = 0.1)
#' upper <- c(muc = 7, b = 1.0, non_dec = 0.6)
#'
#' # simulate synthetic data for demonstration purpose
#' synth_data_prms <- simulate_data(
#'   model,
#'   n = 100, k = 2, lower = lower, upper = upper, seed = 1
#' )
#' synth_data <- synth_data_prms$synth_data
#'
#' # finally, call the fit procedure. To increase speed, we'll use the
#' # Nelder-Mead minimization routine. Note: We'll save the fits in tempdir()
#' # to avoid writing to a user's file directory without explicit permission.
#' estimate_model_ids(
#'   drift_dm_obj = model, # which model (the Ratcliff model)
#'   obs_data_ids = synth_data, # which data (the synthetic data set)
#'   lower = lower, # the lower and upper parameter/search space
#'   upper = upper,
#'   fit_procedure_name = "example", # a label for the fit procedure
#'   fit_path = tempdir(), # temporary directory (replace, e.g., with getwd())
#'   use_nmkb = TRUE, # use Nelder-Mead (fast, but less robust)
#'   use_de_optim = FALSE # and not differential evolution
#' )
#'
#' \dontshow{
#' unlink(file.path(tempdir(), "drift_dm_fits"), recursive = TRUE)
#' }
#'
#' @export
estimate_dm <- function(drift_dm_obj, obs_data = NULL,
                        approach = NULL, framework = NULL, optimizer = NULL,
                        control = list(), n_cores = 1, lower = NULL,
                        upper = NULL, start_vals = NULL, means = NULL,
                        sds = NULL, shapes = NULL, rates = NULL,
                        n_chains = 40, burn_in = 500, samples = 1000,
                        prob_migration = 0.1, prob_re_eval = 1,
                        messaging = TRUE, parallelization_strategy = 1,
                        seed = NULL, ...) {

  dots = list(...)

  # user input checks ####
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }

  # check if it can be evaluated
  drift_dm_obj = re_evaluate_model(drift_dm_obj)

  # use data from the model or from the obs_data object
  if (is.null(obs_data)) {

    if (is.null(drift_dm_obj$obs_data) & is.null(drift_dm_obj$stats_agg)) {
      stop("No observed data was supplied via the 'obs_data' argument, and no ",
           "observed data is attached to the model. Parameter estimation is ",
           "therefore not possible.")
    }

    if (messaging) {
      message("Using the data attached to the model")
    }
    which_data = "model"

  } else {

    # check if everything is ok
    b_coding <- b_coding(drift_dm_obj)
    b_column <- b_coding$column
    u_name_value <- b_coding$u_name_value
    l_name_value <- b_coding$l_name_value

    obs_data <- check_reduce_raw_data(
      obs_data,
      b_coding_column = b_column,
      u_value = u_name_value,
      l_value = l_name_value
    )

    obs_data(drift_dm_obj) <- NULL # to ensure nothing weird is happening

    # check for t_max and conds
    model_conds <- conds(drift_dm_obj)
    data_cond <- conds(obs_data)
    if (!all(data_cond %in% model_conds)) {
      warning(
        "The Cond column in 'obs_data' provides a condition that is ",
        "not listed in the model's conditions. This condition was dropped."
      )
      obs_data <- obs_data[obs_data$Cond %in% model_conds, ]
    }

    if (drift_dm_obj$prms_solve[["t_max"]] < max(obs_data$RT)) {
      stop(
        "the 't_max' settings in 'drift_dm_obj' is smaller than the maximum ",
        "RT. Please increase 't_max' before calling 'estimate_dm()' again."
      )
    }

    # print out processing message and continue
    if (messaging) {
      message("Using the data supplied via the 'obs_data' argument")
    }
    which_data = "obs_data"
  }
  stopifnot(which_data %in% c("model", "obs_data"))

  # check the approach settings and set default values
  if (is.null(approach)) {
    approach = "NULL"
  }
  approach <- match.arg(
    approach, choices = c("NULL", "aggregated", "hierarchical")
  )

  # check the framework and set default values
  if (is.null(framework)) {
    framework = "classical"
  }
  framework <- match.arg(
    framework, choices = c("classical", "bayesian")
  )

  # check the combinations (currently, not everything is implemented)
  if (approach %in% c("separately", "aggregated")) {
    if (messaging) {
      message("Approach '", approach, "' requested, using the 'classical' ",
              "estimation framework")
    }
    framework = "classical"
  }
  if (approach == "hierarchical") {
    if (messaging) {
      message("Approach '", approach, "' requested, using the 'bayesian' ",
              "estimation framework")
    }
    framework = "bayesian"
  }


  # check the optimizer or use default
  requ_optimizer = NULL
  if (approach == "hierarchical") {
    requ_optimizer = "DE-MCMC"
  }
  if (framework == "bayesian") {
    requ_optimizer = "DE-MCMC"
  }

  # if the user did not specify an optimizer, choose for them
  if (is.null(optimizer)) {
    if (is.null(requ_optimizer)) {
      if (!is.null(lower) | !is.null(upper)) {
        optimizer = "DEoptim"
      } else {
        optimizer = "nmk"
      }
    } else {
      optimizer = requ_optimizer
    }
  } else {
    # if the user did specify an optimizer, maybe override it
    if (!is.null(requ_optimizer) && !identical(requ_optimizer, optimizer)) {
      message("The provided optimizer ('", optimizer, "') is not compatible ",
              "with the specified approach ('", approach, "') and framework ",
              "('", framework, "'). Using the following optimizer instead: '",
              requ_optimizer, "'.")
    }
    optimizer = requ_optimizer
  }

  # check if the optimizer is implemented
  optimizer <- match.arg(
    optimizer, choices = c("nmkb", "nmk", "BFGS", "L-BFGS-B", "DEoptim",
                           "DE-MCMC", "TIDE")
  )

  # check n_cores
  if (!is_numeric(n_cores) || length(n_cores) != 1 || n_cores <= 0) {
    stop("n_cores must be a single numeric >= 0")
  }

  # check seed input
  if (!is.null(seed)) {
    if (!is.numeric(seed) | length(seed) != 1)
      stop ("seed must be a single numeric")
    withr::local_preserve_seed()
    set.seed(seed)
  }

  # now handle/dispatch the separate strategies ####
  # aggregated ####
  if (approach == "aggregated") {
    cost_fun = cost_function(drift_dm_obj)
    if (cost_fun != "rmse") {
      cost_function(drift_dm_obj) <- "rmse"
      message("Changing the 'cost_function' to 'rmse'")
    }

    if (which_data == "model" || !("ID" %in% colnames(obs_data))) {
      stop("Cannot fit the model to aggregated data without data from multiple ",
           "participants supplied via the 'obs_data' argument. ",
           "Please provide individual-level data (with an ID column).")
    }

    # aggregate the data
    data_available = !(is.null(drift_dm_obj$obs_data) &
      is.null(drift_dm_obj$stats_agg))
    if (messaging & data_available) {
      message("Aggregated data has been set to the model. The previously ",
              "attached data has been overridden.")
    } else if (messaging) {
      message("Aggregated data has been set to the model")
    }
    drift_dm_obj = set_agg_data(
      drift_dm_obj = drift_dm_obj, obs_data_ids = obs_data, probs = dots$probs,
      n_bins = dots$n_bins
    )

    # then call the estimation function for a single model
    drift_dm_obj <- estimate_classical(
      drift_dm_obj = drift_dm_obj, optimizer = optimizer,
      start_vals = dots$start_vals, return_runs = dots$return_runs,
      lower = lower, upper = upper, verbose = dots$verbose, de_n_cores = n_cores,
      control = control, rounding = dots$rounding
    )

    # wrap up the return value and create a custom class
    fit_obj = list(drift_dm_obj = drift_dm_obj, obs_data_ids = obs_data)
    class(fit_obj) = "fits_agg_dm"
    return(fit_obj)
  }


  # interim: handle the NULL (default) setting (can lead to fitting just
  # one participant or multiple participants separately)
  if (approach == "NULL") {
    if (which_data == "obs_data" && !("ID" %in% colnames(obs_data))) {
      approach = "one_subj"
      obs_data(drift_dm_obj) <- obs_data
      if (messaging) {
        message("No 'ID' column found in the supplied 'obs_data'. ",
                "Attaching the data to the model assuming a single-participant ",
                "setup.")
      }
    }
    if (which_data == "obs_data") {
      approach = "separately"
    }
    if (which_data == "model") {
      approach = "one_subj"
    }
  }

  # handle the original case: Fit a model to just one participant
  # one_subj ####
  if (approach == "one_subj") {
    if (messaging) {
      message("Fitting the model to the data of one participant")
    }
    if (framework == "classical") {
      drift_dm_obj <- estimate_classical(
        drift_dm_obj = drift_dm_obj, optimizer = optimizer, lower = lower,
        upper = upper, verbose = dots$verbose, de_n_cores = n_cores,
        control = control, rounding = dots$rounding
      )
      return(drift_dm_obj)
    }

    if (framework == "bayesian") {
      mcmc_out = estimate_bayesian(
        drift_dm_obj = drift_dm_obj, sampler = optimizer,
        n_chains = n_chains, burn_in = burn_in, samples = samples,
        prob_migration = prob_migration, prob_re_eval = prob_re_eval,
        progress = dots$progress, means = means, sds = sds,
        lower = lower, upper = upper, shapes = shapes, rates = rates
      )
      return(mcmc_out)
    }
  }

  # continue with hierarchical or separately (both require a valid obs_data
  # input)
  if (which_data != "obs_data") {
    stop ("The specified approach ('", approach, "') requires observed data ",
          "via the 'obs_data' argument")
  }
  if (!("ID" %in% colnames(obs_data))) {
    stop("No 'ID' column found in 'obs_data'")
  }

  # now call the Bayesian-hierarchical estimation approach
  # hierarchical ####
  if (approach == "hierarchical") {

    if (length(unique(obs_data$ID)) <= 1) {
      stop("The 'ID' column provides only one participant. A hierarchical",
           " approach doesn't make sense in this case.")
    }
    if (messaging) {
      message("Fitting the model hierarchically")
    }

    mcmc_out = estimate_bayesian(
      drift_dm_obj = drift_dm_obj, obs_data_ids = obs_data, sampler = optimizer,
      n_chains = n_chains, burn_in = burn_in, samples = samples,
      prob_migration = prob_migration, prob_re_eval = prob_re_eval,
      progress = dots$progress, n_cores = n_cores, means = means, sds = sds,
      lower = lower, upper = upper, shapes = shapes, rates = rates
    )
    return(mcmc_out)
  }

  # finally, handle the last case
  # separately ####
  if (approach == "separately") {
    if (messaging) {
      message("Fitting the model separately for each participant")
    }
    all_fits = estimate_classical_wrapper(
      drift_dm_obj = drift_dm_obj, obs_data_ids = obs_data,
      parallelization_strategy = parallelization_strategy,
      progress = dots$progress, start_vals = start_vals,
      optimizer = optimizer, lower = lower, upper = upper,
      verbose = dots$verbose, n_cores = n_cores, control = control,
      rounding = dots$rounding
    )
  }
}




# CLASSICAL OPTIMIZATION --------------------------------------------------

#' Estimate Parameters of a `drift_dm` Model via Classical Optimization
#'
#' @description
#' `estimate_classical()` estimates the parameters of a [dRiftDM::drift_dm]
#' model by minimizing the model's cost function (e.g., RMSE or negative
#' log-likelihood) using classical (non-Bayesian) optimization routines.
#'
#' Available optimizers include:
#' * Nelder-Mead (bounded or unbounded): `"nmk"`, `"nmkb"` (via [dfoptim::nmk()]
#'  and [dfoptim::nmkb()])
#' * BFGS and L-BFGS-B (via [stats::optim()])
#' * Differential Evolution (via [DEoptim::DEoptim()])
#'
#'
#' @param drift_dm_obj an object inheriting from [dRiftDM::drift_dm].
#' @param optimizer a character string specifying the optimizer to use.
#'   Must be one of `"nmk"`, `"nmkb"`, `"BFGS"`, `"L-BFGS-B"`, or `"DEoptim"`.
#' @param start_vals a set of starting values. Must be compatible with
#' [dRiftDM::get_parameters_smart()]. If `start_vals`
#' is not `NULL`, the function tries to set the provided parameter values to
#' the model, using those values as starting values for the optimization routine.
#' Special case: If start_vals is a `data.frame`, the function is recursively
#' called for each row of `start_vals`, providing a handy way to run an
#' optimization routine with different starting values. Default is `NULL`,
#' which implies that the current model parameters are used as starting
#' values.
#' @param return_runs a single logical. Only relevant when `start_vals` is
#' a `data.frame` and the optimization routine is called multiple times with
#' different starting values. If `FALSE`, the best-fitting model
#' is returned. If `TRUE`, a list is returned, containing the best-fitting model,
#' all cost values across runs, and all estimated model parameters across runs.
#' @param lower,upper bounds on the parameters to be estimated.
#'   Can be numeric vectors, named vectors, or flexible lists (see Details).
#' @param verbose an integer (0, 1, or 2). Controls the amount of printed output.
#'   * 0 = silent
#'   * 1 = starting/exiting messages
#'   * 2 = all parameters and the cost value per iteration
#' @param de_n_cores an integer > 0. Number of CPU cores to use for `DEoptim`.
#' @param control a named list of control parameters passed to the chosen
#' optimizer.
#' @param rounding an integer. Number of digits to round cost values in printed
#'   output. If `NULL`, defaults to [dRiftDM::drift_dm_default_rounding()].
#'
#' @return The updated `drift_dm_obj`, with optimized parameters.
#'
#' @details
#'
#' ## Search space specification
#' `lower` and `upper` can be specified flexibly:
#'
#' * As unnamed numeric vectors (not recommended unless you're sure of the
#'   parameter order)
#' * As named numeric vectors matching the parameter names of the model
#' * As lists with a `default_values` entry (plus optional condition-specific
#'   entries)
#'
#' This design mirrors the structure used in [dRiftDM::simulate_data.drift_dm()].
#'
#' ## Optimization details
#'
#' Some optimizers (i.e., `"nmkb"`, `"L-BFGS-B"`, `"DEoptim"`) require both
#' `lower` and `upper` bounds.
#'
#' Differential Evolution (`DEoptim`) supports parallelization across cores via
#' `de_n_cores`. If `de_n_cores > 1`, a parallel cluster is created and
#' automatically closed after optimization.
#'
#' The cost function being minimized depends on the [dRiftDM::cost_function()]
#' of the model.
#'
#' During optimization, failed model evaluations yield a very high
#' cost value (i.e., `.Machine$double.xmax`). In some cases, this ensures that
#' the optimization doesn't crash, though, this is not guaranteed.
#'
#' @keywords internal
estimate_classical <- function(drift_dm_obj, optimizer, start_vals = NULL,
                               return_runs = NULL,
                               lower = NULL, upper = NULL, verbose = NULL,
                               de_n_cores = 1,
                               control = list(), rounding = NULL) {
  # input checks
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }


  optimizer <- match.arg(
    optimizer,
    choices = c("nmkb", "nmk", "BFGS", "L-BFGS-B", "DEoptim")
  )

  # set start_vals or recursive call
  if (!is.null(start_vals)) {

    if (is.null(return_runs)) {
      return_runs = FALSE
    }
    if (!is.logical(return_runs) | length(return_runs) != 1) {
      stop("'return_runs' must be a single logical")
    }

    # if start_vals is not a data.frame, try to set it and continue
    if (!is.data.frame(start_vals)) {
      prm <- get_parameters_smart(
        drift_dm_obj = drift_dm_obj,
        input_a = start_vals
      )$vec_a
      coef(drift_dm_obj) <- prm
    } else {
      # otherwise call the function recursively (but not for DEoptim)
      if (!is.data.frame(start_vals)) {
        stop("'start_vals' must be a data.frame or a numeric vector")
      }
      stopifnot(nrow(start_vals) > 0)
      if (optimizer != "DEoptim") {
        results <- lapply(1:nrow(start_vals), function(i){
          if (verbose > 0) {
            message("Optimization run: ", i)
          }
          one_row = unlist(start_vals[i,])
          estimate_classical(drift_dm_obj = drift_dm_obj, optimizer = optimizer,
                             start_vals = one_row, return_runs = FALSE,
                             lower = lower, upper = upper, verbose = verbose,
                             control = control, rounding = rounding)
        })
        cost_values = sapply(results, cost_value)
        prms = do.call(rbind, lapply(results, coef))
        j = which.min(cost_values)
        if (verbose > 0) {
          message(
            "Optimization run ", j, " yielded the smallest cost value"
          )
        }
        r = results[[j]]
        if (return_runs) {
          r = list(best_run = r, prms = prms, cost_values = cost_values)
        }
        return(r)
      } else {
        warning(
          "The 'start_vals' argument is ignored for the 'DEoptim' optimizer. ",
          "If you want to specify an initial population for DEoptim, please ",
          "use the 'control' argument (see ?DEoptim::DEoptim)."
        )
      }
    }
  }

  # get lower upper vectors (returns NULL if lower/upper are NULL)
  lower_upper <- get_parameters_smart(
    drift_dm_obj = drift_dm_obj,
    input_a = lower,
    input_b = upper,
    labels = FALSE
  )
  lower <- lower_upper$vec_a
  upper <- lower_upper$vec_b

  if (optimizer %in% c("nmkb", "L-BFGS-B", "DEoptim")) {
    if (is.null(lower) || is.null(upper)) {
      stop(
        "The optimizer '", optimizer, "' requires both 'lower' and 'upper' ",
        "bounds. Please specify these arguments and call the function again."
      )
    }
  }


  # continue checks/defaults
  if (is.null(verbose)) {
    verbose = 1
  }
  if (length(verbose) != 1 || verbose < 0) {
    stop("verbose must be numeric >= 0")
  }

  if (optimizer == "DEoptim") {
    if (!is.numeric(de_n_cores) || de_n_cores <= 0) {
      stop("de_n_cores must be a numeric > 0")
    }
  }

  if (!is.list(control)) {
    stop("control must be a list")
  }

  if (is.null(rounding)) {
    rounding = drift_dm_default_rounding()
  }
  if (!is.numeric(rounding) || length(rounding) != 1) {
    stop("'rounding' must be a single numeric value")
  }





  # objective function to minimize
  goal_wrapper <- function(new_model_prms, drift_dm_obj, verbose, rounding) {
    tryCatch(
      {
        # set (must re_evaluate!)
        drift_dm_obj$flex_prms_obj <- x2prms_vals(
          x = new_model_prms,
          flex_prms_obj = drift_dm_obj$flex_prms_obj
        )

        # re_evaluate
        drift_dm_obj <- re_evaluate_model(
          drift_dm_obj = drift_dm_obj,
          eval_model = TRUE
        )
        cost_value <- drift_dm_obj$cost_value

        # maybe print
        if (verbose == 2) {
          prms_string <- prms_to_str(drift_dm_obj, sep = " = ")
          message(
            "Parameters\n", prms_string, "\n==> gave a ",
            drift_dm_obj$cost_function, " of ",
            round(drift_dm_obj$cost_value, rounding)
          )
        }
        return(cost_value)
      },
      error = function(e) {
        return(.Machine$double.xmax)
      }
    )
  }

  if (verbose >= 1) {
    add = ""
    if (optimizer %in% c("nmkb", "nmk", "L-BFGS-B", "BFGS")) {
      add = "using the current parameter values of the model"
    }
    message("Starting optimizer '", optimizer, "' ", add)
  }


  # now call the requested optimization routine
  out = NULL
  if (optimizer == "nmkb") {
    out <- dfoptim::nmkb(
      par = coef(drift_dm_obj), fn = goal_wrapper, lower = lower,
      upper = upper, control = control, drift_dm_obj = drift_dm_obj,
      verbose = verbose, rounding = rounding
    )
    n_eval <- out$feval

  }

  if (optimizer == "nmk") {
    out <- dfoptim::nmk(
      par = coef(drift_dm_obj), fn = goal_wrapper, control = control,
      drift_dm_obj = drift_dm_obj, verbose = verbose, rounding = rounding
    )
    n_eval <- out$feval
  }

  if (optimizer == "L-BFGS-B") {
    out <- stats::optim(
      par = coef(drift_dm_obj), fn = goal_wrapper, method = optimizer,
      control = control, lower = lower, upper = upper,
      drift_dm_obj = drift_dm_obj, verbose = verbose, rounding = rounding
    )
    n_eval <- out$counts
  }

  if (optimizer == "BFGS") {
    out <- stats::optim(
      par = coef(drift_dm_obj), fn = goal_wrapper, method = optimizer,
      control = control, drift_dm_obj = drift_dm_obj, verbose = verbose,
      rounding = rounding
    )
    n_eval <- out$counts
  }
  if (!is.null(out)) {
    convergence <- out$convergence
    prms <- out$par
    message <- out$message
  }

  if (optimizer == "DEoptim") {
    # create clusters
    cl <- NULL
    if (de_n_cores > 1) {
      all_funs <- c(
        "goal_wrapper", "re_evaluate_model", "prms_to_str",
        "x2prms_vals"
      )
      cl <- parallel::makeCluster(de_n_cores)
      parallel::clusterExport(
        cl = cl,
        varlist = as.list(all_funs),
        envir = environment()
      )
    }

    # set control parameters
    if (is.null(control$trace)) {
      control$trace = FALSE
    }
    de_controls <- DEoptim::DEoptim.control(cluster = cl)
    de_controls <- utils::modifyList(de_controls, control)


    # now run the optimization (with tryCatch to avoid that clusters are not
    # closed)
    tryCatch(
      expr = {
        out <- DEoptim::DEoptim(
          fn = goal_wrapper, lower = lower, upper = upper,
          control = de_controls, drift_dm_obj = drift_dm_obj,
          verbose = verbose
        )
      },
      error = function(e) {
        if (!is.null(cl)) parallel::stopCluster(cl)
        stop(conditionMessage(e))
      }
    )
    if (!is.null(cl)) parallel::stopCluster(cl)
    n_eval <- out$optim$nfeval
    n_iter <- out$optim$iter
    convergence <- NA
    prms <- out$optim$bestmem
    message <- NULL
  }

  # continue with some exit messages and return value
  conv_flag = TRUE
  if (is.na(convergence)) conv_flag = NA
  if (!is.na(convergence) && convergence > 0) {
    warning(
      "The optimization routine did not converge successfully (convergence message: ",
      message, "). Treat the estimated parameters with caution."
    )
    conv_flag = FALSE
  }

  if (verbose >= 1) {
    # special treatment of n_eval as vector (occurs for BFGS)
    if (length(n_eval) > 1) {
      message(
        "Optimization routine exited with ", n_eval[1], " function ",
        "evaluations and ", n_eval[2], " gradient evaluations\n"
      )
    } else {
      message(
        "Optimization routine exited with ", n_eval, " function ",
        "evaluations"
      )
    }
  }

  # set parameters an evaluate fully
  coef(drift_dm_obj, eval_model = TRUE) <- as.numeric(prms)

  if (verbose >= 1) {
    prms_string <- prms_to_str(drift_dm_obj, sep = " = ")
    message(
      "Final Parameters\n", prms_string, "\n==> gave a ",
      drift_dm_obj$cost_function, " of ",
      round(drift_dm_obj$cost_value, rounding)

    )
  }

  # finally, attach some infos for downstream processing
  drift_dm_obj$estimate_info = list(
    conv_flag = conv_flag, optimizer = optimizer, message = message
  )


  return(drift_dm_obj)
}



# LOOP AROUND CLASSICAL OPTIMIZATION --------------------------------------

# formerly estimate_model_ids
estimate_classical_wrapper = function(drift_dm_obj, obs_data_ids,
                                      parallelization_strategy = NULL,
                                      progress = NULL,
                                      start_vals = NULL, ...) {

  # input checks and default values for par_strat/start_vals
  if (is.null(parallelization_strategy)) {
    parallelization_strategy = 1
  }
  if (!is.numeric(parallelization_strategy) |
      length(parallelization_strategy) != 1 |
      !(parallelization_strategy %in% c(1, 2))) {
    stop("'parallelization_strategy' must be 1 or 2")
  }

  if (is.null(progress)) {
    progress = 1
  }
  if (!is.numeric(progress) | length(progress) != 1 | progress < 0) {
    stop("'progress' must be >= 0")
  }


  # input checks for start_vals
  if (!is.null(start_vals)) {
    if (!is.data.frame(start_vals)) {
      stop("'start_vals' must be a data.frame")
    }
    if (!("ID" %in% colnames(start_vals))) {
      stop("No 'ID' column found in 'start_vals'")
    }
    ids_data <- unique(obs_data_ids$ID)
    ids_start <- start_vals$ID
    if (any(duplicated(ids_start))) {
      stop("The 'ID' column in 'start_vals' contains duplicates. Please ",
           "ensure that each 'ID' uniquely codes one row of 'start_vals'.")
    }
    if (!all(ids_data %in% ids_start)) {
      stop ("The 'IDs' listed in 'start_vals' don't match with the 'IDs' of ",
            "the observed data")
    }
  }


  # create a list of models to fit, handling observed data, and starting values
  tmp <- function(one_id) {
    obs_data(drift_dm_obj) <- obs_data_ids[obs_data_ids$ID == one_id,]

    if (!is.null(start_vals)) {
      start_vals <- start_vals[start_vals$ID == one_id, ]
      start_vals <- start_vals[names(start_vals) != "ID"]
    }

    return(list(drift_dm_obj = drift_dm_obj, start_vals = start_vals))
  }
  ids = unique(obs_data_ids$ID)
  list_of_models <- lapply(ids, tmp)
  names(list_of_models) = ids


  # decide over parallelization strategy (1 = parallelize individuals,
  # 2 = parallelize within inidviduals, currently only supported for DEoptim
  # and handled by the DEoptim package)
  dots = list(...)
  optimizer = dots$optimizer
  stopifnot(!is.null(optimizer))
  n_cores = dots$n_cores
  stopifnot(!is.null(n_cores))

  # if anything but DEoptim, parallelize individuals
  if (optimizer != "DEoptim") {
    parallelization_strategy = 1
  }

  # if n_cores is 1, then no parallelization is requested
  if (n_cores == 1) {
    parallelization_strategy = 2
  }


  # helper function for easier call
  run_estimation <- function(model_list, n_cores, dots, cl = NULL) {
    FUN <- function(one_model_start, n_cores, dots) {
      estimate_classical(
        drift_dm_obj = one_model_start$drift_dm_obj, optimizer = dots$optimizer,
        start_vals = one_model_start$start_vals,
        lower = dots$lower, upper = dots$upper, verbose = dots$verbose,
        de_n_cores = n_cores, control = dots$control,
        rounding = dots$rounding
      )
    }

    # Use parallel if cluster provided
    if (!is.null(cl)) {
      pbapply::pblapply(model_list, FUN, n_cores = n_cores, dots = dots, cl = cl)
    } else {
      pbapply::pblapply(model_list, FUN, n_cores = n_cores, dots = dots)
    }
  }

  # Cluster Setup
  cl <- NULL
  if (parallelization_strategy == 1) {
    cl <- parallel::makeCluster(n_cores)
    parallel::clusterExport(
      cl,
      varlist = c("estimate_classical"),
      envir = environment()
    )
    n_cores = 1 # to avoid that de_n_cores is larger than 1 in par_strat 1
  }

  # progress bar output
  if (progress == 0) {
    op <- pbapply::pboptions(type = "none")  # respect the user settings
    withr::defer(pbapply::pboptions(type = "timer"))
  }

  # if no verbose was specified, set to 0  (to avoid interference with
  # the progress bar)
  if (is.null(dots$verbose)) {
    dots$verbose = 0
  }

  # Run estimation (within tryCatch to ensure that clusters are closed)
  tryCatch({
    all_fits <- run_estimation(
      list_of_models, n_cores = n_cores, dots = dots, cl = cl
    )
  }, error = function(e){
    stop(conditionMessage(e))
  }, finally = {
    if (!is.null(cl)) parallel::stopCluster(cl)
  })

  # Check convergence: did some of the fit runs not converge?
  not_conv = sapply(all_fits, \(x) isFALSE(x$estimate_info$conv_flag))
  not_conv = not_conv[not_conv] # get those that did not converge

  messages = ""
  ids = character(0)
  if (length(not_conv) >= 1) {
    ids = names(not_conv)
    messages = unique(sapply(ids, \(x) all_fits[[x]]$estimate_info$message))
    messages = paste("-", messages)
    add = ""
    n_ids = length(ids)
    if (n_ids > drift_dm_n_id_trunc_warn()) {
      n_ids = drift_dm_n_id_trunc_warn()
      add = ", (and more)"
    }
    which_ids <- paste(paste(ids[1:n_ids], collapse = ", "), add)
    warning(
      "The optimization routine did not converge successfully for the ",
      "following IDs: ", which_ids, "\n", "Summary of messages\n",
      paste(messages, collapse = "\n")
    )
  }

  # wrap it up
  drift_dm_fit_info = list(
    drift_dm_obj = drift_dm_obj, obs_data_ids = obs_data_ids,
    optimizer = dots$optimizer,
    conv_info = list(messages = messages, ids = ids)
  )
  fits_ids <- list(drift_dm_fit_info = drift_dm_fit_info, all_fits = all_fits)
  class(fits_ids) <- "fits_ids_dm"
  return(fits_ids)
}


# HELPER FUNCTIONS --------------------------------------------------------

#' Set aggregated data to a model object
#'
#' Helper function that aggregates a data set across all participants
#' and attaches the resulting group-level summary statistics to a
#' `drift_dm_obj`. This is required when the user wants to fit aggregated data
#'  (e.g., via the RMSE cost function).
#'
#' @param drift_dm_obj a model object (of class `drift_dm`) to which the
#'  aggregated data will be attached.
#' @param obs_data a data.frame containing individual-level observations.
#'  Must include an `ID` column identifying participants.
#' @param ... optional arguments (currently supported are `n_bins` and
#' `probs` which are relevant when using the `rmse` cost function)
#'
#' @return The updated `drift_dm_obj` with aggregated `stats_agg` and
#' `obs_data` set to `NULL`.
#
#' @details This function is intended for internal use and is called by
#' [dRiftDM::estimate_dm()] when aggregated model fitting is requested.
#'
#' @keywords internal
set_agg_data <- function(drift_dm_obj, obs_data_ids, ...) {

  stopifnot(cost_function(drift_dm_obj) == "rmse")
  stopifnot("ID" %in% colnames(obs_data_ids))

  # now get the stats_agg by iterating over the IDs and setting each
  # individual's data to the model
  ids = unique(obs_data_ids$ID)
  all_aggs_infos = lapply(ids, \(one_id){
    sub_dat = obs_data_ids[obs_data_ids$ID == one_id,]
    obs_data(drift_dm_obj, ...) <- sub_dat
    return(list(drift_dm_obj$stats_agg, drift_dm_obj$stats_agg_info))
  })

  all_aggs <- lapply(all_aggs_infos, \(x) x[[1]])


  # Get condition names
  conds <- conds(drift_dm_obj)

  # get the summary stats (assumes this is always the same)
  summary_stats_names = names(all_aggs[[1]][[conds[1]]])

  # Average each stat across list entries
  avg_stats <- lapply(conds, function(one_cond) {

    res_list = list()
    for (one_sum_stat in summary_stats_names) {
      tmp <- lapply(all_aggs, \(x){
        x[[one_cond]][[one_sum_stat]]
      })
      avg <- colMeans(do.call(rbind, tmp))
      res_list[[one_sum_stat]] = avg
    }
    return(res_list)
  })

  # Name the result
  names(avg_stats) <- conds

  # add and return
  drift_dm_obj$obs_data <- NULL
  drift_dm_obj$stats_agg <- avg_stats

  # assumes this is always the same! but not hold for cost functions that
  # are not rmse!
  drift_dm_obj$stats_agg_info <- all_aggs_infos[[1]][[2]]


  return(drift_dm_obj)
}
