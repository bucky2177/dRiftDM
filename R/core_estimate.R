

#' Fit a DDM to observed data
#'
#' @description
#'
#' `estimate_dm()` is the main function to fit a drift diffusion model (DDM)
#' in `dRiftDM`. Several ways of fitting a model are supported: fitting a single
#' participant, fitting multiple participants separately or aggregated, and
#' fitting a (hierarchical) Bayesian model. The particular way is controlled
#' via the `approach` and `framework` arguments.
#'
#' Note that not all combinations of `approach` and `framework` are currently
#' supported. Also, the hierarchical estimation procedure is in an experimental
#' stage.
#'
#' @param drift_dm_obj a [dRiftDM::drift_dm] object containing the model to be
#'   fitted.
#' @param obs_data an optional [data.frame] (see also [dRiftDM::obs_data]).
#'   If no `ID` column is present, a single-individual setup is assumed.
#'   If an `ID` column is present, the model is fitted separately for each individual.
#' @param approach an optional character string, can be `"separately"`,
#'   `"aggregated"` or `"hierarchical"`.
#' @param framework a character string, one of `"classical"` or `"bayesian"`.
#'   This argument is adjusted automatically for incompatible combinations of
#'   `approach` and `framework` (e.g., `"hierarchical"` implies `"bayesian"`).
#'   Defaults to `"classical"`.
#' @param optimizer a character string. For the classical framework, one of
#'   `"nmkb"`, `"nmk"`, `"BFGS"`, `"L-BFGS-B"`, `"DEoptim"`. For the Bayesian
#'   framework, only `"DE-MCMC"` is currently supported. If `NULL` and the
#'   framework is `"classical"`, `"DEoptim"` or `"nmk"` is used, depending
#'   on whether `lower/upper` are provided or not. If `NULL` and
#'   the framework is `"bayesian"`, then `"DE-MCMC` is used. Note that
#'   `"BFGS"` and `"L-BFGS-B"` are often unstable.
#' @param control a list of control parameters passed to the optimizer
#'   (see [dfoptim::nmk], [dfoptim::nmkb], [DEoptim::DEoptim], [stats::optim])
#' @param n_cores an integer > 0, indicating the number of CPU cores/threads to
#'   use (at the moment, this doesn't have an effect when fitting a single
#'   individual within the Bayesian framework).
#' @param parallelization_strategy an integer, controlling how parallelization
#'   is performed when fitting multiple individuals with the classical approach.
#'   If `1`, parallelization is across individuals. If `2`, parallelization is
#'   within individuals (currently only supported for `"DEoptim"`). Defaults to
#'   `2` if `optimizer = "DEoptim"`, otherwise to `1`.
#' @param lower,upper numeric vectors or lists, specifying the lower and upper
#'  bounds on each parameter to be optimized (see Details).
#' @param start_vals optional starting values for classical single-subject fits
#'   and when using an optimizer that requires a starting value. Can be
#'   a numeric vector of model parameters when fitting a single individual, or
#'   a `data.frame` with columns for each model parameter. In the latter case,
#'   enables multi-start (one row per start). For `'approach = "separately"'`, a
#'   `data.frame` with an `ID` column is required.
#' @param means,sds,shapes,rates optional numeric vectors for prior
#'   specification (when using the Bayesian framework, see Details).
#' @param n_chains an integer, providing the number of MCMC chains (Bayesian
#'   framework).
#' @param burn_in an integer, number of burn-in iterations (Bayesian
#'   framework).
#' @param samples an integer, number of post-burn-in samples per chain (
#'   Bayesian framework).
#' @param prob_migration a numeric in `[0,1]`, controlling the migration
#'   probability of the `DE-MCMC` algorithm (Bayesian framework).
#' @param prob_re_eval a numeric in `[0,1]`, probability to re-evaluate the
#'   model at current group-level parameters during sampling (Bayesian
#'   framework; only relevant for the hierarchical case).
#' @param messaging a logical, if `TRUE` progress/info messages are printed
#' @param seed an optional integer to set the RNG seed for reproducibility.
#' @param ... additional arguments forwarded to lower-level routines. Options
#'    are: `progress/verbose` (integers, for controlling progress bars and
#'    verbosity of estimation infos), `round_digits` (for controlling the number
#'    of digits for rounding when printing individual model evaluations;
#'    if `verbose = 2`), `return_runs`
#'    (when fitting a single individual and starting the estimation
#'    routine with multiple starting points; if `TRUE`, then a list of all
#'    routines is returned), `probs/n_bins` (the quantile levels and the number
#'    of CAF bins when fitting aggregated data using the RMSE cost function).
#'
#' @return
#'   * If fitting a single individual: either a `drift_dm` object with
#'     fitted parameters (for the classical optimization framework) or
#'     an object of type `mcmc_dm` (for the Bayesian framework)
#'   * If fitting multiple individuals separately: a `fits_ids_dm` object
#'     containing all the individual model fits and some estimation info.
#'   * If fitting aggregated data: a `fits_agg_dm` object containing the model
#'     itself and the raw data.
#'   * If fitting multiple individuals hierarchically: an object of type
#'     `mcmc_dm`.
#'
#' @details
#'
#' For aggregated fits, aggregated statistics are set to the model and the cost
#' function is switched to `"rmse"`. If incompatible settings are requested,
#' the function switches to a compatible configuration and informs the user
#' with messages (these messages can be suppressed via the `messaging` argument).
#' Lower and upper parameter bounds are required for `"nmkb"`, `"L-BFGS-B"`,
#' and `"DEoptim"` optimizer.
#'
#' ## Specifying `lower/upper` for Classical optimization
#'
#' the function `estimate_model_dm()` provides a flexible way of specifying the
#' optimization space; this is identical to specifying the parameter simulation
#' space in [dRiftDM::simulate_data.drift_dm()].
#'
#' Users have three options to specify the search space (see also the examples
#' below):
#'
#' * Plain numeric vectors (not very much recommended). In this case,
#' `lower/upper` must be sorted in accordance with the parameters in the
#' underlying [dRiftDM::flex_prms] object of `drift_dm_obj` that vary for at
#' least one condition (call `print(drift_dm_obj)` and have a look at the
#' columns of the `Parameter Settings` output; for each column that has a
#' number > 0, specify an entry in `lower/upper`).
#'
#' * Named numeric vectors. In this case `lower/upper` have to provide labels
#' in accordance with the parameters that are considered "free" at least once
#' across conditions (call `coef(drift_dm_obj)` and provide one named entry for
#' each parameter; dRiftDM will try to recycle parameter values across
#' conditions).
#'
#' * The most precise way is when `lower/upper` are lists. In this case, the
#' list requires an entry called "default_values" which specifies the named or
#' plain numeric vectors as above. If the list only contains this entry, then
#' the behavior is as if `lower/upper` were already numeric vectors. However,
#' the `lower/upper` lists can also provide entries labeled as specific
#' conditions, which contain named (!) numeric vectors with parameter labels.
#' This will modify the value for the upper/lower parameter space with respect
#' to the specified parameters in the respective condition.
#'
#'
#' ## Specifying Priors for Bayesian Estimation
#'
#' **(Default) Prior settings in the non-hierarchical case:**
#'
#' Let \eqn{\theta^{(j)}} indicate parameter \eqn{j} of a model (e.g., the
#' drift rate).
#' The prior on \eqn{\theta^{(j)}} is a truncated normal distribution:
#'  \deqn{
#'  \theta^{(j)} \sim NT(\mu^{(j)}, \sigma^{(j)}, l^{(j)}, u^{(j)})
#'  }
#' With \eqn{\mu^{(j)}} and \eqn{\sigma^{(j)}} representing the mean and standard
#' deviation of parameter \eqn{j}. \eqn{l^{(j)}} and \eqn{u^{(j)}} represent the
#' lower and upper boundary. \eqn{\mu^{(j)}} is taken from the `mean`
#' argument or the currently set model parameters (i.e., from
#' `coef(drift_dm_obj)`) when calling the function. \eqn{\sigma^{(j)}} is, per
#' default, equal to \eqn{\mu^{(j)}}. This can be changed by passing
#' the `sd` argument. The lower and upper boundaries of the truncated normal
#' are `-Inf` and `Inf` per default. This can be altered by passing the
#' arguments `lower` and  `upper`.
#'
#' **(Default) Prior settings in the hierarchical case:**
#'
#' Let \eqn{\theta_i^{(j)}} indicate parameter \eqn{j} for participant \eqn{i}
#' (e.g., the  drift rate estimated for individual \eqn{i}). The prior on
#' \eqn{\theta_i^{(j)}} is a truncated normal distribution:
#'  \deqn{
#'  \theta_i^{(j)} \sim NT(\mu^{(j)}, \sigma^{(j)}, l^{(j)}, u^{(j)})
#'  }
#' With \eqn{\mu^{(j)}} and \eqn{\sigma^{(j)}} representing the mean and
#' standard deviation of parameter \eqn{j} at the group level. \eqn{l^{(j)}} and
#' \eqn{u^{(j)}} represent the lower and upper boundary. The lower and upper
#' boundaries of the truncated normal are `-Inf` and `Inf` per default.
#' This can be altered by passing the arguments `lower` and  `upper`.
#'
#' For a group-level mean parameter, \eqn{\mu^{(j)}}, the prior is also a
#' truncated normal distributions:
#'  \deqn{
#'  \mu^{(j)} \sim NT(M^{(j)}, SD^{(j)}, l^{(j)}, u^{(j)})
#'  }
#' With \eqn{M^{(j)}} specified by the `mean` argument or the currently
#' set model parameters. \eqn{SD^{(j)}} is, per default, equal to \eqn{M^{(j)}}.
#' This can be changed by passing the `sd` argument.
#'
#' For a group-level standard deviation parameter, \eqn{\sigma^{(j)}}, the prior
#' is a gamma distribution:
#'  \deqn{
#'  \sigma^{(j)} \sim \Gamma(shape^{(j)},rate^{(j)})
#'  }
#' With \eqn{shape^{(j)}} and \eqn{rate^{(j)}} being `1` by default. This
#' can be changed by passing the arguments `shape` and `rate`.
#'
#' **Specifying Prior Settings/Arguments**
#'
#' Argument specification for `mean`, `sd`, `lower`, `upper`, `shape` and
#' `rate` is conceptually identical to specifying `lower/upper` for the
#' classical optimization approach (see the subsection above and the examples
#' below).
#'
#' @note
#' `estimate_dm` dispatches to underlying estimation routines that are not
#' exported:
#' - Classical optimization of one individual via
#'   [dRiftDM::estimate_classical()]
#' - Classical optimization of multiple individuals via
#'   [dRiftDM::estimate_classical_wrapper()]
#' - Bayesian estimation via [dRiftDM::estimate_bayesian()].
#' - Aggregated fitting is handled within `estimate_dm()` in combination with
#'   [dRiftDM::estimate_classical()]
#'
#' @seealso [dRiftDM::estimate_classical()], [dRiftDM::estimate_bayesian()],
#'   [dRiftDM::estimate_classical_wrapper()], [dRiftDM::get_parameters_smart()]
#'
#' @examples
#' ##########
#' # Note: The following examples were trimmed for speed to ensure they run
#' # within seconds. They do not provide realistic settings.
#' ##########
#' tic()
#' # get a model for the examples (DMC with just two free parameters)
#' model <- dmc_dm(
#'   t_max = 1.5, dx = .01, dt = .01, # very coarse settings for speed
#'   instr = '
#'    b <!>
#'    non_dec <!>
#'    sd_non_dec <!>
#'    tau <!>
#'    alpha <!>
#'    '
#' )
#' # simulate two data sets under the model (for demonstration purpose)
#' lower <- c(muc = 1, A = 0.05)
#' upper <- c(muc = 7, A = 0.15)
#' synth_data_prms <- simulate_data(
#'   model, n = 200, k = 2, lower = lower, upper = upper, seed = 1
#' )
#' synth_data <- synth_data_prms$synth_data
#'
#'
#' ####
#' # Fit a single individual (using unbounded Nelder-Mead)
#' one_subj <- synth_data[synth_data$ID == 1,] # data of one individual
#' obs_data(model) <- one_subj
#' fit <- estimate_dm(
#'   drift_dm_obj = model,
#'   optimizer = "nmk"
#' )
#' print(fit)
#'
#'
#' ####
#' # Fit a single individual (using bounded Nelder-Mead and custom starting
#' # values)
#' fit <- estimate_dm(
#'   drift_dm_obj = model,
#'   optimizer = "nmkb",
#'   lower = lower, upper = upper,
#'   start_vals = c(muc = 4, A = 0.06)
#' )
#' print(fit)
#'
#' ####
#' # Fit multiple individuals (separately; using bounded Nelder-Mead)
#' fit <- estimate_dm(
#'   drift_dm_obj = model,
#'   obs_data = synth_data, # contains data for two individuals
#'   optimizer = "nmkb",
#'   lower = lower, upper = upper
#' )
#' print(fit)
#' coef(fit)
#'
#'
#' ###
#' # Fit to aggregated data (using unbounded Nelder-Mead)
#' fit <- estimate_dm(
#'   drift_dm_obj = model,
#'   obs_data = synth_data, # contains data for two individuals
#'   optimizer = "nmk",
#'   approach = "agg"
#' )
#' print(fit)
#' coef(fit)
#'
#'
#' ###
#' # Fit a single individual (using DE-MCMC; Bayesian; custom priors)
#' fit <- estimate_dm(
#'   drift_dm_obj = model,
#'   optimizer = "DE-MCMC",
#'   burn_in = 2, # this is usually way higher
#'   samples = 2, # this too
#'   n_chains = 10, # this too
#'   mean = c(muc = 3, A = 0.9),
#'   sd = c(muc = 2, A = 0.8),
#' )
#' print(fit)
#' coef(fit)
#'
#'
#' ###
#' # Fit multiple individuals (using DE-MCMC; hierarchical Bayesian)
#' fit <- estimate_dm(
#'   drift_dm_obj = model,
#'   obs_data = synth_data, # contains data for two individuals
#'   optimizer = "DE-MCMC",
#'   burn_in = 2, # this is usually way higher
#'   samples = 2, # this too
#'   n_chains = 10 # this too
#' )
#' print(fit)
#' coef(fit)
#' toc()
#'
#' @export
estimate_dm <- function(drift_dm_obj, obs_data = NULL,
                        approach = NULL, framework = NULL, optimizer = NULL,
                        control = list(), n_cores = 1,
                        parallelization_strategy = NULL, lower = NULL,
                        upper = NULL, start_vals = NULL, means = NULL,
                        sds = NULL, shapes = NULL, rates = NULL,
                        n_chains = 40, burn_in = 500, samples = 1000,
                        prob_migration = 0.1, prob_re_eval = 1,
                        messaging = TRUE,
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
    approach, choices = c("NULL", "separately", "aggregated", "hierarchical")
  )

  # check the framework and set default values
  if (is.null(framework) & optimizer != "DE-MCMC") {
    framework = "classical"
  } else if (optimizer == "DE-MCMC") {
    framework = "bayesian"
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
      if (messaging) {
        message("The provided optimizer ('", optimizer, "') is not compatible ",
                "with the specified approach ('", approach, "') and framework ",
                "('", framework, "'). Using the following optimizer instead: '",
                requ_optimizer, "'.")
      }
    optimizer = requ_optimizer
    }
  }

  # check if the optimizer is implemented
  optimizer <- match.arg(
    optimizer, choices = c("nmk", "nmkb", "BFGS", "L-BFGS-B", "DEoptim",
                           "DE-MCMC")
  )

  # check n_cores
  if (!is.numeric(n_cores) || length(n_cores) != 1 || n_cores <= 0) {
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
      if (messaging) message("Changing the 'cost_function' to 'rmse'")
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
      start_vals = start_vals, return_runs = dots$return_runs,
      lower = lower, upper = upper, verbose = dots$verbose, de_n_cores = n_cores,
      control = control, round_digits = dots$round_digits
    )

    # wrap up the return value and create a custom class
    fit_obj = list(drift_dm_obj = drift_dm_obj, obs_data_ids = obs_data)
    class(fit_obj) = "fits_agg_dm"
    return(fit_obj)
  }


  # interim: handle the NULL (default) setting (can lead to fitting just
  # one participant or multiple participants separately)
  if (approach == "NULL") {
    if (which_data == "obs_data") {
      if (!("ID" %in% names(obs_data))) {
        approach <- "one_subj"
        obs_data(drift_dm_obj) <- obs_data
        if (messaging) {
          message(
            "No 'ID' column found in the supplied 'obs_data'. Attaching the ",
            "data to the model assuming a single-participant setup."
          )
        }
      } else {
        if (framework == "classical") {
          approach <- "separately"
        } else {
          approach <- "hierarchical"
        }
      }
    } else { # which_data == "model"
      approach <- "one_subj"
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
        drift_dm_obj = drift_dm_obj, optimizer = optimizer,
        start_vals = start_vals, return_runs = dots$return_runs,
        lower = lower, upper = upper, verbose = dots$verbose,
        de_n_cores = n_cores, control = control, round_digits = dots$round_digits
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
    if (!is.null(dots$return_runs)) {
      if (messaging) {
        message(
          "The argument `return_runs` is ignored for ",
          "`'approach = 'separately'`."
        )
      }
    }
    all_fits = estimate_classical_wrapper(
      drift_dm_obj = drift_dm_obj, obs_data_ids = obs_data,
      parallelization_strategy = parallelization_strategy,
      progress = dots$progress, start_vals = start_vals,
      optimizer = optimizer, lower = lower, upper = upper,
      verbose = dots$verbose, n_cores = n_cores, control = control,
      round_digits = dots$round_digits
    )
    return(all_fits)
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
#' @param round_digits an integer. Number of digits to round cost values in printed
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
                               control = list(), round_digits = NULL) {
  # input checks
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }


  optimizer <- match.arg(
    optimizer,
    choices = c("nmkb", "nmk", "BFGS", "L-BFGS-B", "DEoptim")
  )

  # continue checks/defaults
  if (is.null(verbose)) {
    verbose = 1
  }
  if (length(verbose) != 1 || verbose < 0) {
    stop("verbose must be numeric >= 0")
  }

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
      if (!is.numeric(start_vals)) {
        stop("'start_vals' must be a data.frame or a numeric vector")
      }
      prm <- get_parameters_smart(
        drift_dm_obj = drift_dm_obj,
        input_a = start_vals
      )$vec_a
      coef(drift_dm_obj) <- prm
    } else {
      # otherwise call the function recursively (but not for DEoptim)
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
                             control = control, round_digits = round_digits)
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


  if (optimizer == "DEoptim") {
    if (!is.numeric(de_n_cores) || de_n_cores <= 0) {
      stop("de_n_cores must be a numeric > 0")
    }
  }

  if (!is.list(control)) {
    stop("control must be a list")
  }

  if (is.null(round_digits)) {
    round_digits = drift_dm_default_rounding()
  }
  if (!is.numeric(round_digits) || length(round_digits) != 1) {
    stop("'round_digits' must be a single numeric value")
  }


  # objective function to minimize
  goal_wrapper <- function(new_model_prms, drift_dm_obj, verbose, round_digits) {
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
            round(drift_dm_obj$cost_value, round_digits)
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
      verbose = verbose, round_digits = round_digits
    )
    n_eval <- out$feval

  }

  if (optimizer == "nmk") {
    out <- dfoptim::nmk(
      par = coef(drift_dm_obj), fn = goal_wrapper, control = control,
      drift_dm_obj = drift_dm_obj, verbose = verbose, round_digits = round_digits
    )
    n_eval <- out$feval
  }

  if (optimizer == "L-BFGS-B") {
    out <- stats::optim(
      par = coef(drift_dm_obj), fn = goal_wrapper, method = optimizer,
      control = control, lower = lower, upper = upper,
      drift_dm_obj = drift_dm_obj, verbose = verbose, round_digits = round_digits
    )
    n_eval <- out$counts
  }

  if (optimizer == "BFGS") {
    out <- stats::optim(
      par = coef(drift_dm_obj), fn = goal_wrapper, method = optimizer,
      control = control, drift_dm_obj = drift_dm_obj, verbose = verbose,
      round_digits = round_digits
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
      }, finally = {
        if (!is.null(cl)) parallel::stopCluster(cl)
      }
    )
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
        "Optimization routine exited after ", n_eval[1], " function ",
        "evaluations and ", n_eval[2], " gradient evaluations\n"
      )
    } else {
      message(
        "Optimization routine exited after ", n_eval, " function ",
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
      round(drift_dm_obj$cost_value, round_digits)

    )
  }

  # finally, attach some infos for downstream processing
  drift_dm_obj$estimate_info = list(
    conv_flag = conv_flag, optimizer = optimizer, message = message
  )


  return(drift_dm_obj)
}



# LOOP AROUND CLASSICAL OPTIMIZATION --------------------------------------

#' Internal wrapper for classical estimation of individuals
#'
#' This function wraps [dRiftDM::estimate_classical()] to handle fitting
#' multiple individuals in a consistent way. It prepares the data, distributes
#' the estimation across individuals, and manages parallelization strategies and
#' progress reporting. Unlike the deprecated `estimate_model_ids()`, this
#' function no longer saves results to disk --- instead, it directly returns an
#' object of class `fits_ids_dm`.
#'
#' @param drift_dm_obj a [dRiftDM::drift_dm] object that will be estimated for
#'   each individual in `obs_data_ids`.
#' @param obs_data_ids a [data.frame] of observed data including an `ID`
#'   column that uniquely identifies each individual.
#' @param parallelization_strategy an integer, either `1` or `2`. Strategy `1`
#'   parallelizes across individuals, while strategy `2` parallelizes within
#'   individuals (only supported for `"DEoptim"`). Default is `1`
#' @param progress an integer, controlling progress output. `0` = no progress,
#'   `1` = minimal output, `2` = progress bar. Default is `1`
#' @param start_vals an optional `data.frame` with starting values for each
#'   individual. Must contain an `ID` column matching the IDs in
#'   `obs_data_ids`, and one column per parameter.
#' @param ... further arguments passed to [estimate_classical()], including
#'   `optimizer`, `lower`, `upper`, `verbose`, `control`, `round_digits`, and
#'   `n_cores`. Note that the argument `return_runs` is not supported.
#'
#' @return an object of class `fits_ids_dm`, which is a list with two
#'   components:
#'   * `drift_dm_fit_info` --- a list containing the model object,
#'     observed data, optimizer information, and convergence messages
#'   * `all_fits` --- a list of individual estimation results
#'
#' @details
#' Convergence issues are checked automatically. If one or more individuals
#' fail to converge, a warning is issued with the corresponding IDs and
#' messages returned by the optimizer.
#'
#' @keywords internal
#' @seealso [dRiftDM::estimate_classical()], [dRiftDM::estimate_dm()]
estimate_classical_wrapper = function(drift_dm_obj, obs_data_ids,
                                      parallelization_strategy = NULL,
                                      progress = NULL,
                                      start_vals = NULL, ...) {

  dots = list(...)

  # input checks and default values for par_strat/start_vals
  optimizer = dots$optimizer
  stopifnot(!is.null(optimizer))
  n_cores = dots$n_cores
  stopifnot(!is.null(n_cores))

  # decide over parallelization strategy (1 = parallelize individuals,
  # 2 = parallelize within inidviduals, currently only supported for DEoptim
  # and handled by the DEoptim package
  if (optimizer == "DEoptim") {
    if (is.null(parallelization_strategy)) {
      parallelization_strategy <- 2
    }
  } else {
    parallelization_strategy <- 1
  }
  if (n_cores == 1)  parallelization_strategy <- 1
  if (!is.numeric(parallelization_strategy) |
      !(parallelization_strategy %in% c(1, 2))) {
    stop("'parallelization_strategy' must be 1 or 2")
  }

  # continue checks
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
    ids_start <- unique(start_vals$ID)
    if (!setequal(ids_data, ids_start)) {
      stop ("The 'IDs' listed in 'start_vals' don't match with the 'IDs' of ",
            "the observed data")
    }
  }


  # create a list of models to fit, handling observed data, and starting values
  tmp <- function(one_id) {
    obs_data(drift_dm_obj) <- obs_data_ids[obs_data_ids$ID == one_id,]

    sv <- NULL
    if (!is.null(start_vals)) {
      sv <- start_vals[start_vals$ID == one_id, ]
      sv <- sv[names(sv) != "ID"]
    }

    return(list(drift_dm_obj = drift_dm_obj, start_vals = sv))
  }
  ids = unique(obs_data_ids$ID)
  list_of_models <- lapply(ids, tmp)
  names(list_of_models) = ids


  # helper function for easier call
  run_estimation <- function(model_list, n_cores, dots, cl = NULL) {
    FUN <- function(one_model_start, n_cores, dots) {
      estimate_classical(
        drift_dm_obj = one_model_start$drift_dm_obj, optimizer = dots$optimizer,
        start_vals = one_model_start$start_vals,
        lower = dots$lower, upper = dots$upper, verbose = dots$verbose,
        de_n_cores = n_cores, control = dots$control,
        round_digits = dots$round_digits
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
  if (parallelization_strategy == 1 & n_cores > 1) {
    cl <- parallel::makeCluster(n_cores)
    parallel::clusterExport(
      cl,
      varlist = c("estimate_classical"),
      envir = environment()
    )
    n_cores = 1 # to avoid that de_n_cores is larger than 1 for parstrat 1
  }

  # progress bar output
  if (progress == 0) {
    op <- pbapply::pboptions(type = "none")  # respect the user settings
    withr::defer(pbapply::pboptions(type = op$type))
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

  messages = character(0)
  ids = character(0)
  if (length(not_conv) >= 1) {
    ids = names(not_conv)
    messages = sapply(ids, \(x) all_fits[[x]]$estimate_info$message)
    messages_formatted = paste("-", unique(messages))
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
      paste(messages_formatted, collapse = "\n")
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
