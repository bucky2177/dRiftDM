#' Fit a DDM to Observed Data
#'
#' @description
#'
#' `estimate_dm()` is the main function to fit a drift diffusion model (DDM)
#' in `dRiftDM`. Several ways of fitting a model are supported: fitting a single
#' participant, fitting multiple participants separately or aggregated, and
#' fitting a (hierarchical) Bayesian model. The particular way is controlled
#' via the `approach` argument.
#'
#' @param drift_dm_obj a [dRiftDM::drift_dm] object containing the model to be
#'   fitted.
#' @param obs_data an optional [data.frame] (see also [dRiftDM::obs_data]).
#'   If no `ID` column is present, a single-individual setup is assumed.
#'   If an `ID` column is present, the model is fitted separately for each
#'   individual.
#' @param approach an optional character string, specifying the approach to
#'   fitting the model. Options are `"sep_c"`, `"agg_c"`, `"sep_b"`, `"hier_b"`
#'   (see the Details).
#' @param optimizer a character string. For classical optimization, one of
#'   `"nmkb"`, `"Nelder-Mead"`, `"BFGS"`, `"L-BFGS-B"`, `"DEoptim"`. For the
#'   Bayesian framework, only `"DE-MCMC"` is currently supported. If `NULL` and
#'   if a classical optimization approach is used, defaults to `"DEoptim"` or
#'   `"Nelder-Mead"`, depending on whether `lower/upper` are provided or not. If
#'   `NULL` and if a Bayesian framework is used, defaults to `"DE-MCMC`.
#'   Note that `"BFGS"` and `"L-BFGS-B"` are often unstable.
#' @param control a list of control parameters passed to the optimizer
#'   (for Nelder-Mead, BFGS, and L-BFGS-B, see [stats::optim]; for nmkb, see
#'   [dfoptim::nmkb]; for DEoptim, see [DEoptim::DEoptim]).
#'   Per default, we set the `trace` control argument for [DEoptim::DEoptim] to
#'   `FALSE`. Also, we set the `parscale` control argument for "Nelder-Mead" via
#'   [stats::optim] to `pmax(x0, 1e-6)`.
#' @param n_cores an integer > 0, indicating the number of CPU cores/threads to
#'   use (at the moment, this doesn't have an effect when fitting a single
#'   individual within the Bayesian framework).
#' @param parallelization_strategy an integer, controlling how parallelization
#'   is performed when fitting multiple individuals with the classical approach.
#'   If `1`, parallelization is across individuals. If `2`, parallelization is
#'   within individuals (currently only supported for `"DEoptim"`). Defaults to
#'   `1`.
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
#'    of CAF bins when fitting aggregated data using the RMSE cost function),
#'    `use_ez/n_lhs` (logical and integer; the first controls if EZ-Diffusion
#'    Parameter Estimates shall be used for determining starting points; the
#'    latter controls the number of parameters to sample per dimension for the
#'    latin hypercube sampling when searching for starting values)
#' @param round_digits integer, specifying the number of decimal places for
#'   rounding in the printed summary. Default is 3.
#' @param x an object of type `fits_agg_dm`, `fits_ids_dm`, or `mcmc_dm`
#'
#' @return
#'   * If fitting a single individual: either a `drift_dm` object with
#'     fitted parameters and additional fit information (for the classical
#'     optimization framework) or an object of type `mcmc_dm` (for the Bayesian
#'     framework)
#'   * If fitting multiple individuals separately: a `fits_ids_dm` object
#'     or a list of `mcmc_dm` objects, containing all the individual model fits.
#'   * If fitting aggregated data: a `fits_agg_dm` object containing the model
#'     itself and the raw data.
#'   * If fitting multiple individuals hierarchically: an object of type
#'     `mcmc_dm`.
#'
#' @details
#'
#'
#' ## Fitting Approaches
#'
#' The function supports different "approaches" to fitting data.
#'
#' - `"sep_c"`: This means that data is always considered `sep`arately for
#'   each participant (if there are multiple participants) and that a
#'   `c`lassical approach to parameter optimization is used. This means that
#'   a standard [dRiftDM::cost_function] is minimized (e.g., the negative
#'   log-likelihood). If users provide only a single participant or a data set
#'   without an `ID` column, then the model is fitted just once to that data
#'   set.
#'
#' - `"agg_c"`: This fits the model to aggregated data. For each individual in
#'   a data set, summary statistics (e.g., quantiles, accuracies) are
#'   calculated, and the model is fitted once to the average of these summary
#'   statistics.
#'
#' - `"sep_b"`: Similar to `sep_b"`, although a Bayesian approach is used to
#'   sample from the posterior distribution.
#'
#' - `"hier_b"`: A hierarchical approach to parameter estimation. In this case
#'   all participants are considered simultaneously and samples are drawn both
#'   at the individual-level and group-level.
#'
#' The optimizers  `"nmkb"`, `"L-BFGS-B"`, and `"DEoptim"` (for classical
#' parameter optimization) require the specification of the `lower/upper`
#' arguments.
#'
#' ## Fitting to Aggregated Data
#'
#' For aggregated fits, aggregated statistics are set to the model and the cost
#' function is switched to `"rmse"`. If incompatible settings are requested,
#' the function switches to a compatible configuration and informs the user
#' with messages (these messages can be suppressed via the `messaging` argument).
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
#' arguments `lower` and  `upper` (see the examples below).
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
#' When fitting a model with `optimizer = "DEoptim"`, the corresponding
#' minimization routine always runs for 200 iterations by default, irrespective
#' of whether a minimum has already been reached (see
#' [DEoptim::DEoptim.control]). Therefore, with default optimization settings,
#' `estimate_dm()` returns the convergence flag `NA` for
#' `optimizer = "DEoptim"`, because the termination of the routine does not
#' necessarily indicate convergence. However, this is typically not an issue, as
#' 200 iterations are generally sufficient for the algorithm to find the global
#' minimum. If users explicitly define convergence criteria via the `control`
#' argument of `estimate_dm()` (which is passed on to
#' [DEoptim::DEoptim.control]), valid convergence messages and flags are
#' returned.
#'
#'
#' @seealso [dRiftDM::estimate_classical()], [dRiftDM::estimate_bayesian()],
#'   [dRiftDM::estimate_classical_wrapper()], [dRiftDM::get_parameters_smart()]
#'
#' @examples
#' ##########
#' # Note: The following examples were trimmed for speed to ensure they run
#' # within seconds. They do not always provide realistic scenarios.
#' ##########
#'
#' ####
#' # Setup
#'
#' # get a model for the examples (DMC with just two free parameters)
#' model <- dmc_dm(
#'   instr = "
#'    b <!>
#'    non_dec <!>
#'    sd_non_dec <!>
#'    tau <!>
#'    alpha <!>
#'    "
#' )
#'
#' # get some data (the first two participants in the flanker data set of
#' # Ulrich et al.)
#' data <- ulrich_flanker_data[ulrich_flanker_data$ID %in% 1:2, ]
#'
#'
#' ####
#' # Fit a single individual (using unbounded Nelder-Mead)
#' fit <- estimate_dm(
#'   drift_dm_obj = model,
#'   obs_data = data[data$ID == 1, ],
#'   optimizer = "Nelder-Mead"
#' )
#' print(fit)
#'
#'
#' ####
#' # Fit a single individual (using bounded Nelder-Mead and custom starting
#' # values)
#' l_u <- get_lower_upper(model)
#' fit <- estimate_dm(
#'   drift_dm_obj = model,
#'   obs_data = data[data$ID == 1, ],
#'   optimizer = "nmkb",
#'   lower = l_u$lower, upper = l_u$upper,
#'   start_vals = c(muc = 4, A = 0.06)
#' )
#' print(fit)
#'
#'
#' ####
#' # Fit a single individual (using DEoptim)
#' l_u <- get_lower_upper(model)
#' set.seed(2)
#' fit <- estimate_dm(
#'   drift_dm_obj = model,
#'   obs_data = data[data$ID == 1, ],
#'   optimizer = "DEoptim",
#'   lower = l_u$lower, upper = l_u$upper,
#'   control = list(itermax = 5) # way higher in practice! (default: 200)
#' )
#' print(fit)
#'
#'
#' ####
#' # Fit multiple individuals (separately; using bounded Nelder-Mead)
#' l_u <- get_lower_upper(model)
#' fit <- estimate_dm(
#'   drift_dm_obj = model,
#'   obs_data = data, # contains the data for two individuals
#'   optimizer = "nmkb",
#'   lower = l_u$lower, upper = l_u$upper,
#' )
#' print(fit)
#' coef(fit)
#'
#'
#' ###
#' # Fit to aggregated data (using unbounded Nelder-Mead)
#' fit <- estimate_dm(
#'   drift_dm_obj = model,
#'   obs_data = data, # contains data for two individuals
#'   optimizer = "Nelder-Mead",
#'   approach = "agg_c"
#' )
#' print(fit)
#' coef(fit)
#'
#'
#' ###
#' # EXPERIMENTAL
#' # Fit a single individual (using DE-MCMC; Bayesian; custom priors)
#' fit <- estimate_dm(
#'   drift_dm_obj = model,
#'   obs_data = data[data$ID == 1, ],
#'   approach = "sep_b",
#'   burn_in = 1, # higher in practice (e.g., 500)
#'   samples = 1, # higher in practice (e.g., 1000)
#'   n_chains = 5, # higher in practice (e.g., 40)
#'   mean = c(muc = 3, A = 0.9),
#'   sd = c(muc = 2, A = 0.8),
#' )
#' print(fit)
#' coef(fit)
#'
#'
#' ###
#' # EXPERIMENTAL
#' # Fit multiple individuals (using DE-MCMC; hierarchical Bayesian)
#' fit <- estimate_dm(
#'   drift_dm_obj = model,
#'   approach = "hier_b",
#'   obs_data = data, # contains data for two individuals
#'   burn_in = 1, # higher in practice (e.g., 500)
#'   samples = 1, # higher in practice (e.g., 1000)
#'   n_chains = 5, # higher in practice (e.g., 40)
#'   n_cores = 1, # higher in practice (depending on your machine and data set)
#' )
#' print(fit)
#' coef(fit)
#'
#' @export
estimate_dm <- function(
  drift_dm_obj,
  obs_data = NULL,
  approach = NULL,
  optimizer = NULL,
  control = list(),
  n_cores = 1,
  parallelization_strategy = NULL,
  lower = NULL,
  upper = NULL,
  start_vals = NULL,
  means = NULL,
  sds = NULL,
  shapes = NULL,
  rates = NULL,
  n_chains = 40,
  burn_in = 500,
  samples = 1000,
  prob_migration = 0.1,
  prob_re_eval = 1,
  messaging = TRUE,
  seed = NULL,
  ...
) {
  dots <- list(...)

  # user input checks ####
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }

  # check if it can be evaluated
  drift_dm_obj <- re_evaluate_model(drift_dm_obj)

  # use data from the model or from the obs_data object
  if (is.null(obs_data)) {
    if (is.null(drift_dm_obj$obs_data) & is.null(drift_dm_obj$stats_agg)) {
      stop(
        "No observed data was supplied via the 'obs_data' argument, and no ",
        "observed data is attached to the model. Parameter estimation is ",
        "therefore not possible."
      )
    }

    if (messaging) {
      message("Using the data attached to the model.")
    }
    which_data <- "model"
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
    data_conds <- conds(obs_data)
    conds_diff <- setdiff(data_conds, model_conds)
    if (length(conds_diff) >= 1) {
      conds_diff <- paste(conds_diff, collapse = ",")
      warning(
        "The Cond column in 'obs_data' provides conditions that are ",
        "not listed in the model's conditions ('",
        conds_diff,
        "'). ",
        "These conditions were dropped."
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
      message("Using the data supplied via the 'obs_data' argument.")
    }
    which_data <- "obs_data"
  }
  stopifnot(which_data %in% c("model", "obs_data"))

  # check the approach settings and set default values
  if (is.null(approach)) {
    approach <- "sep_c"
  }
  approach <- match.arg(
    approach,
    choices = c("sep_c", "sep_b", "agg_c", "hier_b")
  )

  # check the optimizer or use default
  requ_optimizer <- NULL
  if (approach %in% c("hier_b", "sep_b")) {
    requ_optimizer <- "DE-MCMC"
  }

  # if the user did not specify an optimizer, choose for them
  if (is.null(optimizer)) {
    if (is.null(requ_optimizer)) {
      if (!is.null(lower) | !is.null(upper)) {
        optimizer <- "DEoptim"
      } else {
        optimizer <- "Nelder-Mead"
      }
    } else {
      optimizer <- requ_optimizer
    }
  }

  # check if the optimizer is actually implemented
  optimizer <- match.arg(
    optimizer,
    choices = c("Nelder-Mead", "nmkb", "BFGS", "L-BFGS-B", "DEoptim", "DE-MCMC")
  )

  # if there is a required optimizer not matching with the input,
  # inform the user
  if (!is.null(requ_optimizer) && !identical(requ_optimizer, optimizer)) {
    if (messaging) {
      message(
        "The requested optimizer ('",
        optimizer,
        "') is not compatible ",
        "with the specified approach ('",
        approach,
        "'). Using the ",
        "following optimizer instead: '",
        requ_optimizer,
        "'."
      )
    }
    optimizer <- requ_optimizer
  } else {
    if (messaging) {
      message("Using optimizer '", optimizer, "'.")
    }
  }

  # check for the required cost function
  cost_fun <- cost_function(drift_dm_obj)
  if (approach %in% c("hier_b", "sep_b") && cost_fun != "neg_log_like") {
    cost_function(drift_dm_obj) <- "neg_log_like"
    if (messaging) {
      message(
        "Changing the 'cost_function' to 'neg_log_like', to get the ",
        "log-likelihood for Bayesian inference."
      )
    }
  }
  if (approach == "agg_c" && cost_fun != "rmse") {
    cost_function(drift_dm_obj) <- "rmse"
    if (messaging) message("Changing the 'cost_function' to 'rmse'.")
  }

  # check n_cores
  if (!is.numeric(n_cores) || length(n_cores) != 1 || n_cores <= 0) {
    stop("n_cores must be a single numeric >= 0")
  }

  # check seed input
  if (!is.null(seed)) {
    if (!is.numeric(seed) | length(seed) != 1) {
      stop("seed must be a single numeric")
    }
    withr::local_preserve_seed()
    set.seed(seed)
  }

  # now handle/dispatch the separate strategies ####
  # aggregated - classical ####
  if (approach == "agg_c") {
    if (which_data == "model" || !("ID" %in% colnames(obs_data))) {
      stop(
        "Cannot fit the model to aggregated data without data from multiple ",
        "participants supplied via the 'obs_data' argument. ",
        "Please provide individual-level data (with an ID column)."
      )
    }

    # aggregate the data
    if (messaging) {
      message("Aggregated data has been set to the model.")
    }
    drift_dm_obj <- set_agg_data(
      drift_dm_obj = drift_dm_obj,
      obs_data_ids = obs_data,
      probs = dots$probs,
      n_bins = dots$n_bins
    )
    if (messaging) {
      message(
        "Fitting the model once to the aggregated data. The ",
        "returned object will of type 'fits_agg_dm'."
      )
    }

    # then call the estimation function as if for a single model
    drift_dm_obj <- estimate_classical(
      drift_dm_obj = drift_dm_obj,
      optimizer = optimizer,
      start_vals = start_vals,
      return_runs = dots$return_runs,
      lower = lower,
      upper = upper,
      verbose = dots$verbose,
      de_n_cores = n_cores,
      control = control,
      round_digits = dots$round_digits,
      seed = seed,
      use_ez = dots$use_ez,
      n_lhs = dots$n_lhs
    )

    # wrap up the return value and create a custom class
    fit_obj <- list(drift_dm_obj = drift_dm_obj, obs_data_ids = obs_data)
    class(fit_obj) <- "fits_agg_dm"
    return(fit_obj)
  }

  # handle the classical ways of fitting a single or multiple participants
  toggle <- ""
  if (approach == "sep_c" || approach == "sep_b") {
    if (identical(which_data, "obs_data")) {
      n_ids <- if ("ID" %in% names(obs_data)) {
        length(unique(obs_data$ID))
      } else {
        0L
      }
      if (n_ids <= 1L) {
        obs_data(
          drift_dm_obj,
          probs = dots$probs,
          n_bins = dots$n_bins
        ) <- obs_data
        toggle <- "single"
      } else {
        toggle <- "multi"
      }
    } else {
      toggle <- "single"
    }
  }

  # handle the rather original case: Fit a model to just one participant
  # single (only for approach == "sep_c" or "sep_b", see above) ####
  if (toggle == "single") {
    if (messaging && approach == "sep_c") {
      message(
        "Fitting a single data set/participant (cost function: '",
        drift_dm_obj$cost_function,
        "'). The returned object will be the model ",
        "itself."
      )
    }
    if (approach == "sep_c") {
      drift_dm_obj <- estimate_classical(
        drift_dm_obj = drift_dm_obj,
        optimizer = optimizer,
        start_vals = start_vals,
        return_runs = dots$return_runs,
        lower = lower,
        upper = upper,
        verbose = dots$verbose,
        de_n_cores = n_cores,
        control = control,
        round_digits = dots$round_digits,
        seed = seed,
        use_ez = dots$use_ez,
        n_lhs = dots$n_lhs
      )
      return(drift_dm_obj)
    }

    if (messaging && approach == "sep_b") {
      message(
        "Fitting a single data set/participant using the Bayesian framework. ",
        "The result will be a fit object of type 'mcmc_dm'."
      )
    }
    if (approach == "sep_b") {
      mcmc_out <- estimate_bayesian(
        drift_dm_obj = drift_dm_obj,
        sampler = optimizer,
        n_chains = n_chains,
        burn_in = burn_in,
        samples = samples,
        prob_migration = prob_migration,
        prob_re_eval = prob_re_eval,
        verbose = dots$verbose,
        means = means,
        sds = sds,
        lower = lower,
        upper = upper,
        shapes = shapes,
        rates = rates
      )
      return(mcmc_out)
    }
  }

  # handle the separate case: Iterate over multiple participants
  # multi (only for approach == "sep_c" or "sep_b", see above) ####
  if (toggle == "multi") {
    if (messaging && approach == "sep_c") {
      message(
        "Fitting the model separately to multiple participants (cost function:",
        "'",
        drift_dm_obj$cost_function,
        "'). The result will be a fit object ",
        "of type 'fits_ids_dm'."
      )
    }
    if (!is.null(dots$return_runs) && approach == "sep_c") {
      warning(
        "The argument 'return_runs' is currently not supported for ",
        "'approach = sep_c'."
      )
    }
    if (approach == "sep_c") {
      all_fits <- estimate_classical_wrapper(
        drift_dm_obj = drift_dm_obj,
        obs_data_ids = obs_data,
        parallelization_strategy = parallelization_strategy,
        progress = dots$progress,
        start_vals = start_vals,
        optimizer = optimizer,
        lower = lower,
        upper = upper,
        verbose = dots$verbose,
        n_cores = n_cores,
        control = control,
        round_digits = dots$round_digits,
        seed = seed,
        use_ez = dots$use_ez,
        n_lhs = dots$n_lhs,
        probs = dots$probs,
        n_bins = dots$n_bins
      )
    }

    if (messaging && approach == "sep_b") {
      message(
        "Fitting the model separaetely to multiple participants using the ",
        "Bayesian framework. For now, the result will be a list of 'mcmc_dm' ",
        "objects."
      )
    }
    if (approach == "sep_b") {
      # prepare cluster
      cl <- NULL
      if (n_cores > 1) {
        cl <- parallel::makeCluster(n_cores)
        withr::defer(parallel::stopCluster(cl))
        parallel::clusterSetRNGStream(cl, iseed = seed)
        parallel::clusterExport(
          cl,
          varlist = c("estimate_bayesian"),
          envir = environment()
        )
      }

      # prepare progress
      if (!is.null(dots$progress) && dots$progress == 0) {
        op <- pbapply::pboptions(type = "none") # respect the user settings
        withr::defer(pbapply::pboptions(type = op$type))
      }

      # helper function for easier call
      run_estimation <- function(args, cl = NULL) {
        # one iteration for a single ID
        FUN <- function(one_id, args) {
          # set the data and delete from args
          obs_data(args$drift_dm_obj) <-
            args$obs_data[args$obs_data$ID == one_id, ]
          args$obs_data <- NULL
          # run the Bayesian estimation
          do.call(estimate_bayesian, args)
        }

        # Use parallel if cluster provided
        ids <- unique(args$obs_data$ID)
        if (!is.null(cl)) {
          result <- pbapply::pblapply(ids, FUN, args = args, cl = cl)
        } else {
          result <- pbapply::pblapply(ids, FUN, args = args)
        }
        names(result) <- ids
        return(result)
      }

      # prepare the args
      args <- list(
        obs_data = obs_data,
        drift_dm_obj = drift_dm_obj,
        sampler = optimizer,
        n_chains = n_chains,
        burn_in = burn_in,
        samples = samples,
        prob_migration = prob_migration,
        prob_re_eval = prob_re_eval,
        verbose = dots$verbose,
        means = means,
        sds = sds,
        lower = lower,
        upper = upper,
        shapes = shapes,
        rates = rates
      )

      # run everything
      all_fits <- run_estimation(args, cl)
    }
    return(all_fits)
  }

  # finally, handle the hierarchical case ####
  if (approach == "hier_b") {
    if (which_data != "obs_data") {
      stop(
        "The specified approach ('",
        approach,
        "') requires observed data ",
        "via the 'obs_data' argument."
      )
    }
    if (!("ID" %in% colnames(obs_data))) {
      stop("No 'ID' column found in 'obs_data'.")
    }
    if (length(unique(obs_data$ID)) <= 1L) {
      stop(
        "The 'ID' column provides only one participant. A hierarchical",
        " approach doesn't make sense in this case."
      )
    }

    if (messaging) {
      message(
        "Fitting the model hierarchically using a Bayesian framework. ",
        "The result will be a fit object of type 'mcmc_dm'"
      )
    }
    mcmc_out <- estimate_bayesian(
      drift_dm_obj = drift_dm_obj,
      obs_data_ids = obs_data,
      sampler = optimizer,
      n_chains = n_chains,
      burn_in = burn_in,
      samples = samples,
      prob_migration = prob_migration,
      prob_re_eval = prob_re_eval,
      verbose = dots$verbose,
      n_cores = n_cores,
      means = means,
      sds = sds,
      lower = lower,
      upper = upper,
      shapes = shapes,
      rates = rates,
      seed = seed
    )
    return(mcmc_out)
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
#' * Nelder-Mead (bounded or unbounded): `"Nelder-Mead"`, `"nmkb"`
#'   (via [stats::optim()] and [dfoptim::nmkb()], respectively)
#' * BFGS and L-BFGS-B (via [stats::optim()])
#' * Differential Evolution (via [DEoptim::DEoptim()])
#'
#'
#' @param drift_dm_obj an object inheriting from [dRiftDM::drift_dm].
#' @param optimizer a character string specifying the optimizer to use.
#'   Must be one of `"Nelder-Mead"`, `"nmkb"`, `"BFGS"`, `"L-BFGS-B"`, or `"DEoptim"`.
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
#' @param seed a seed, to make the results of DEoptim reproducible.
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
estimate_classical <- function(
  drift_dm_obj,
  optimizer,
  start_vals = NULL,
  return_runs = NULL,
  lower = NULL,
  upper = NULL,
  verbose = NULL,
  de_n_cores = 1,
  control = list(),
  round_digits = NULL,
  seed = NULL,
  use_ez = NULL,
  n_lhs = NULL
) {
  # input checks
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }

  cost_function <- cost_function(drift_dm_obj)
  no_raw <- cost_function == "neg_log_like" && is.null(drift_dm_obj$obs_data)
  no_agg <- cost_function == "rmse" &&
    (is.null(drift_dm_obj$stats_agg) || is.null(drift_dm_obj$stats_agg_info))
  if (no_raw || no_agg) {
    warning("No data set, passing back unmodified object")
    return(drift_dm_obj)
  }

  optimizer <- match.arg(
    optimizer,
    choices = c("nmkb", "Nelder-Mead", "BFGS", "L-BFGS-B", "DEoptim")
  )

  if (optimizer %in% c("nmkb", "L-BFGS-B", "DEoptim")) {
    if (is.null(lower) || is.null(upper)) {
      stop(
        "The optimizer '",
        optimizer,
        "' requires both 'lower' and 'upper' ",
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
    round_digits <- drift_dm_default_rounding()
  }
  if (!is.numeric(round_digits) || length(round_digits) != 1) {
    stop("'round_digits' must be a single numeric value")
  }

  # continue checks/defaults
  if (is.null(verbose)) {
    verbose <- 1
  }
  if (length(verbose) != 1 || !is.numeric(verbose) || verbose < 0) {
    stop("verbose must be numeric >= 0")
  }

  # ignore start_vals for DEoptim
  if (optimizer == "DEoptim" && !is.null(start_vals)) {
    start_vals <- NULL
    warning(
      "The 'start_vals' argument is ignored for the 'DEoptim' optimizer. ",
      "If you want to specify an initial population for DEoptim, please ",
      "use the 'control' argument (see ?DEoptim::DEoptim)."
    )
  }

  # maybe get starting values if they are not provided
  if (optimizer != "DEoptim" && is.null(start_vals)) {
    start_vals <- get_starting_values(
      drift_dm_obj,
      lower = lower,
      upper = upper,
      verbose = verbose,
      use_ez = use_ez,
      n_lhs = n_lhs
    )
  }

  # check if start_vals is a data.frame, and omit the ID column;
  # mabye turn into a simple vector if it has only one row
  if (is.data.frame(start_vals)) {
    start_vals <- start_vals[, names(start_vals) != "ID"]
    if (nrow(start_vals) == 1) {
      start_vals <- unlist(start_vals)
    }
  }

  # set start_vals or recursive call
  if (!is.null(start_vals)) {
    if (is.null(return_runs)) {
      return_runs <- FALSE
    }
    if (
      !is.logical(return_runs) | length(return_runs) != 1 | is.na(return_runs)
    ) {
      stop("'return_runs' must be a single logical")
    }

    # if start_vals is not a data.frame, try to set it and continue
    if (!is.data.frame(start_vals)) {
      if (!is_numeric(start_vals)) {
        stop("'start_vals' must be a data.frame or a numeric vector")
      }
      prm <- get_parameters_smart(
        drift_dm_obj = drift_dm_obj,
        input_a = start_vals
      )$vec_a
      drift_dm_obj$flex_prms_obj <- x2prms_vals(
        x = prm,
        flex_prms_obj = drift_dm_obj$flex_prms_obj
      )
    } else {
      # otherwise call the function recursively
      stopifnot(nrow(start_vals) > 1)
      results <- lapply(1:nrow(start_vals), function(i) {
        if (verbose > 0) {
          message("Optimization run: ", i)
        }
        one_row <- unlist(start_vals[i, ])
        estimate_classical(
          drift_dm_obj = drift_dm_obj,
          optimizer = optimizer,
          start_vals = one_row,
          return_runs = FALSE,
          lower = lower,
          upper = upper,
          verbose = verbose,
          control = control,
          round_digits = round_digits,
          use_ez = use_ez,
          n_lhs = n_lhs
        )
      })
      cost_values <- sapply(results, cost_value)
      prms <- do.call(rbind, lapply(results, coef))
      j <- which.min(cost_values)
      if (verbose > 0) {
        message(
          "Optimization run ",
          j,
          " yielded the smallest cost value"
        )
      }
      r <- results[[j]]
      if (return_runs) {
        r <- list(best_run = r, prms = prms, cost_values = cost_values)
      }
      return(r)
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

  # objective function to minimize
  goal_wrapper <- function(
    new_model_prms,
    drift_dm_obj,
    verbose,
    round_digits
  ) {
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
            "Parameters\n",
            prms_string,
            "\n==> gave a ",
            drift_dm_obj$cost_function,
            " of ",
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

  # providing starting of optimizer message
  if (verbose >= 1) {
    add <- ""
    check_opt <- optimizer %in% c("nmkb", "Nelder-Mead", "L-BFGS-B", "BFGS")
    if (check_opt) {
      add <- paste0(
        "with the following starting values:\n",
        prms_to_str(drift_dm_obj, collapse = ", ", sep = "=")
      )
    }
    message("Starting optimizer '", optimizer, "' ", add)
  }

  # now call the requested optimization routine
  out <- NULL
  if (optimizer == "nmkb") {
    out <- dfoptim::nmkb(
      par = coef(drift_dm_obj),
      fn = goal_wrapper,
      lower = lower,
      upper = upper,
      control = control,
      drift_dm_obj = drift_dm_obj,
      verbose = verbose,
      round_digits = round_digits
    )
    n_eval <- out$feval
  }

  if (optimizer == "Nelder-Mead") {
    x0 <- coef(drift_dm_obj)
    if (is.null(control$parscale)) {
      control$parscale <- pmax(x0, 1e-6)
    }
    out <- stats::optim(
      par = x0,
      fn = goal_wrapper,
      method = optimizer,
      control = control,
      drift_dm_obj = drift_dm_obj,
      verbose = verbose,
      round_digits = round_digits
    )
    n_eval <- out$counts
  }

  if (optimizer == "L-BFGS-B") {
    out <- stats::optim(
      par = coef(drift_dm_obj),
      fn = goal_wrapper,
      method = optimizer,
      control = control,
      lower = lower,
      upper = upper,
      drift_dm_obj = drift_dm_obj,
      verbose = verbose,
      round_digits = round_digits
    )
    n_eval <- out$counts
  }

  if (optimizer == "BFGS") {
    out <- stats::optim(
      par = coef(drift_dm_obj),
      fn = goal_wrapper,
      method = optimizer,
      control = control,
      drift_dm_obj = drift_dm_obj,
      verbose = verbose,
      round_digits = round_digits
    )
    n_eval <- out$counts
  }
  # extract infos and wrangle in a message if not present for optim
  if (!is.null(out)) {
    convergence <- out$convergence
    prms <- out$par
    message <- out$message
    n_iter <- NA_real_

    check <- optimizer %in% c("L-BFGS-B", "BFGS", "Nelder-Mead")
    if (check && is.null(message)) {
      if (convergence == 1) {
        message <- "maxit reached"
      }
      if (convergence == 10) message <- "degeneracy of Nelder-Mead simplex"
    }
  }

  if (optimizer == "DEoptim") {
    # create clusters
    cl <- NULL
    if (de_n_cores > 1) {
      cl <- parallel::makeCluster(de_n_cores)
      parallel::clusterSetRNGStream(cl = cl, iseed = seed)
      withr::defer(parallel::stopCluster(cl))
      parallel::clusterExport(
        cl = cl,
        varlist = c("goal_wrapper"),
        envir = environment()
      )
    }

    # set control parameters
    if (is.null(control$trace)) {
      control$trace <- FALSE
    }
    de_controls <- do.call(
      DEoptim::DEoptim.control,
      args = c(list(cluster = cl), control)
    )

    # set seed, if desired
    if (!is.null(seed)) {
      withr::local_preserve_seed()
      set.seed(seed)
    }

    # now run the optimization
    out <- DEoptim::DEoptim(
      fn = goal_wrapper,
      lower = lower,
      upper = upper,
      control = de_controls,
      drift_dm_obj = drift_dm_obj,
      verbose = verbose
    )
    n_eval <- out$optim$nfeval
    n_iter <- out$optim$iter
    prms <- out$optim$bestmem
    steptol <- de_controls$steptol
    itermax <- de_controls$itermax
    VTR <- de_controls$VTR

    # figure out convergence
    # info and message (default)
    convergence <- NA_integer_
    message <- "DEoptim had to run for the maximum number of iterations"

    # possible convergence via VTR or steptol
    vtr_on <- !is.infinite(VTR)
    stall_on <- steptol < itermax
    if (vtr_on || stall_on) {
      convergence <- 0L
      message <- "Successful convergence"

      # override if it actually wasn't successful
      if (n_iter == itermax) {
        convergence <- 1L
        message <- NULL
        if (vtr_on) {
          message <- c(message, "DEoptim did not reach VTR")
        }
        if (stall_on) {
          message <- c(message, "DEoptim did not meet the stall criterion")
        }
      }
      message <- paste(message, collapse = " + ")
    }
  }

  # continue with some exit messages and return value
  conv_flag <- TRUE
  if (is.na(convergence)) {
    conv_flag <- NA
  }
  if (!is.na(convergence) && convergence > 0) {
    add <- paste0(" (convergence message: ", message, ")")
    warning(
      "The optimization routine did not converge successfully",
      add,
      ". ",
      "Treat the estimated parameters with some caution."
    )
    conv_flag <- FALSE
  }

  n_eval <- n_eval[!is.na(n_eval)]
  if (verbose >= 1) {
    # special treatment of n_eval as vector (occurs for optim)
    if (length(n_eval) > 1) {
      message(
        "Optimization routine exited after ",
        n_eval[1],
        " function ",
        "evaluations and ",
        n_eval[2],
        " gradient evaluations\n"
      )
    } else {
      if (is.na(n_iter)) {
        message(
          "Optimization routine exited after ",
          n_eval,
          " function ",
          "evaluations"
        )
      } else {
        message(
          "Optimization routine exited after ",
          n_iter,
          " iterations."
        )
      }
    }
  }

  # set parameters and evaluate fully
  coef(drift_dm_obj, eval_model = TRUE) <- as.numeric(prms)

  if (verbose >= 1) {
    prms_string <- prms_to_str(drift_dm_obj, sep = " = ")
    message(
      "Final Parameters:\n",
      prms_string,
      "\n==> gave a ",
      drift_dm_obj$cost_function,
      " of ",
      round(drift_dm_obj$cost_value, round_digits)
    )
  }

  # finally, attach some infos for downstream processing
  drift_dm_obj$estimate_info <- list(
    conv_flag = conv_flag,
    optimizer = optimizer,
    message = message,
    n_iter = n_iter,
    n_eval = n_eval
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
#' @param seed an optional seed to make the results reproducible
#' @param ... further arguments passed to [estimate_classical()], including
#'   `lower`, `upper`, `verbose`, `control`, `round_digits`.
#'   Note that the argument `return_runs` is not supported.
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
estimate_classical_wrapper <- function(
  drift_dm_obj,
  obs_data_ids,
  parallelization_strategy = NULL,
  progress = NULL,
  start_vals = NULL,
  optimizer,
  n_cores = NULL,
  seed = NULL,
  ...
) {
  dots <- list(...)

  # idefault values for par_strat
  n_cores <- n_cores %||% 1

  # decide over parallelization strategy (1 = parallelize individuals,
  # 2 = parallelize within inidviduals, currently only supported for DEoptim
  # and handled by the DEoptim package
  if (is.null(parallelization_strategy) || optimizer != "DEoptim") {
    parallelization_strategy <- 1
  }
  if (n_cores == 1) {
    parallelization_strategy <- 1
  }
  if (
    !is.numeric(parallelization_strategy) |
      !(parallelization_strategy %in% c(1, 2))
  ) {
    stop("'parallelization_strategy' must be 1 or 2")
  }

  # continue checks
  if (is.null(progress)) {
    progress <- 1
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
      stop(
        "The 'IDs' listed in 'start_vals' don't match with the 'IDs' of ",
        "the observed data"
      )
    }
  }

  # create a list of models to fit, handling observed data, and starting values
  tmp <- function(one_id) {
    obs_data(
      drift_dm_obj,
      probs = dots$probs,
      n_bins = dots$n_bins
    ) <- obs_data_ids[obs_data_ids$ID == one_id, ]

    sv <- NULL
    if (!is.null(start_vals)) {
      sv <- start_vals[start_vals$ID == one_id, ]
      sv <- sv[names(sv) != "ID"]
      if (nrow(sv) == 1) sv <- unlist(sv)
    }

    return(list(drift_dm_obj = drift_dm_obj, start_vals = sv))
  }
  ids <- unique(obs_data_ids$ID)
  list_of_models <- lapply(ids, tmp)
  names(list_of_models) <- ids

  # helper function for easier call
  run_estimation <- function(model_list, dots, cl = NULL) {
    FUN <- function(one_model_start, dots) {
      estimate_classical(
        drift_dm_obj = one_model_start$drift_dm_obj,
        optimizer = dots$optimizer,
        start_vals = one_model_start$start_vals,
        lower = dots$lower,
        upper = dots$upper,
        verbose = dots$verbose,
        de_n_cores = dots$de_n_cores,
        control = dots$control,
        round_digits = dots$round_digits,
        seed = dots$seed,
        use_ez = dots$use_ez,
        n_lhs = dots$n_lhs
      )
    }

    # Use parallel if cluster provided
    if (!is.null(cl)) {
      pbapply::pblapply(model_list, FUN, dots = dots, cl = cl)
    } else {
      pbapply::pblapply(model_list, FUN, dots = dots)
    }
  }

  # Cluster Setup
  cl <- NULL
  if (parallelization_strategy == 1 & n_cores > 1) {
    cl <- parallel::makeCluster(n_cores)
    withr::defer(parallel::stopCluster(cl))
    parallel::clusterSetRNGStream(cl, iseed = seed)
    parallel::clusterExport(
      cl,
      varlist = c("estimate_classical"),
      envir = environment()
    )
    dots$de_n_cores <- 1 # to avoid that de_n_cores is larger than 1 for parstrat 1
  } else {
    dots$de_n_cores <- n_cores
    dots$seed <- seed
  }

  # progress bar output
  if (progress == 0) {
    op <- pbapply::pboptions(type = "none") # respect the user settings
    withr::defer(pbapply::pboptions(type = op$type))
  }

  # if no verbose was specified, set to 0  (to avoid interference with
  # the progress bar)
  if (is.null(dots$verbose)) {
    dots$verbose <- 0
  }

  # Run estimation
  dots$optimizer <- optimizer
  all_fits <- run_estimation(list_of_models, dots = dots, cl = cl)

  # Check convergence: did some of the fit runs not converge?
  not_conv <- !sapply(all_fits, \(x) x$estimate_info$conv_flag)
  messages <- lapply(all_fits, \(x) x$estimate_info$message)

  check <- any(vapply(not_conv, isTRUE, logical(1)))
  if (check) {
    ids <- names(not_conv[not_conv])
    not_conv_msg <- messages[not_conv]
    messages_formatted <- paste("-", unique(not_conv_msg))
    add <- ""
    n_ids <- length(ids)
    if (n_ids > drift_dm_n_id_trunc_warn()) {
      n_ids <- drift_dm_n_id_trunc_warn()
      add <- ", (and more)"
    }
    which_ids <- paste(paste(ids[1:n_ids], collapse = ", "), add, sep = "")
    warning(
      "The optimization routine did not converge successfully for the ",
      "following IDs: ",
      which_ids,
      "\n",
      "Summary of messages:\n",
      paste(messages_formatted, collapse = "\n")
    )
  }

  # wrap it up
  drift_dm_fit_info <- list(
    drift_dm_obj = drift_dm_obj,
    obs_data_ids = obs_data_ids,
    optimizer = dots$optimizer,
    conv_info = list(not_conv = not_conv, messages = messages)
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
#' @param obs_data_ids a data.frame containing individual-level observations.
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
  ids <- unique(obs_data_ids$ID)
  all_aggs_infos <- lapply(ids, \(one_id) {
    sub_dat <- obs_data_ids[obs_data_ids$ID == one_id, ]
    obs_data(drift_dm_obj, ...) <- sub_dat
    return(list(drift_dm_obj$stats_agg, drift_dm_obj$stats_agg_info))
  })

  all_aggs <- lapply(all_aggs_infos, \(x) x[[1]])

  # Get condition names
  conds <- conds(drift_dm_obj)

  # get the summary stats (assumes this is always the same)
  summary_stats_names <- names(all_aggs[[1]][[conds[1]]])

  # Average each stat across list entries
  avg_stats <- lapply(conds, function(one_cond) {
    res_list <- list()
    for (one_sum_stat in summary_stats_names) {
      tmp <- lapply(all_aggs, \(x) {
        x[[one_cond]][[one_sum_stat]]
      })
      avg <- colMeans(do.call(rbind, tmp))
      res_list[[one_sum_stat]] <- avg
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
