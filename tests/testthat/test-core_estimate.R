# APPROACHES --------------------------------------------------------------

test_that("estimate_dm -> sep_c works as expected", {
  model <- ratcliff_dummy
  prms_solve(model)[c("dx", "dt", "t_max")] <- c(.01, .01, 1.5)
  l_u <- get_lower_upper(model)

  # test the old variant: Data attached to the model and classical
  # optimization via the negative log-likelihood and DEoptim
  # -> trimmed for speed with VTR = 0
  tmp <- model
  tmp <- estimate_dm(
    drift_dm_obj = tmp,
    lower = l_u$lower,
    upper = l_u$upper,
    messaging = FALSE,
    verbose = 0,
    control = list(VTR = 0, trace = FALSE)
  )

  expect_identical(
    names(tmp$estimate_info),
    c("conv_flag", "optimizer", "message", "n_iter", "n_eval")
  )
  expect_identical(tmp$estimate_info$optimizer, "DEoptim")

  # now with data supplied separately
  tmp2 <- model
  data <- obs_data(tmp2, messaging = FALSE)
  tmp2 <- estimate_dm(
    drift_dm_obj = tmp2,
    obs_data = data,
    lower = l_u$lower,
    upper = l_u$upper,
    messaging = FALSE,
    verbose = 0,
    control = list(VTR = 0, trace = FALSE)
  )
  expect_identical(tmp2, tmp)

  # check if starting values are used
  tmp <- ratcliff_dm(var_non_dec = TRUE)
  expect_snapshot(
    estimate_dm(drift_dm_obj = tmp, obs_data = ratcliff_synth_data, seed = 1)
  )

  # check messages
  expect_message(
    expect_message(
      expect_message(
        tmp2 <- estimate_dm(
          drift_dm_obj = tmp2,
          obs_data = data,
          lower = l_u$lower,
          upper = l_u$upper,
          verbose = 0,
          control = list(VTR = 0, trace = FALSE)
        ),
        "supplied via the 'obs_data'"
      ),
      "DEoptim"
    ),
    "Fitting a single data set"
  )

  # check verbose
  expect_message(
    expect_message(
      expect_message(
        tmp2 <- estimate_dm(
          drift_dm_obj = tmp2,
          obs_data = data,
          lower = l_u$lower,
          upper = l_u$upper,
          messaging = FALSE,
          control = list(VTR = 0, trace = FALSE)
        ),
        "Starting optimizer"
      ),
      "exited after 0 iterations"
    ),
    "Final Parameters"
  )

  # now try multiple individuals for fits_ids
  tmp <- model
  data <- rbind(
    cbind(ID = 1, ratcliff_synth_data),
    cbind(ID = 2, ratcliff_synth_data)
  )
  expect_snapshot(
    all_fits <- estimate_dm(
      drift_dm_obj = tmp,
      obs_data = data,
      lower = l_u$lower,
      upper = l_u$upper,
      messaging = TRUE,
      verbose = 0,
      progress = 0,
      control = list(VTR = 0, trace = FALSE)
    )
  )
  expect_identical(
    names(all_fits),
    c("drift_dm_fit_info", "all_fits")
  )
  expect_identical(
    names(all_fits$drift_dm_fit_info),
    c("drift_dm_obj", "obs_data_ids", "optimizer", "conv_info")
  )

  # different optimizer and conv. warning
  tmp <- model
  expect_warning(
    tmp <- estimate_dm(
      drift_dm_obj = tmp,
      approach = "sep_c",
      lower = l_u$lower,
      upper = l_u$upper,
      optimizer = "nmkb",
      messaging = FALSE,
      verbose = 0,
      progress = 0,
      control = list(maxfeval = 20),
      use_ez = 0,
      n_lhs = 0
    ),
    "fevals exceeded"
  )
  expect_identical(tmp$estimate_info$optimizer, "nmkb")
  expect_identical(tmp$estimate_info$conv_flag, FALSE)
  expect_match(tmp$estimate_info$message, "fevals exceeded")
  expect_identical(tmp$estimate_info$n_iter, NA_real_)
  expect_identical(tmp$estimate_info$n_eval, 20)

  # some edge cases
  expect_error(estimate_dm(drift_dm_obj = "foo"), "not of type drift_dm")

  tmp <- model
  tmp$obs_data <- NULL
  expect_error(estimate_dm(drift_dm_obj = tmp), "No observed data")

  tmp <- model
  tmp$obs_data <- NULL
  prms_solve(tmp)["t_max"] <- 0.5
  data <- ratcliff_synth_data
  expect_error(
    estimate_dm(drift_dm_obj = tmp, obs_data = data),
    "increase 't_max'"
  )

  tmp <- model
  expect_error(
    estimate_dm(drift_dm_obj = tmp, n_cores = "foo", messaging = FALSE),
    "n_cores"
  )

  tmp <- model
  expect_error(
    estimate_dm(drift_dm_obj = tmp, seed = "foo", messaging = FALSE),
    "seed"
  )

  tmp <- model
  data <- rbind(
    cbind(ID = 1, ratcliff_synth_data),
    cbind(ID = 2, ratcliff_synth_data)
  )
  expect_warning(
    estimate_dm(
      drift_dm_obj = tmp,
      obs_data = data,
      optimizer = "DEoptim",
      lower = l_u$lower,
      upper = l_u$upper,
      messaging = FALSE,
      control = list(VTR = 0),
      return_runs = TRUE,
      progress = 0
    ),
    "'return_runs' is currently not supported"
  )
})


test_that("estimate_dm -> agg_c works as expected", {
  model <- ratcliff_dummy
  prms_solve(model)[c("dx", "dt", "t_max")] <- c(.01, .01, 1.5)
  l_u <- get_lower_upper(model)

  data <- rbind(
    cbind(ID = 1, ratcliff_synth_data),
    cbind(ID = 2, ratcliff_synth_data)
  )

  # test the fitted aggregated variant
  tmp <- model
  agg_fit <- estimate_dm(
    drift_dm_obj = tmp,
    obs_data = data,
    approach = "agg_c",
    lower = l_u$lower,
    upper = l_u$upper,
    messaging = FALSE,
    n_bins = 6,
    probs = c(0.1, 0.4, 0.6, 0.8),
    verbose = 0,
    control = list(VTR = 0.3, trace = FALSE, NP = 30)
  )

  expect_identical(names(agg_fit), c("drift_dm_obj", "obs_data_ids"))

  # did the aggregation work?
  exp_stats <- calc_stats(
    data,
    type = c("quantiles", "cafs"),
    level = "group",
    n_bins = 6,
    probs = c(0.1, 0.4, 0.6, 0.8)
  )
  model_stats <- agg_fit$drift_dm_obj$stats_agg
  expect_identical(
    exp_stats$quantiles$Quant_corr,
    model_stats$null$quantiles_corr
  )
  expect_identical(exp_stats$cafs$P_corr, model_stats$null$cafs)

  # check messages (1)
  tmp <- model
  expect_snapshot(
    estimate_dm(
      drift_dm_obj = tmp,
      obs_data = data,
      approach = "agg_c",
      lower = l_u$lower,
      upper = l_u$upper,
      messaging = TRUE,
      verbose = 0,
      control = list(VTR = 0.3, trace = FALSE, NP = 30)
    )
  )

  # check verbose
  withr::local_seed(1)
  expect_message(
    expect_message(
      expect_message(
        estimate_dm(
          drift_dm_obj = tmp,
          obs_data = data,
          approach = "agg_c",
          lower = l_u$lower,
          upper = l_u$upper,
          messaging = FALSE,
          verbose = 1,
          control = list(VTR = 0.03, trace = FALSE, NP = 30)
        ),
        "Starting optimizer"
      ),
      "exited after 6 iterations"
    ),
    "Final Parameters"
  )

  # some edge cases
  tmp <- model
  expect_error(
    estimate_dm(drift_dm_obj = tmp, approach = "agg_c", messaging = FALSE),
    "'obs_data'"
  )
})


test_that("estimate_dm -> sep_b works as expected", {
  model <- ratcliff_dummy
  prms_solve(model)[c("dx", "dt", "t_max")] <- c(.01, .01, 1.5)
  l_u <- get_lower_upper(model)

  # test the bayesian estimation (separately for each participant)
  # -> one participant
  tmp <- model
  data <- ratcliff_synth_data
  tmp$obs_data <- NULL
  withr::local_seed(1)
  mcmc_obj <- estimate_dm(
    drift_dm_obj = tmp,
    obs_data = data,
    approach = "sep_b",
    lower = l_u$lower,
    messaging = FALSE,
    verbose = 0,
    burn_in = 1,
    samples = 1,
    n_chains = 5
  )
  expect_s3_class(mcmc_obj, "mcmc_dm")

  # now with data already attached
  tmp <- model
  withr::local_seed(1)
  mcmc_obj2 <- estimate_dm(
    drift_dm_obj = tmp,
    approach = "sep_b",
    lower = l_u$lower,
    messaging = FALSE,
    verbose = 0,
    burn_in = 1,
    samples = 1,
    n_chains = 5
  )
  expect_identical(mcmc_obj$theta, mcmc_obj2$theta)

  # check messages
  expect_message(
    expect_message(
      expect_message(
        estimate_dm(
          drift_dm_obj = tmp,
          approach = "sep_b",
          lower = l_u$lower,
          messaging = TRUE,
          verbose = 0,
          burn_in = 1,
          samples = 1,
          n_chains = 3
        ),
        "Using the data attached"
      ),
      "DE-MCMC"
    ),
    "using the Bayesian framework"
  )

  # check verbose
  expect_message(
    expect_message(
      estimate_dm(
        drift_dm_obj = tmp,
        approach = "sep_b",
        lower = l_u$lower,
        messaging = FALSE,
        verbose = 1,
        burn_in = 1,
        samples = 1,
        n_chains = 3
      ),
      "starting values"
    ),
    "sampling procedure"
  )

  # now try multiple individuals for the list of mcmc_obj (experimental)
  tmp <- model
  data <- rbind(
    cbind(ID = 1, ratcliff_synth_data),
    cbind(ID = 2, ratcliff_synth_data)
  )
  expect_snapshot(
    all_fits <- estimate_dm(
      drift_dm_obj = tmp,
      obs_data = data,
      approach = "sep_b",
      lower = l_u$lower,
      messaging = TRUE,
      verbose = 0,
      burn_in = 1,
      progress = 0,
      samples = 1,
      n_chains = 3,
      n_cores = 2,
      seed = 1
    )
  )
  expect_vector(all_fits)
  expect_s3_class(all_fits$`1`, "mcmc_dm")
  expect_s3_class(all_fits$`2`, "mcmc_dm")
})


test_that("estimate_dm -> hier_b runs as expected", {
  model <- ratcliff_dummy
  prms_solve(model)[c("dx", "dt", "t_max")] <- c(.01, .01, 1.5)
  l_u <- get_lower_upper(model)

  # simple call runs?
  data <- rbind(
    cbind(ID = 1, ratcliff_synth_data),
    cbind(ID = 2, ratcliff_synth_data)
  )
  mcmc_obj <- estimate_dm(
    drift_dm_obj = model,
    obs_data = data,
    approach = "hier_b",
    lower = l_u$lower,
    messaging = FALSE,
    verbose = 0,
    burn_in = 1,
    progress = 0,
    samples = 1,
    n_chains = 3,
    n_cores = 2,
    seed = 1
  )
  expect_true(attr(mcmc_obj, "hierarchical"))

  mock_call_bayesian <- function(...) {
    return(1)
  }

  with_mocked_bindings(
    estimate_bayesian = mock_call_bayesian,
    {
      # check messages
      expect_message(
        expect_message(
          expect_message(
            estimate_dm(
              drift_dm_obj = model,
              obs_data = data,
              approach = "hier_b"
            ),
            "supplied via the 'obs_data' argument"
          ),
          "DE-MCMC"
        ),
        "hierarchically"
      )
    },
  )

  # input checks
  expect_error(
    estimate_dm(drift_dm_obj = model, approach = "hier_b", messaging = FALSE),
    "requires observed data via the 'obs_data' argument"
  )

  tmp <- data
  tmp$ID <- NULL
  expect_error(
    estimate_dm(
      drift_dm_obj = model,
      obs_data = tmp,
      approach = "hier_b",
      messaging = FALSE
    ),
    "No 'ID' column"
  )

  tmp <- data[data$ID == 1, ]
  expect_error(
    estimate_dm(
      drift_dm_obj = model,
      obs_data = tmp,
      approach = "hier_b",
      messaging = FALSE
    ),
    "only one participant"
  )
})


# ESTIMATE_DM INPUT CHECKS/WARNINGS ---------------------------------------

test_that("estimate_dm throws warning/errors for unreasonable input", {
  # prepare a model
  model <- dmc_dummy
  prms_solve(model)[c("dx", "dt")] <- .01

  # warning about unused conditions
  data <- obs_data(model, messaging = FALSE)
  tmp <- data[data$Cond == "comp", ]
  tmp$Cond <- "neutral"
  tmp <- rbind(data, tmp)
  expect_warning(
    expect_warning(
      estimate_dm(
        drift_dm_obj = model,
        obs_data = tmp,
        control = list(maxit = 1),
        progress = 0,
        verbose = 0,
        messaging = FALSE
      ),
      "These conditions were dropped."
    )
  )

  # message about switching optimizer and cost function for Bayesian inference
  cost_function(model) <- "rmse"
  expect_snapshot(
    out <- estimate_dm(
      drift_dm_obj = model,
      obs_data = data,
      approach = "sep_b",
      optimizer = "Nelder-Mead",
      samples = 1,
      burn_in = 1,
      n_chains = 4,
      progress = 0,
      verbose = 0,
      messaging = TRUE
    )
  )
})


# ESTIMATE_CLASSICAL ------------------------------------------------------

test_that("estimate_classical -> start_vals are applied and correctly mapped", {
  model <- ratcliff_dummy
  prms_solve(model)[c("dx", "dt")] <- .01
  coef(model)["muc"] <- 5

  # named vector
  expect_snapshot(
    suppressWarnings(
      estimate_classical(
        model,
        verbose = 2,
        optimizer = "Nelder-Mead",
        control = list(maxit = 1),
        start_vals = c(b = 0.3, non_dec = 0.2, muc = 2)
      )
    )
  )

  # or data.frame for multiple runs
  start_vals <- data.frame(
    muc = c(1, 3, 7),
    b = c(0.3, 0.5, 0.9),
    non_dec = c(0.2, 0.3, 0.4)
  )
  expect_snapshot(
    suppressWarnings(
      res <- estimate_classical(
        model,
        verbose = 2,
        optimizer = "Nelder-Mead",
        control = list(maxit = 1),
        start_vals = start_vals,
        round_digits = 2
      )
    )
  )

  exp_best <- start_vals[2, ]
  expect_s3_class(res, "drift_dm")
  expect_true(all(abs(coef(res) - exp_best) / exp_best <= 0.2))

  # return_runs returns all runs
  suppressWarnings(
    res <- estimate_classical(
      model,
      verbose = 0,
      optimizer = "Nelder-Mead",
      control = list(maxit = 1),
      start_vals = start_vals,
      return_runs = TRUE
    )
  )
  expect_type(res, "list")
  expect_identical(names(res), c("best_run", "prms", "cost_values"))
  expect_s3_class(res[[1]], "drift_dm")
  expect_true(is.matrix(res[[2]]))
  expect_true(!is.matrix(res[[3]]) && is.numeric(res[[3]]))
})


test_that("estimate_classical -> optimizer is dispatched correctly", {
  model <- ratcliff_dummy
  prms_solve(model)[c("dx", "dt")] <- .01

  # start vals must be a data.frame or numeric vector
  expect_error(
    estimate_classical(
      model,
      verbose = 0,
      optimizer = "Nelder-Mead",
      start_vals = NA
    ),
    "'start_vals'"
  )

  # return_runs must be logical
  start_vals <- data.frame(
    muc = c(1, 3, 7),
    b = c(0.3, 0.5, 0.9),
    non_dec = c(0.2, 0.3, 0.4)
  )
  expect_error(
    estimate_classical(
      model,
      verbose = 0,
      optimizer = "Nelder-Mead",
      start_vals = start_vals,
      return_runs = NA,
      control = list(maxit = 1)
    ),
    "'return_runs'"
  )

  # DEoptim is not compatible with stat_vals must be logical
  start_vals <- data.frame(
    muc = c(1, 3, 7),
    b = c(0.3, 0.5, 0.9),
    non_dec = c(0.2, 0.3, 0.4)
  )
  l_u <- get_lower_upper(model)
  expect_warning(
    estimate_classical(
      model,
      verbose = 0,
      optimizer = "DEoptim",
      start_vals = start_vals,
      lower = l_u$lower,
      upper = l_u$upper,
      control = list(VTR = 5000, trace = TRUE)
    ),
    "'start_vals' argument is ignored for the 'DEoptim' optimizer"
  )
})


test_that("estimate_clasical -> correctly dispatches to stats::optim", {
  # get a dummy model
  model <- ratcliff_dummy
  l_u <- get_lower_upper(model)

  # mock optim and capture method input
  mocked_optim <- function(...) {
    dots <- list(...)
    # use message as transport
    list(par = dots$par, counts = 1L, convergence = 0L, message = dots$method)
  }

  # call with Nelder-Mead
  with_mocked_bindings(
    {
      res <- estimate_classical(
        drift_dm_obj = model,
        optimizer = "Nelder-Mead",
        verbose = 0
      )
    },
    optim = mocked_optim,
    .package = "stats"
  )
  expect_identical(res$estimate_info$message, "Nelder-Mead")
  expect_identical(res$estimate_info$optimizer, "Nelder-Mead")

  # call with BFGS
  with_mocked_bindings(
    {
      res <- estimate_classical(
        drift_dm_obj = model,
        optimizer = "BFGS",
        verbose = 0
      )
    },
    optim = mocked_optim,
    .package = "stats"
  )
  expect_identical(res$estimate_info$message, "BFGS")
  expect_identical(res$estimate_info$optimizer, "BFGS")

  # call with L-BFGS-B
  with_mocked_bindings(
    {
      res <- estimate_classical(
        drift_dm_obj = model,
        optimizer = "L-BFGS-B",
        verbose = 0,
        lower = l_u$lower,
        upper = l_u$upper
      )
    },
    optim = mocked_optim,
    .package = "stats"
  )
  expect_identical(res$estimate_info$message, "L-BFGS-B")
  expect_identical(res$estimate_info$optimizer, "L-BFGS-B")
})


test_that("estimate_clasical -> correctly dispatches to dfoptim::dfoptim", {
  # get a dummy model
  model <- ratcliff_dummy
  l_u <- get_lower_upper(model)

  # mock optim and capture method input
  mocked_nmkb <- function(...) {
    dots <- list(...)
    # use message as transport
    list(par = dots$par, feval = 1L, convergence = 0L, message = "dfoptim")
  }

  # call with Nelder-Mead
  with_mocked_bindings(
    {
      res <- estimate_classical(
        drift_dm_obj = model,
        optimizer = "nmkb",
        verbose = 0,
        lower = l_u$lower,
        upper = l_u$upper
      )
    },
    nmkb = mocked_nmkb,
    .package = "dfoptim"
  )
  expect_identical(res$estimate_info$message, "dfoptim")
  expect_identical(res$estimate_info$optimizer, "nmkb")
})


# ESTIMATE_CLASSICAL_WRAPPER ----------------------------------------------

test_that("estimate_classical_wrapper -> start_vals correctly passed forward", {
  # get some sample model and data
  model <- ratcliff_dummy
  prms_solve(model)[c("dx", "dt")] <- .01

  dat1 <- ratcliff_synth_data
  dat1$ID <- 1
  dat2 <- ratcliff_synth_data
  dat2$ID <- 2
  data <- rbind(dat1, dat2)

  # create starting values, with multiple runs per subject
  start_vals <- data.frame(
    ID = c(1, 1, 2, 2),
    muc = c(2, 3, 4, 5),
    b = c(0.4, 0.5, 0.6, 0.7),
    non_dec = c(0.2, 0.3, 0.4, 0.5)
  )

  # have a run through all starting values
  expect_snapshot(
    suppressWarnings(
      estimate_classical_wrapper(
        model,
        obs_data_ids = data,
        verbose = 2,
        optimizer = "Nelder-Mead",
        control = list(maxit = 1),
        start_vals = start_vals,
        progress = 0
      )
    )
  )

  # create starting values, with one run per subject
  start_vals <- data.frame(
    ID = c(1, 2),
    muc = c(2, 4),
    b = c(0.4, 0.6),
    non_dec = c(0.2, 0.4)
  )

  # have a run through all starting values
  expect_snapshot(
    suppressWarnings(
      res <- estimate_classical_wrapper(
        model,
        obs_data_ids = data,
        verbose = 2,
        optimizer = "Nelder-Mead",
        control = list(maxit = 1),
        start_vals = start_vals,
        progress = 0
      )
    )
  )
  expect_s3_class(res, "fits_ids_dm")
})


test_that("estimate_classical_wrapper -> multiple core usage works", {
  # get some sample model and data
  model <- ratcliff_dummy
  l_u <- get_lower_upper(model)
  prms_solve(model)[c("dx", "dt")] <- .01

  dat1 <- ratcliff_synth_data
  dat1$ID <- 1
  dat2 <- ratcliff_synth_data
  dat2$ID <- 2
  data <- rbind(dat1, dat2)

  # two cores - para-strat 1 (between participants)
  suppressWarnings(
    res_2 <- estimate_classical_wrapper(
      model,
      obs_data_ids = data,
      optimizer = "Nelder-Mead",
      n_cores = 2,
      control = list(maxit = 20),
      progress = 0
    )
  )

  # one core - para-strat 1 (between participants)
  suppressWarnings(
    res_1 <- estimate_classical_wrapper(
      model,
      obs_data_ids = data,
      optimizer = "Nelder-Mead",
      n_cores = 1,
      control = list(maxit = 20),
      progress = 0
    )
  )
  expect_identical(coef(res_1), coef(res_2))

  # two cores - para-strat 2 (within participants for DE)
  expect_snapshot(
    suppressWarnings(
      res_3 <- estimate_classical_wrapper(
        model,
        obs_data_ids = data,
        optimizer = "DEoptim",
        n_cores = 2,
        lower = l_u$lower,
        upper = l_u$upper,
        progress = 0,
        control = list(VTR = -300, trace = TRUE),
        parallelization_strategy = 2,
        seed = 1
      )
    )
  )

  # two cores - para-strat 1 (between participants for DE)
  expect_snapshot({
    suppressWarnings(
      res_4 <- estimate_classical_wrapper(
        model,
        obs_data_ids = data,
        optimizer = "DEoptim",
        n_cores = 2,
        lower = l_u$lower,
        upper = l_u$upper,
        progress = 0,
        control = list(VTR = -300, trace = TRUE),
        parallelization_strategy = 1,
        seed = 1
      )
    )
    coef(res_4)
  })
})

test_that("estimate_classical_wrapper -> input error messages", {
  model <- ratcliff_dummy

  dat1 <- ratcliff_synth_data
  dat1$ID <- 1
  dat2 <- ratcliff_synth_data
  dat2$ID <- 2
  data <- rbind(dat1, dat2)

  # parallelization_strategy
  expect_error(
    estimate_classical_wrapper(
      drift_dm_obj = model,
      obs_data_ids = data,
      optimizer = "DEoptim",
      parallelization_strategy = 3, # invalid
      n_cores = 2 # avoid auto-forcing to 1
    ),
    "'parallelization_strategy' must be 1 or 2"
  )

  # progress
  expect_error(
    estimate_classical_wrapper(
      drift_dm_obj = model,
      obs_data_ids = data,
      optimizer = "Nelder-Mead",
      progress = -1
    ),
    "'progress' must be >= 0"
  )

  # start_vals
  expect_error(
    estimate_classical_wrapper(
      drift_dm_obj = model,
      obs_data_ids = data,
      optimizer = "Nelder-Mead",
      start_vals = 1:3
    ),
    "'start_vals' must be a data.frame"
  )

  # start_vals
  expect_error(
    estimate_classical_wrapper(
      drift_dm_obj = model,
      obs_data_ids = data,
      optimizer = "Nelder-Mead",
      start_vals = 1:3
    ),
    "'start_vals' must be a data.frame"
  )

  # start_vals must have an ID column
  expect_error(
    estimate_classical_wrapper(
      drift_dm_obj = model,
      obs_data_ids = data,
      optimizer = "Nelder-Mead",
      start_vals = data.frame(a = c(1, 2), b = c(3, 4))
    ),
    "No 'ID' column found in 'start_vals'"
  )

  # start_vals must have IDs matching with obs_data_ids
  expect_error(
    estimate_classical_wrapper(
      drift_dm_obj = model,
      obs_data_ids = data,
      optimizer = "Nelder-Mead",
      start_vals = data.frame(ID = c(1L, 3L), a = c(0.1, 0.2), b = c(0.3, 0.4))
    ),
    "The 'IDs' listed in 'start_vals' don't match"
  )
})
