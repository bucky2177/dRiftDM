
test_that("estimate_dm -> sep_c works as expected", {

  model <- ratcliff_dummy
  prms_solve(model)[c("dx", "dt", "t_max")] <- c(.01, .01, 1.5)
  l_u <- get_lower_upper(model)

  # test the old variant: Data attached to the model and classical
  # optimization via the negative log-likelihood and DEoptim
  # -> trimmed for speed with VTR = 0
  tmp <- model
  tmp <- estimate_dm(
    drift_dm_obj = tmp, lower = l_u$lower, upper = l_u$upper, messaging = FALSE,
    verbose = 0, control = list(VTR = 0, trace = FALSE)
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
    drift_dm_obj = tmp2, obs_data = data, lower = l_u$lower, upper = l_u$upper,
    messaging = FALSE, verbose = 0,
    control = list(VTR = 0, trace = FALSE)
  )
  expect_identical(tmp2, tmp)

  # check messages
  expect_message(
    expect_message(
      expect_message(
        tmp2 <- estimate_dm(
          drift_dm_obj = tmp2, obs_data = data, lower = l_u$lower,
          upper = l_u$upper, verbose = 0, control = list(VTR = 0, trace = FALSE)
        ), "supplied via the 'obs_data'"
      ), "DEoptim"
    ), "Fitting a single data set"
  )

  # check verbose
  expect_message(
    expect_message(
      expect_message(
        tmp2 <- estimate_dm(
          drift_dm_obj = tmp2, obs_data = data, lower = l_u$lower,
          upper = l_u$upper, verbose = 1, messaging = FALSE,
          control = list(VTR = 0, trace = FALSE)
        ), "Starting optimizer"
      ), "exited after 0 iterations"
    ), "Final Parameters"
  )


  # now try multiple individuals for fits_ids
  tmp <- model
  data <- rbind(
    cbind(ID = 1, ratcliff_synth_data), cbind(ID = 2, ratcliff_synth_data)
  )
  all_fits <- estimate_dm(
    drift_dm_obj = tmp, obs_data = data, lower = l_u$lower, upper = l_u$upper,
    messaging = FALSE, verbose = 0, progress = 0,
    control = list(VTR = 0, trace = FALSE)
  )
  expect_identical(
    names(all_fits), c("drift_dm_fit_info", "all_fits")
  )
  expect_identical(
    names(all_fits$drift_dm_fit_info),
    c("drift_dm_obj", "obs_data_ids", "optimizer", "conv_info")
  )


  # different optimizer and conv. warning
  tmp <- model
  expect_warning(
    tmp <- estimate_dm(
      drift_dm_obj = tmp, approach = "sep_c", lower = l_u$lower, upper = l_u$upper,
      optimizer = "nmkb", messaging = FALSE, verbose = 0, progress = 0,
      control = list(maxfeval = 20)
    ), "fevals exceeded"
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
  prms_solve(tmp)["t_max"] = 0.5
  data <- ratcliff_synth_data
  expect_error(
    estimate_dm(drift_dm_obj = tmp, obs_data = data), "increase 't_max'"
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
    cbind(ID = 1, ratcliff_synth_data), cbind(ID = 2, ratcliff_synth_data)
  )
  expect_warning(
    estimate_dm(
      drift_dm_obj = tmp, obs_data = data, optimizer = "DEoptim",
      lower = l_u$lower, upper = l_u$upper, messaging = FALSE,
      control = list(VTR = 0), return_runs = TRUE, progress = 0
    ), "'return_runs' is currently not supported"
  )
})



test_that("estimate_dm -> agg_c works as expected", {

  model <- ratcliff_dummy
  prms_solve(model)[c("dx", "dt", "t_max")] <- c(.01, .01, 1.5)
  l_u <- get_lower_upper(model)

  data <- rbind(
    cbind(ID = 1, ratcliff_synth_data), cbind(ID = 2, ratcliff_synth_data)
  )

  # test the fitted aggregated variant
  tmp <- model
  agg_fit <- estimate_dm(
    drift_dm_obj = tmp, obs_data = data, approach = "agg_c", lower = l_u$lower,
    upper = l_u$upper, messaging = FALSE, n_bins = 6,
    probs = c(0.1, 0.4, 0.6, 0.8),
    verbose = 0, control = list(VTR = 0.3, trace = FALSE, NP = 30)
  )

  expect_identical(names(agg_fit), c("drift_dm_obj", "obs_data_ids"))

  # did the aggregation work?
  exp_stats = calc_stats(
    data, type = c("quantiles", "cafs"), level = "group", n_bins = 6,
    probs = c(0.1, 0.4, 0.6, 0.8)
  )
  model_stats = agg_fit$drift_dm_obj$stats_agg
  expect_identical(exp_stats$quantiles$Quant_corr, model_stats$null$quantiles_corr)
  expect_identical(exp_stats$cafs$P_corr, model_stats$null$cafs)

  # check messages
  expect_message(
    expect_message(
      expect_message(
        expect_message(
          expect_message(
            estimate_dm(
              drift_dm_obj = tmp, obs_data = data, approach = "agg_c",
              lower = l_u$lower, upper = l_u$upper, messaging = TRUE,
              verbose = 0, control = list(VTR = 0.3, trace = FALSE, NP = 30)
            ), "supplied via the 'obs_data'"
          ), "DEoptim"
        ), "Changing the 'cost_function' to 'rmse'"
      ), "Fitting the model to aggregated"
    ), "Aggregated data has been set"
  )

  # check verbose
  withr::local_seed(1)
  expect_message(
    expect_message(
      expect_message(
        estimate_dm(
          drift_dm_obj = tmp, obs_data = data, approach = "agg_c",
          lower = l_u$lower, upper = l_u$upper, messaging = FALSE,
          verbose = 1, control = list(VTR = 0.03, trace = FALSE, NP = 30)
        ), "Starting optimizer"
      ), "exited after 5 iterations"
    ), "Final Parameters"
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
    drift_dm_obj = tmp, obs_data = data, approach = "sep_b", lower = l_u$lower,
    messaging = FALSE, verbose = 0, burn_in = 1, samples = 1, n_chains = 5
  )
  expect_s3_class(mcmc_obj, "mcmc_dm")

  # now with data already attached
  tmp <- model
  withr::local_seed(1)
  mcmc_obj2 <- estimate_dm(
    drift_dm_obj = tmp, approach = "sep_b", lower = l_u$lower,
    messaging = FALSE, verbose = 0, burn_in = 1, samples = 1, n_chains = 5
  )
  expect_identical(mcmc_obj$theta, mcmc_obj2$theta)

  # check messages
  expect_message(
    expect_message(
      expect_message(
        estimate_dm(
          drift_dm_obj = tmp, approach = "sep_b", lower = l_u$lower,
          messaging = TRUE, verbose = 0, burn_in = 1, samples = 1, n_chains = 3
        ), "Using the data attached"
      ), "DE-MCMC"
    ), "using the Bayesian framework"
  )

  # check verbose
  expect_message(
    expect_message(
      estimate_dm(
        drift_dm_obj = tmp, approach = "sep_b", lower = l_u$lower,
        messaging = FALSE, verbose = 1, burn_in = 1, samples = 1, n_chains = 3
      ), "starting values"
    ), "sampling procedure"
  )



  # now try multiple individuals for the list of mcmc_obj (experimental)
  tmp <- model
  data <- rbind(
    cbind(ID = 1, ratcliff_synth_data), cbind(ID = 2, ratcliff_synth_data)
  )
  all_fits <- estimate_dm(
    drift_dm_obj = tmp, obs_data = data, approach = "sep_b", lower = l_u$lower,
    messaging = FALSE, verbose = 0, burn_in = 1, progress = 0, samples = 1,
    n_chains = 3, n_cores = 2, seed = 1
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
    cbind(ID = 1, ratcliff_synth_data), cbind(ID = 2, ratcliff_synth_data)
  )
  mcmc_obj <- estimate_dm(
    drift_dm_obj = model, obs_data = data, approach = "hier_b", lower = l_u$lower,
    messaging = FALSE, verbose = 0, burn_in = 1, progress = 0, samples = 1,
    n_chains = 3, n_cores = 2, seed = 1
  )
  expect_true(attr(mcmc_obj, "hierarchical"))



  mock_call_bayesian <- function(...) return(1)

  with_mocked_bindings(
    estimate_bayesian = mock_call_bayesian,
    {
      # check messages
      expect_message(
        expect_message(
          expect_message(
            estimate_dm(
              drift_dm_obj = model, obs_data = data, approach = "hier_b"
            ), "supplied via the 'obs_data' argument"
          ), "DE-MCMC"
        ), "hierarchically"
      )
    },
  )
})



