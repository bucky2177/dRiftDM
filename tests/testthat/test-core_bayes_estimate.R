
# HELPER FUNCTIONS REQUIRED FOR MCMC --------------------------------------

test_that("log_posterior_lower -> works as expected (hierarchical case)", {

  withr::local_preserve_seed()
  set.seed(1)
  model_subj <- readRDS(test_path("fixtures", "ratcliff.rds"))
  prms_solve(model_subj)[c("t_max", "dx", "dt")] = c(1, 0.05, 0.01)
  obs_data(model_subj) <- ratcliff_synth_data

  # create matrices
  thetas <- matrix(
    c(2,3,4, 0.4, 0.5, 0.6, 0.2, 0.3, 0.4), nrow = 3, byrow = TRUE
  )
  rownames(thetas) <- c("muc", "b", "non_dec")
  phis <- matrix(
    c(3,4,5, 0.5, 0.6, 0.7, 0.3, 0.4, 0.5, # hyper-means
      1,2,3, 0.1, 0.2, 0.3, 0.2, 0.3, 0.4), #hyper-sds
    nrow = 6, byrow = TRUE,
  )
  rownames(phis) <- c("M-muc", "M-b", "M-non_dec", "S-muc", "S-b", "S-non_dec")
  temperatures = c(1,2,3)

  # and the prior functions
  prior = get_default_prior_settings(drift_dm_obj = model_subj, level = "lower")
  prior = prior$log_dens_priors

  # get the likelihoods
  lls = sapply(1:3, \(x){
    coef(model_subj) <- thetas[,x]
    re_evaluate_model(model_subj)$log_like_val
  })

  # get the log-posteriors
  d_prior_1 = dtnorm(x = thetas[1,], mean = phis[1,], sd = phis[4,], log = TRUE)
  d_prior_2 = dtnorm(x = thetas[2,], mean = phis[2,], sd = phis[5,], log = TRUE)
  d_prior_3 = dtnorm(x = thetas[3,], mean = phis[3,], sd = phis[6,], log = TRUE)
  d_prior = rowSums(cbind(d_prior_1, d_prior_2, d_prior_3))

  # calculate the expected value
  posteriors = lls * temperatures + d_prior

  # call the function
  result <-  log_posterior_lower(
    thetas_one_subj_mat = thetas, all_phis_mat = phis, model_subj = model_subj,
    log_prior_lower_funs = prior, temperatures = temperatures,
    suppress_warnings = TRUE
  )

  # test expectations
  expect_named(result, c("posterior_vals", "log_like_vals"))
  expect_equal(lls * temperatures, result$log_like_vals)
  expect_equal(posteriors, result$posterior_vals)
})




# CUSTOM DISTRIBUTIONS ----------------------------------------------------

test_that("dtnorm works as expected", {

  test_trunc = truncnorm::dtruncnorm

  # compare density values with dtruncnorm
  x = seq(0, 1, 0.01)
  y_vals = dtnorm(x = x, mean = 0.5, sd = 0.2, lower = 0, upper = 0.9)
  exp = test_trunc(x = x, a = 0, b = 0.9, mean = 0.5, sd = 0.2)
  expect_equal(y_vals, exp)

  # works with log as well?
  y_vals = dtnorm(x = x, mean = 0.5, sd = 0.2, lower = 0, upper = 0.9,
                  log = TRUE)
  exp = log(test_trunc(x = x, a = 0, b = 0.9, mean = 0.5, sd = 0.2))
  expect_equal(y_vals, exp)


  # expect zeros and Infs when lower > upper
  expect_equal(dtnorm(x, lower = Inf, upper = -Inf), rep(0, length(x)))
  expect_equal(
    suppressWarnings(dtnorm(x, lower = Inf, upper = -Inf, log = TRUE)),
    rep(-Inf, length(x))
  )

  # test for correct parameter recycling (for correct usage in
  # log_posterior_hyper)
  mat <- matrix(seq(0, 1, length.out = 20), nrow = 5, ncol = 4)
  means = seq(0.3, 0.7, length.out = 5)
  sds = seq(0.1, 0.5, length.out = 5)

  result <- dtnorm(mat, mean = means, sd = sds, lower = 0, upper = 1)
  expect_equal(dim(result), dim(mat))
  exps <- vapply(seq_len(nrow(mat)), \(i){
    test_trunc(x = mat[i,], a = 0, b = 1, mean = means[i], sd = sds[i])
  }, FUN.VALUE = numeric(ncol(mat)))
  expect_equal(result, t(exps))
})

test_that("dtnorm handles boundary cases, extreme values, and NA values", {
  # NA and Infs as input
  x <- c(-Inf, 0, 0.5, 1, Inf, NA)

  y <- dtnorm(x, mean = 0.5, sd = 0.2, lower = 0, upper = 1)
  expect_true(is.na(y[6]))
  expect_equal(y[1], 0)
  expect_equal(y[5], 0)

  y <- dtnorm(x, mean = 0.5, sd = 0.2, lower = 0, upper = 1, log = TRUE)
  expect_true(is.na(y[6]))
  expect_equal(y[1], -Inf)
  expect_equal(y[5], -Inf)

  # boundary cases
  x <- c(-1, 0, 0.5, 1, 2)
  res <- dtnorm(x, mean = 0.5, sd = 1, lower = 0, upper = 1, log = TRUE)
  expect_equal(res[1], -Inf)
  expect_equal(res[5], -Inf)
  expect_true(all(is.finite(res[2:4])))

  x <- c(0.1, 0.5, 0.9)
  expect_equal(dtnorm(x, lower = 1, upper = 1), rep(0, 3))
  expect_equal(dtnorm(x, lower = 1, upper = 1, log = TRUE), rep(-Inf, 3))
  expect_equal(dtnorm(x, lower = 0.5, upper = 0.5), c(0, Inf, 0))
})

# rtnorm is already used and tested alongside simulate_values



# PRIOR FUNCTIONS (DENSITIES AND RANDOM GENERATION) -----------------------



test_that("d_default_prior_hyper -> works as expected", {
  # Input values (vector)
  x <- c(0.5, 1.5)
  mean <- 0.2
  sd <- 1
  lower <- 0
  upper <- Inf
  shape <- 2
  rate <- 1

  # obtained output
  log_val <- d_default_prior_hyper(
    x = x, mean = mean, sd = sd, lower = lower, upper = upper,
    shape = shape, rate = rate, log = TRUE
  )
  nonlog_val <- d_default_prior_hyper(
    x = x, mean = mean, sd = sd, lower = lower, upper = upper,
    shape = shape, rate = rate, log = FALSE
  )

  # test - log
  exp_log = dtnorm(x[1], mean = mean, sd = sd, lower = lower, upper = upper,
                   log = TRUE) +
    stats::dgamma(x = x[2], shape = shape, rate = rate, log = TRUE)
  expect_identical(log_val, exp_log)

  # test - nonlog
  exp_nonlog = dtnorm(x[1], mean = mean, sd = sd, lower = lower,
                      upper = upper) * stats::dgamma(x = x[2], shape = shape,
                                                     rate = rate)
  expect_identical(nonlog_val, exp_nonlog)

  # test matrix input
  x_mat <- matrix(c(0.5, 1.5, 0.75, 1.25), nrow = 2)  # [mean; sd]
  val_1 <- d_default_prior_hyper(
    x = c(0.5, 1.5), mean = 0.2, sd = 1, lower = 0, upper = Inf,
    shape = 2, rate = 1, log = TRUE
  )
  val_2 <- d_default_prior_hyper(
    x = c(0.75, 1.25), mean = 0.3, sd = 2, lower = -5, upper = 5,
    shape = 3, rate = 2, log = TRUE
  )
  val_mat <- d_default_prior_hyper(
    x = x_mat, mean = c(0.2, 0.3), sd = c(1,2), lower = c(0, -5),
    upper = c(Inf, 5), shape = c(2,3), rate = c(1,2), log = TRUE
  )
  expect_identical(val_mat, c(val_1, val_2))

  # test wrong input
  expect_error(
    d_default_prior_hyper(x = c(1), mean = 0, sd = 1, lower = 0, upper = 1,
                          shape = 2, rate = 1, log = FALSE)
  )
  mat <- matrix(c(1,2,3,4,5,6), nrow = 3)
  expect_error(
    d_default_prior_hyper(x = mat, mean = 0, sd = 1, lower = 0, upper = 1,
                          shape = 2, rate = 1, log = FALSE)
  )
})

test_that("d_default_prior_hyper handles extreme and NA values", {
  x_bad <- c(0.5, NA)
  val <- d_default_prior_hyper(x = x_bad, mean = 0.2, sd = 1, lower = 0,
                               upper = Inf, shape = 2, rate = 1, log = TRUE)
  expect_true(is.na(val))

  x_neg <- c(0.5, -1)
  val_neg <- d_default_prior_hyper(x = x_neg, mean = 0.2, sd = 1, lower = 0,
                                   upper = Inf, shape = 2, rate = 1, log = TRUE)
  expect_equal(val_neg, -Inf)
})




test_that("r_default_prior_hyper -> works as expected", {
  withr::local_preserve_seed()
  set.seed(456)
  n <- 1000
  lower <- 0
  upper <- 1
  shape = 2
  rate = 1
  mean = 0.5
  sd = 0.25

  out <- r_default_prior_hyper(
    n = n, mean = mean, sd = sd, lower = lower, upper = upper,
    shape = shape, rate = rate
  )

  expect_true(all(out[1, ] >= lower & out[1, ] <= upper))
  expect_true(all(out[2, ] > 0))  # rgamma always positive
  expect_true(all(rownames(out) == c("r_m", "r_sd")))
  expect_true(is.matrix(out))
  expect_equal(dim(out), c(2, n))

  # quantiles should match roughly
  exp_quants = qgamma(c(0.25, 0.5, 0.75), shape = shape, rate = rate)
  obs_quants = quantile(out[2,], probs = c(0.25, 0.5, 0.75))
  obs_quants = unname(obs_quants)
  expect_equal(obs_quants, exp_quants, tolerance = 0.1)

  # quantiles should match roughly
  exp_quants = truncnorm::qtruncnorm(c(0.25, 0.5, 0.75), a = lower, b = upper,
                                     mean = mean, sd = sd)
  obs_quants = quantile(out[1,], probs = c(0.25, 0.5, 0.75))
  obs_quants = unname(obs_quants)
  expect_equal(obs_quants, exp_quants, tolerance = 0.1)


  # check if return value is true for n = 1
  out_vec = r_default_prior_hyper(
    n = 1, mean = mean, sd = sd, lower = lower, upper = upper,
    shape = shape, rate = rate
  )
  expect_true(all(names(out_vec) == c("r_m", "r_sd")))
  expect_length(out_vec, 2)
  expect_true(is.vector(out_vec))

  # check failure for n < 1
  expect_error(
    r_default_prior_hyper(
      n = 0, mean = mean, sd = sd, lower = lower, upper = upper,
      shape = shape, rate = rate
    )
  )
})

test_that("get_default_prior_settings -> returns correct structure", {
  drift_dm_obj <- readRDS(test_path("fixtures", "ratcliff.rds"))
  mean <- c(muc = 0.5, b = 0.6)

  out <- get_default_prior_settings(drift_dm_obj, level = "none", mean = mean, sd = mean)
  expect_s3_class(out, "ddm_prior_settings")
  expect_named(out, c("log_dens_priors", "r_priors"))
  expect_equal(attr(out, "level"), "none")
  expect_length(out$log_dens_priors, 3)
  expect_length(out$r_priors, 3)

  out <- get_default_prior_settings(drift_dm_obj, level = "hyper", mean = mean, sd = mean)
  expect_equal(attr(out, "level"), "hyper")

  out <- get_default_prior_settings(drift_dm_obj, level = "lower", mean = mean, sd = mean)
  expect_equal(attr(out, "level"), "lower")
})

test_that("get_default_prior_settings -> creates working functions", {
  drift_dm_obj <- readRDS(test_path("fixtures", "ratcliff.rds"))
  mean <- c(muc = 1)
  sd <- c(muc = 0)

  #####
  # checks for none
  out <- get_default_prior_settings(drift_dm_obj, level = "none", mean = mean,
                                    sd = sd)

  # Should return a log-density and random sampler function
  expect_type(out$log_dens_priors$muc, "closure")
  expect_type(out$r_priors$non_dec, "closure")

  # Should return expected value
  expect_equal(out$log_dens_priors$muc(c(0,1)), c(-Inf, Inf))
  expect_equal(out$r_priors$muc(1), 1)

  # should return expected dimension
  expect_length(out$log_dens_priors$non_dec(c(0,1)), 2)
  expect_length(out$r_priors$non_dec(1), 1)


  ###
  # checks for hyper
  out <- get_default_prior_settings(drift_dm_obj, level = "hyper", mean = mean,
                                    sd = sd)

  # should return expected dimension
  expect_length(out$log_dens_priors$non_dec(c(0,1)), 1)
  expect_length(out$r_priors$non_dec(1), 2)


  ###
  # checks for lower
  out <- get_default_prior_settings(drift_dm_obj, level = "lower", mean = mean,
                                    sd = sd)

  # should return expected dimension
  expect_length(out$log_dens_priors$non_dec(c(0,1)), 2)
  expect_length(out$r_priors$non_dec(1), 1)

})

test_that("get_default_prior_settings -> returns correct density values", {
  # Create dummy drift_dm_obj (not used in this path)
  drift_dm_obj <- readRDS(test_path("fixtures", "ratcliff.rds"))

  coef(drift_dm_obj)["b"] = 0.5
  mean = c(muc = 3)
  sd = c(muc = 2)
  lower = c(muc = 1)
  upper = c(muc = 5)
  shape = c(muc = 1)
  rate = c(muc = 2)

  ### Test none
  # Get prior settings
  prior <- get_default_prior_settings(
    drift_dm_obj = drift_dm_obj,
    level = "none", mean = mean, sd = sd, lower = lower, upper = upper
  )

  # Manually compute expected density
  expected_log_density_b <- dtnorm(0.4, mean = 0.5, sd = 0.5,
                                 lower = -Inf, upper = Inf,
                                 log = TRUE)
  expected_log_density_muc <- dtnorm(3.5, mean = 3, sd = 2,
                                   lower = 1, upper = 5,
                                   log = TRUE)
  # Compare with the generated function
  actual_log_density_b <- prior$log_dens_priors$b(0.4)
  actual_log_density_muc <- prior$log_dens_priors$muc(3.5)

  expect_equal(actual_log_density_b, expected_log_density_b)
  expect_equal(actual_log_density_muc, expected_log_density_muc)


  ### Test hyper
  # Get prior settings
  prior <- get_default_prior_settings(
    drift_dm_obj = drift_dm_obj,
    level = "hyper", mean = mean, sd = sd, lower = lower, upper = upper,
    shape = shape, rate = rate
  )

  # Manually compute expected density
  expected_log_density_b <- d_default_prior_hyper(
    c(0.4, 1), mean = 0.5, sd = 0.5, lower = -Inf, upper = Inf, shape = 1,
    rate = 1, log = TRUE
  )
  expected_log_density_muc <- d_default_prior_hyper(
    c(3.5, 2), mean = 3, sd = 2, lower = 1, upper = 5, shape = 1, rate = 2,
    log = TRUE
  )
  # Compare with the generated function
  actual_log_density_b <- prior$log_dens_priors$b(c(0.4, 1))
  actual_log_density_muc <- prior$log_dens_priors$muc(c(3.5, 2))

  expect_equal(actual_log_density_b, expected_log_density_b)
  expect_equal(actual_log_density_muc, expected_log_density_muc)



  ### Test hyper
  # Get prior settings
  prior <- get_default_prior_settings(
    drift_dm_obj = drift_dm_obj,
    level = "lower", mean = mean, sd = sd, lower = lower, upper = upper,
    shape = shape, rate = rate
  )

  # Manually compute expected density
  expected_log_density_b <- dtnorm(
    c(0.4), mean = 0.5, sd = 0.5, lower = -Inf, upper = Inf, log = TRUE
  )
  expected_log_density_muc <- dtnorm(
    3.5, mean = 3, sd = 2, lower = 1, upper = 5, log = TRUE
  )

  # Compare with the generated function
  actual_log_density_b <- prior$log_dens_priors$b(0.4, mean = 0.5, sd = 0.5)
  actual_log_density_muc <- prior$log_dens_priors$muc(3.5, mean = 3, sd = 2)

  expect_equal(actual_log_density_b, expected_log_density_b)
  expect_equal(actual_log_density_muc, expected_log_density_muc)
})
