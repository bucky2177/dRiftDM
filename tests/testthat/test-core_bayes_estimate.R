# CUSTOM DISTRIBUTIONS ----------------------------------------------------


test_that("dtnorm works as expected", {

  # compare density values with dtruncnorm
  x = seq(0, 1, 0.01)
  y_vals = dtnorm(x = x, mean = 0.5, sd = 0.2, lower = 0, upper = 0.9)
  exp = truncnorm::dtruncnorm(x = x, a = 0, b = 0.9, mean = 0.5, sd = 0.2)
  expect_equal(y_vals, exp)

  # works with log as well?
  y_vals = dtnorm(x = x, mean = 0.5, sd = 0.2, lower = 0, upper = 0.9,
                  log = TRUE)
  exp = log(truncnorm::dtruncnorm(x = x, a = 0, b = 0.9, mean = 0.5, sd = 0.2))
  expect_equal(y_vals, exp)


  # expect zeros and Infs when lower > upper
  expect_equal(dtnorm(x, lower = Inf, upper = -Inf), rep(0, length(x)))
  expect_equal(
    suppressWarnings(dtnorm(x, lower = Inf, upper = -Inf, log = TRUE)),
    rep(-Inf, length(x))
  )
})

# rtnorm is already used and tested alongside simulate_values



# PRIOR FUNCTIONS (DENSITIES AND RANDOM GENERATION) -----------------------



test_that("d_default_prior_hyper -> works as expected", {
  # Input values
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
  expect_true(all(colnames(out) == c("r_m", "r_sd")))

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

})

