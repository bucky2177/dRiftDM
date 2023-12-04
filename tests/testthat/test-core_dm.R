test_that("creating a drift_dm object", {
  # no data
  my_prms <- c("a" = 2, "b" = 3, "c" = 4)
  conds <- c("a")
  free_prms <- c("c", "a")
  a_model <- drift_dm(prms_model = my_prms, conds = conds, free_prms = free_prms)
  expect_identical(a_model$prms_model, my_prms)
  expect_identical(a_model$conds, conds)
  expect_identical(a_model$free_prms, c("a", "c"))
  expect_identical(a_model$prms_solve, default_solver_prms())
  expect_identical(a_model$solver, default_solver())
  expect_identical(class(a_model), c("drift_dm"))


  # with data
  some_data <- ratcliff_data
  my_prms <- c("a" = 2, "b" = 3, "c" = 4)
  conds <- c("null")
  a_model <- drift_dm(
    prms_model = my_prms, conds = conds,
    obs_data = some_data, sigma = 2,
    t_max = 4, dt = .01, dx = .05
  )
  expect_identical(a_model$prms_model, my_prms)
  expect_identical(a_model$conds, conds)
  expect_identical(a_model$free_prms, names(my_prms))
  exp_solver_prms <- c(
    "sigma" = 2, "t_max" = 4, "dt" = .01, "dx" = .05,
    "nt" = 400, "nx" = 40
  )
  expect_identical(a_model$prms_solve, exp_solver_prms)
  expect_identical(a_model$solver, default_solver())

  rts_corr <- list(null = some_data$RT[some_data$Error == 0])
  rts_err <- list(null = some_data$RT[some_data$Error == 1])
  exp_data <- list(rts_corr = rts_corr, rts_err = rts_err)
  expect_identical(a_model$obs_data, exp_data)
})


test_that("input fails dm_drift", {
  # create failing cases
  my_prms <- c("a" = 2, "b" = 3, "c" = 4)
  conds <- c("null")
  expect_error(drift_dm(numeric(), conds), "prms_model")
  expect_error(drift_dm(my_prms, NULL), "conds")
  expect_error(drift_dm(my_prms, conds, sigma = NULL), regexp = "sigma")
  expect_error(drift_dm(my_prms, conds, t_max = NULL), regexp = "t_max")
  expect_error(drift_dm(my_prms, conds, dx = NULL), regexp = "dx")
  expect_error(drift_dm(my_prms, conds, dt = NULL), regexp = "dt")
  expect_error(drift_dm(my_prms, conds, free_prms = c(1, 2)),
    regexp = "free_prms"
  )
  expect_error(drift_dm(my_prms, conds, free_prms = c("a", "d")),
    regexp = "free_prms"
  )
})


test_that("validate_model fails as expected", {
  my_prms <- c("a" = 2, "b" = 3, "c" = 4)
  conds <- c("null")
  a_model <- drift_dm(
    prms_model = my_prms, conds = conds,
    sigma = 1, t_max = 3, dt = .001, dx = .001
  )

  temp <- a_model
  temp$prms_model <- c()
  expect_error(validate_drift_dm(temp), "prms_model")
  temp <- a_model
  temp$conds <- vector(mode = "character")
  expect_error(validate_drift_dm(temp), "conds")
  temp <- a_model
  temp$free_prms <- vector(mode = "numeric")
  expect_error(validate_drift_dm(temp), "free_prms")
  temp <- a_model
  temp$free_prms <- c(names(my_prms), "d")
  expect_error(validate_drift_dm(temp), "free_prms")
  temp <- a_model
  temp$free_prms <- c(c("b", "c", "a"))
  expect_warning(validate_drift_dm(temp), "free_prms")
  expect_identical(
    suppressWarnings(validate_drift_dm(temp)$free_prms),
    c("a", "b", "c")
  )
  temp <- a_model
  temp$free_prms <- c(c("b", "c", "b"))
  expect_error(validate_drift_dm(temp), "duplicate entries in free_prms")
  temp <- a_model
  temp$prms_model <- c("a" = 2, "b" = 3, "b" = 4)
  expect_error(validate_drift_dm(temp), "duplicate entries in prms_model")


  # failures for solver prms -> <= 0 or decimal
  temp <- a_model
  temp$prms_solve["sigma"] <- 0
  expect_error(validate_drift_dm(temp), "sigma")
  temp <- a_model
  temp$prms_solve["t_max"] <- 0
  expect_error(validate_drift_dm(temp), "t_max")
  temp <- a_model
  temp$prms_solve["dt"] <- 0
  expect_error(validate_drift_dm(temp), "dt")
  temp <- a_model
  temp$prms_solve["dx"] <- 0
  expect_error(validate_drift_dm(temp), "dx")
  temp <- a_model
  temp$prms_solve["nt"] <- 0
  expect_error(validate_drift_dm(temp), "nt")
  temp <- a_model
  temp$prms_solve["nx"] <- 0
  expect_error(validate_drift_dm(temp), "nx")
  temp <- a_model
  temp$prms_solve["nx"] <- 0.2
  expect_error(validate_drift_dm(temp), "nx")
  temp <- a_model
  temp$prms_solve["nt"] <- 0.2
  expect_error(validate_drift_dm(temp), "nt")

  # change t_max/dt or dx without adjusting nT nx
  temp <- a_model
  temp$prms_solve["t_max"] <- 2
  expect_error(validate_drift_dm(temp), "t_max")
  temp <- a_model
  temp$prms_solve["dx"] <- .1
  expect_error(validate_drift_dm(temp), "dx")

  # too small nx, nt
  temp <- a_model
  temp$prms_solve["dt"] <- 1
  temp$prms_solve["nt"] <- 3
  expect_warning(validate_drift_dm(temp), "nt seems very small")
  temp <- a_model
  temp$prms_solve["dx"] <- 0.5
  temp$prms_solve["nx"] <- 4
  expect_warning(validate_drift_dm(temp), "nx seems very small")

  # change t_max to a smaller value than max(rt)
  temp <- a_model
  some_data <- ratcliff_data
  temp <- set_obs_data(temp, some_data, eval_model = F)
  temp$prms_solve["t_max"] <- 0.5
  temp$prms_solve["nt"] <- 500
  expect_warning(validate_drift_dm(temp), "adjusting t_max and nt")
  temp <- suppressWarnings(validate_drift_dm(temp))
  expect_identical(temp$prms_solve[["t_max"]], 0.714)
  expect_identical(temp$prms_solve[["nt"]], 714)

  # weird solver input
  temp <- a_model
  temp$solver <- c("two", "things")
  expect_error(validate_drift_dm(temp), "solver")

  # wrong input for drift_dm
  expect_error(validate_drift_dm("hallo"), "not of type drift_dm")

  # wrong function declarations
  temp <- a_model
  temp$comp_funs$mu_fun <- x_beta
  expect_error(
    validate_drift_dm(temp),
    "the second argument of mu_fun must be 't_vec'"
  )

  temp <- a_model
  temp$comp_funs$x_fun <- mu_constant
  expect_error(
    validate_drift_dm(temp),
    "the second argument of x_fun must be 'x_vec'"
  )


  temp <- a_model
  temp$comp_funs$x_fun <- function(foo, x_vec, one_cond) {
    return(NULL)
  }
  expect_error(
    validate_drift_dm(temp),
    "the first argument of x_fun must be 'drift_dm_obj'"
  )

  temp <- a_model
  temp$comp_funs$x_fun <- function(drift_dm_obj, x_vec, foo) {
    return(NULL)
  }
  expect_error(
    validate_drift_dm(temp),
    "the third argument of x_fun must be 'one_cond'"
  )
})


test_that("standard methods for the ddm components work as expected", {
  my_prms <- c("a" = 2, "b" = 3, "c" = 4)
  conds <- c("null")
  a_model <- drift_dm(
    prms_model = my_prms, conds = conds,
    sigma = 1, t_max = 3, dt = .01, dx = .01
  )

  t_vec <- seq(0, 3, 0.01)
  x_vec <- seq(-1, 1, 0.01)
  def_mu <- a_model$comp_funs$mu_fun(a_model, t_vec, one_cond = "W")
  def_mu_int <- a_model$comp_funs$mu_int_fun(a_model, t_vec, one_cond = "W")
  def_b <- a_model$comp_funs$b_fun(a_model, t_vec, one_cond = "W")
  def_dtb <- a_model$comp_funs$dt_b_fun(a_model, t_vec, one_cond = "W")
  def_nt <- a_model$comp_funs$nt_fun(a_model, t_vec, one_cond = "W")
  def_x <- a_model$comp_funs$x_fun(a_model, x_vec, one_cond = "W")

  expect_identical(def_mu, rep(3, 301))
  expect_identical(def_mu_int, 3 * t_vec)
  expect_identical(def_b, rep(0.5, 301))
  expect_identical(def_dtb, rep(0, 301))
  exp_nt <- rep(0, 301)
  exp_nt[31] <- 1 / .01
  expect_identical(def_nt, exp_nt)
  exp_x <- rep(0, 201)
  exp_x[101] <- 1 / .01
  expect_identical(def_x, exp_x)

  # failures for t_vec
  t_vec <- numeric()
  expect_error(
    a_model$comp_funs$mu_fun(a_model, t_vec, one_cond = "W"),
    "t_vec is not a vector"
  )
  expect_error(
    a_model$comp_funs$mu_int_fun(a_model, t_vec, one_cond = "W"),
    "t_vec is not a vector"
  )
  expect_error(
    a_model$comp_funs$b_fun(a_model, t_vec, one_cond = "W"),
    "t_vec is not a vector"
  )
  expect_error(
    a_model$comp_funs$dt_b_fun(a_model, t_vec, one_cond = "W"),
    "t_vec is not a vector"
  )
  expect_error(
    a_model$comp_funs$nt_fun(a_model, t_vec, one_cond = "W"),
    "t_vec is not a vector"
  )
  expect_error(
    a_model$comp_funs$x_fun(a_model, t_vec, one_cond = "W"),
    "x_vec is not a vector"
  )
})


test_that("AIC and BIC calculation works as expected", {
  aic_bic <- calc_ic(ll = 2, k = 3, n = 300)
  expect_identical(aic_bic[["AIC"]], 2 * 3 - 2 * 2)
  expect_identical(aic_bic[["BIC"]], 3 * log(300) - 2 * 2)
})

test_that("re_evaluate_model works as expected", {
  my_prms <- c("a" = 2, "b" = 3, "c" = 4)
  conds <- c("null")
  a_model <- drift_dm(
    prms_model = my_prms, conds = conds,
    sigma = 1, t_max = 3, dt = .01, dx = .01
  )
  expect_null(a_model$log_like_val)
  expect_null(a_model$ic_vals)

  a_model <- set_obs_data(a_model, ratcliff_data, eval_model = T)
  log_like_val <- a_model$log_like_val
  expect_true(!is.null(a_model$log_like_val))
  aic_bic <- a_model$ic_vals
  expect_identical(aic_bic[["AIC"]], 2 * 3 - 2 * a_model$log_like_val)
  expect_identical(
    aic_bic[["BIC"]],
    3 * log(nrow(ratcliff_data)) - 2 * a_model$log_like_val
  )

  expect_error(re_evaluate_model(NULL), "drift_dm_obj")
})


test_that("set_model_prms works as expected", {
  # before evaluating
  my_prms <- c("a" = 2, "b" = 3, "c" = 4)
  conds <- c("null")
  a_model <- drift_dm(
    prms_model = my_prms, conds = conds,
    sigma = 1, t_max = 1, dt = .01, dx = .01,
    obs_data = ratcliff_data
  )
  a_model <- set_model_prms(a_model, c(1, 2, 3), eval_model = F)
  expect_null(a_model$log_like_val)
  expect_null(a_model$ic_vals)
  expect_identical(unname(a_model$prms_model), c(1, 2, 3))

  # after re-evaluating
  a_model <- set_model_prms(a_model, c(1, 2, 3), eval_model = T)
  expect_true(!is.null(a_model$log_like_val))

  # errors and with fewer free_prms
  my_prms <- c("a" = 2, "b" = 3, "c" = 4)
  conds <- c("null")
  a_model <- drift_dm(
    prms_model = my_prms, conds = conds,
    free_prms = c("a", "c"), sigma = 1, t_max = 1, dt = .01,
    dx = .01
  )
  a_model <- set_model_prms(a_model, c(8, 4), eval_model = F)
  expect_identical(unname(a_model$prms_model), c(8, 3, 4))
  a_model$log_like_val <- 23
  a_model <- set_model_prms(a_model, c(8, 4), eval_model = T)
  expect_null(a_model$log_like_val)
  expect_null(a_model$ic_vals)

  expect_error(
    set_model_prms(a_model, NULL, eval_model = F),
    "new_model_prms are not of type numeric"
  )
  expect_error(
    set_model_prms(a_model, c(1), eval_model = F),
    "new_prms don't match the number of free_prms"
  )
  expect_error(
    set_model_prms("hallo", c(1, 3, 4), eval_model = F),
    "drift_dm_obj"
  )
  expect_error(
    set_model_prms(a_model, c(1, 3, 4), eval_model = NULL),
    "eval_model"
  )
})


test_that("set_free_prms works as expected", {
  # before evaluating
  my_prms <- c("a" = 2, "b" = 3, "cd" = 4)
  conds <- c("null")
  a_model <- drift_dm(
    prms_model = my_prms, conds = conds,
    sigma = 1, t_max = 1, dt = .01, dx = .01,
    obs_data = ratcliff_data
  )
  a_model <- set_free_prms(a_model, c("b", "a"))
  expect_identical(a_model$free_prms, c("a", "b"))

  expect_warning(set_free_prms(a_model, c("c", "a")), "corrected")
  a_model <- suppressWarnings(set_free_prms(a_model, c("c", "a")))
  expect_identical(a_model$free_prms, c("a", "cd"))

  expect_error(set_free_prms(a_model, c("x", "a")), "arg")
  expect_error(set_free_prms(a_model, NULL), "character")

  expect_error(
    set_free_prms("hallo", c("b", "a")),
    "drift_dm_obj"
  )
})


test_that("set_solver_settings works as expected", {
  # before evaluating
  my_prms <- c("a" = 2, "b" = 3, "cd" = 4)
  conds <- c("null")
  a_model <- drift_dm(
    prms_model = my_prms, conds = conds,
    sigma = 1, t_max = 1, dt = .01, dx = .01,
    obs_data = ratcliff_data
  )
  a_model <- set_solver_settings(a_model, c("t_max", "dt", "dx", "sigma"),
    c(3, .1, .1, 2),
    eval_model = F
  )
  expect_identical(
    a_model$prms_solve,
    c(sigma = 2, t_max = 3, dt = .1, dx = .1, nt = 30, nx = 20)
  )
  a_model <- set_solver_settings(a_model, c("solver"),
    c("bla"),
    eval_model = F
  )
  expect_identical(a_model$solver, "bla")

  # errors and warnings
  expect_error(
    set_solver_settings(a_model, c("solver"),
      c("bla"),
      eval_model = T
    ),
    "not implemented yet"
  )
  a_model$solver <- "kfe"
  expect_warning(re_evaluate_model(a_model), "negative density values")

  # further errors
  expect_error(
    set_solver_settings(a_model, c("t_max", "dx"), c(2)),
    "don't match"
  )
  expect_warning(
    set_solver_settings(a_model, c("so"), c(2)),
    "Automatically corrected"
  )
  expect_error(set_solver_settings(a_model, c(), NULL), "not of type character")
  expect_error(
    set_solver_settings(a_model, c("t_max"), list(4)),
    "type numeric or character"
  )
  expect_error(
    set_solver_settings("hallo", "t_max", 3),
    "drift_dm_obj"
  )
})




test_that("set_obs_data and check_raw_data throw expected errors", {
  my_prms <- c("a" = 2, "b" = 3, "cd" = 4)
  conds <- c("null")
  a_model <- drift_dm(prms_model = my_prms, conds = conds)

  # wrong inputs to set_obs_data
  expect_error(set_obs_data("uff", ratcliff_data), "not of type drift_dm")
  temp_data <- ratcliff_data
  temp_data <- rbind(temp_data, data.frame(RT = 0.4, Error = 1, Cond = "foo"))
  expect_warning(
    set_obs_data(a_model, temp_data),
    "not listed in the model's conditions"
  )
  a_model$conds <- c("null", "foo")
  expect_error(
    set_obs_data(a_model, ratcliff_data),
    "not part of the Cond column"
  )

  a_model$conds <- c("null")
  expect_error(set_obs_data(a_model, list()), "not a data frame")

  temp_data <- data.frame(RT = 1, Error = "1", Cond = "null")
  expect_warning(set_obs_data(a_model, temp_data), "not of type numeric")

  temp_data <- data.frame(RT = "1", Error = 1, Cond = "null")
  expect_warning(set_obs_data(a_model, temp_data), "not of type numeric")

  a_model$conds <- c("1")
  temp_data <- data.frame(RT = 1, Error = 1, Cond = 1)
  expect_warning(set_obs_data(a_model, temp_data), "not of type character")

  temp_data <- data.frame(RT = 1, Error = 1)
  expect_error(set_obs_data(a_model, temp_data), "no Cond")
  temp_data <- data.frame(RT = 1, Cond = "null")
  expect_error(set_obs_data(a_model, temp_data), "no Error")
  temp_data <- data.frame(Error = 1, Cond = "null")
  expect_error(set_obs_data(a_model, temp_data), "no RT")

  a_model$conds <- c("null")
  temp_data <- data.frame(RT = 1, Error = -1, Cond = "null")
  expect_error(set_obs_data(a_model, temp_data), "only contain 0s and 1s")
  temp_data <- data.frame(RT = -1, Error = 1, Cond = "null")
  expect_error(set_obs_data(a_model, temp_data), "not >= 0")
})

test_that("setting model component functions work as expected", {
  my_prms <- c("a" = 2, "b" = 3, "cd" = 4)
  conds <- c("null")
  a_model <- drift_dm(prms_model = my_prms, conds = conds)

  mu <- function(drift_dm_obj, t_vec, one_cond) {
    rnorm(1)
  }
  a_model <- set_mu_fun(a_model, mu)
  mu_int <- function(drift_dm_obj, t_vec, one_cond) {
    rnorm(2)
  }
  a_model <- set_mu_int_fun(a_model, mu_int)
  x <- function(drift_dm_obj, x_vec, one_cond) {
    rnorm(3)
  }
  a_model <- set_x_fun(a_model, x)
  b <- function(drift_dm_obj, t_vec, one_cond) {
    rnorm(4)
  }
  a_model <- set_b_fun(a_model, b)
  dt_b <- function(drift_dm_obj, t_vec, one_cond) {
    rnorm(5)
  }
  a_model <- set_dt_b_fun(a_model, dt_b)
  nt <- function(drift_dm_obj, t_vec, one_cond) {
    rnorm(6)
  }
  a_model <- set_nt_fun(a_model, nt)

  expect_identical(a_model$comp_funs$mu_fun, mu)
  expect_identical(a_model$comp_funs$mu_int_fun, mu_int)
  expect_identical(a_model$comp_funs$b_fun, b)
  expect_identical(a_model$comp_funs$dt_b_fun, dt_b)
  expect_identical(a_model$comp_funs$x, x)
  expect_identical(a_model$comp_funs$nt, nt)

  # input checks
  expect_error(set_fun("bla", b, "mu", "t"), "not of type drift_dm")
  expect_error(set_fun(a_model, "bla", "mu", "t"), "*_fun argument")

  mu <- function(foo, t_vec, one_cond) {
    rnorm(1)
  }
  expect_error(set_fun(a_model, mu, "mu", "t"), "first argument")
  mu <- function(drift_dm_obj, foo, one_cond) {
    rnorm(1)
  }
  expect_error(set_fun(a_model, mu, "mu", "t"), "second argument")
  mu <- function(drift_dm_obj, t_vec, foo) {
    rnorm(1)
  }
  expect_error(set_fun(a_model, mu, "mu", "t"), "third argument")

  x <- function(drift_dm_obj, t_vec, one_cond) {
    rnorm(1)
  }
  expect_error(set_fun(a_model, x, "x", "x"), "second argument")
})


test_that("simulate_trace works as expected", {
  dt <- .01
  t_max <- 1

  # standard behavior
  my_prms <- c("a" = 2, "b" = 3, "cd" = 4)
  conds <- c("null")
  a_model <- drift_dm(
    prms_model = my_prms, conds = conds,
    t_max = t_max, dt = dt
  )
  a_model$prms_solve[["sigma"]] <- 0

  out <- simulate_trace(drift_dm_obj = a_model, k = 2, one_cond = "null")
  expect_identical(out[1, ], out[2, ])
  expect_equal(dim(out), c(2, 1 / .01 + 1))

  one_trace <- na.omit(out[1, ])
  expect_equal(
    length(one_trace),
    ceiling(standard_boundary() / standard_drift() / .01) + 1
  )

  # with noise and seed
  a_model$prms_solve[["sigma"]] <- 1.5
  out <- simulate_trace(
    drift_dm_obj = a_model, k = 1, one_cond = "null",
    seed = 1, add_x = T
  )
  expect_true(!is.matrix(out))

  withr::local_seed(1)
  # draw from pdf to ensure the same rng behvaior
  xx <- seq(-1, 1, length.out = a_model$prms_solve[["nx"]] + 1)
  pdf_x <- dunif(xx)
  samp_x <- draw_from_pdf(a_pdf = pdf_x, x_def = xx, k = 1)

  # now do the actual trace simulation as in Ulrich et al. Appendix C
  t <- seq(dt, t_max, dt)
  mu <- standard_drift()
  dX <- mu * dt + 1.5 * sqrt(dt) * rnorm(length(t)) # see
  X <- c(0, cumsum(dX))
  X <- X[1:min(which(X > standard_boundary()))]
  expect_identical(out[!is.na(out)], X)

  # expected errors
  expect_error(simulate_trace(
    drift_dm_obj = a_model, k = 1, one_cond = "null",
    seed = c(1, 2, 3)
  ), "seed must be a single numeric")

  expect_error(simulate_trace(
    drift_dm_obj = a_model, k = 1, one_cond = "null",
    add_x = NULL
  ), "logical")

  expect_error(
    simulate_trace(drift_dm_obj = a_model, k = 1, one_cond = "foo"),
    "not in the model's conds"
  )
  expect_error(
    simulate_trace(drift_dm_obj = a_model, k = 1, one_cond = 1),
    "must be a character"
  )
  expect_error(
    simulate_trace(
      drift_dm_obj = a_model, k = list(1),
      one_cond = 1
    ),
    "must be a numeric > 0"
  )
  expect_error(
    simulate_trace(drift_dm_obj = "hallo", k = 1, one_cond = "null"),
    "not of type drift_dm"
  )

  # no boundary hit
  a_model <- set_solver_settings(a_model, "t_max", 0.13)
  a_model$prms_solve[["sigma"]] <- 0
  expect_warning(
    simulate_trace(
      drift_dm_obj = a_model, k = 1,
      one_cond = "null"
    ),
    "no boundary hit"
  )
})


test_that("simualte_data works as expected", {
  # standard behavior
  my_prms <- c("a" = 2, "b" = 3, "cd" = 4)
  conds <- c("null")
  a_model <- drift_dm(
    prms_model = my_prms, conds = conds, dx = .005,
    dt = .001, t_max = 1.5
  )
  sim_data <- simulate_data(a_model, 100000, seed = 1)
  check_raw_data(sim_data)

  # correct quantiles
  sim_quantiles_corr <- quantile(sim_data$RT[sim_data$Error == 0],
    probs = seq(0.1, 0.9, 0.1)
  )
  exp_quantiles <- calc_quantiles(a_model, type = "pred")
  exp_quantiles_corr <- round(exp_quantiles$Quant_Corr, 3)
  expect_true(all(abs(sim_quantiles_corr - exp_quantiles_corr) <= 1))

  # error quantiles
  sim_quantiles_err <- quantile(sim_data$RT[sim_data$Error == 1],
    probs = seq(0.1, 0.9, 0.1)
  )
  exp_quantiles_err <- round(exp_quantiles$Quant_Err, 3)
  expect_true(all(abs(exp_quantiles_err - exp_quantiles_err) <= 1))

  # basic diffusion model has symmetric pdfs
  expect_true(all(
    abs(exp_quantiles$Quant_Corr * 1000 - exp_quantiles$Quant_Err * 1000) <=
      drift_dm_small_approx_error()
  ))

  # input checks
  expect_error(simulate_data(a_model, -1), "> 0")
  expect_error(simulate_data("hello", 1), "not of type drift_dm")
  expect_error(simulate_data(a_model, 10, c("1", "2")), "single numeric")
  expect_error(simulate_data(a_model, 10, c(1, 2)), "single numeric")
})




test_that("set_obs_data works as expected", {
  withr::local_seed(1)
  rts <- rnorm(10, mean = 0.3, sd = 0.02)
  err <- c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1)
  conds <- c("c", "i", "c", "i", "c", "i", "i", "c", "i", "c")
  data <- data.frame(
    RT = rts,
    Error = err,
    Cond = conds
  )
  model_prms <- c("a" = 1)
  model_conds <- c("c", "i")
  a_model <- drift_dm(model_prms, model_conds)
  a_model <- set_obs_data(drift_dm_obj = a_model, obs_data = data)

  expect_identical(
    a_model$obs_data$rts_corr$c,
    rts[err == 0 & conds == "c"]
  )
  expect_identical(
    a_model$obs_data$rts_corr$i,
    rts[err == 0 & conds == "i"]
  )
  expect_identical(
    a_model$obs_data$rts_err$c,
    rts[err == 1 & conds == "c"]
  )
  expect_identical(
    a_model$obs_data$rts_err$i,
    rts[err == 1 & conds == "i"]
  )
})
