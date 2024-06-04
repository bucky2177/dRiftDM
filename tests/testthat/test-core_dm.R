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
  some_data <- ratcliff_synth_data
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

  rts_u <- list(null = some_data$RT[some_data$Error == 0])
  rts_l <- list(null = some_data$RT[some_data$Error == 1])
  exp_data <- list(rts_u = rts_u, rts_l = rts_l)
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
  some_data <- ratcliff_synth_data
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
    "the third argument of mu_fun must be 't_vec'"
  )

  temp <- a_model
  temp$comp_funs$x_fun <- mu_constant
  expect_error(
    validate_drift_dm(temp),
    "the third argument of x_fun must be 'x_vec'"
  )


  temp <- a_model
  temp$comp_funs$x_fun <- function(foo, prms_solve, x_vec, one_cond, ddm_opts) {
    return(NULL)
  }
  expect_error(
    validate_drift_dm(temp),
    "the first argument of x_fun must be 'prms_model'"
  )

  temp <- a_model
  temp$comp_funs$x_fun <- function(prms_model, foo, x_vec, one_cond, ddm_opts) {
    return(NULL)
  }
  expect_error(
    validate_drift_dm(temp),
    "the second argument of x_fun must be 'prms_solve'"
  )

  temp <- a_model
  temp$comp_funs$x_fun <- function(prms_model, prms_solve, x_vec, foo, ddm_opts) {
    return(NULL)
  }
  expect_error(
    validate_drift_dm(temp),
    "the fourth argument of x_fun must be 'one_cond'"
  )

  temp <- a_model
  temp$comp_funs$x_fun <- function(prms_model, prms_solve, x_vec, one_cond, foo) {
    return(NULL)
  }
  expect_error(
    validate_drift_dm(temp),
    "the fifth argument of x_fun must be 'ddm_opts'"
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
  def_mu <- a_model$comp_funs$mu_fun(a_model$prms_model,
    a_model$prms_solve,
    t_vec,
    one_cond = "W",
    a_model$ddm_opts
  )
  def_mu_int <- a_model$comp_funs$mu_int_fun(a_model$prms_model,
    a_model$prms_solve,
    t_vec,
    one_cond = "W",
    a_model$ddm_opts
  )
  def_b <- a_model$comp_funs$b_fun(a_model$prms_model,
    a_model$prms_solve,
    t_vec,
    one_cond = "W",
    a_model$ddm_opts
  )
  def_dtb <- a_model$comp_funs$dt_b_fun(a_model$prms_model,
    a_model$prms_solve,
    t_vec,
    one_cond = "W",
    a_model$ddm_opts
  )
  def_nt <- a_model$comp_funs$nt_fun(a_model$prms_model,
    a_model$prms_solve,
    t_vec,
    one_cond = "W",
    a_model$ddm_opts
  )
  def_x <- a_model$comp_funs$x_fun(a_model$prms_model,
    a_model$prms_solve,
    x_vec,
    one_cond = "W",
    a_model$ddm_opts
  )

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
  x_vec <- numeric()
  expect_error(
    a_model$comp_funs$mu_fun(a_model$prms_model,
      a_model$prms_solve,
      t_vec,
      one_cond = "W",
      a_model$ddm_opts
    ),
    "t_vec is not a vector"
  )
  expect_error(
    a_model$comp_funs$mu_int_fun(a_model$prms_model,
      a_model$prms_solve,
      t_vec,
      one_cond = "W",
      a_model$ddm_opts
    ),
    "t_vec is not a vector"
  )
  expect_error(
    a_model$comp_funs$b_fun(a_model$prms_model,
      a_model$prms_solve,
      t_vec,
      one_cond = "W",
      a_model$ddm_opts
    ),
    "t_vec is not a vector"
  )
  expect_error(
    a_model$comp_funs$dt_b_fun(a_model$prms_model,
      a_model$prms_solve,
      t_vec,
      one_cond = "W",
      a_model$ddm_opts
    ),
    "t_vec is not a vector"
  )
  expect_error(
    a_model$comp_funs$nt_fun(a_model$prms_model,
      a_model$prms_solve,
      t_vec,
      one_cond = "W",
      a_model$ddm_opts
    ),
    "t_vec is not a vector"
  )
  expect_error(
    a_model$comp_funs$x_fun(a_model$prms_model,
      a_model$prms_solve,
      x_vec,
      one_cond = "W",
      a_model$ddm_opts
    ),
    "x_vec is not a vector"
  )
})


test_that("AIC and BIC calculation works as expected", {
  aic_bic <- calc_ic(ll = 2, k = 3, n = 300)
  expect_identical(aic_bic[["aic"]], 2 * 3 - 2 * 2)
  expect_identical(aic_bic[["bic"]], 3 * log(300) - 2 * 2)
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

  a_model <- set_obs_data(a_model, ratcliff_synth_data, eval_model = T)
  log_like_val <- a_model$log_like_val
  expect_true(!is.null(a_model$log_like_val))
  aic_bic <- a_model$ic_vals
  expect_identical(aic_bic[["aic"]], 2 * 3 - 2 * a_model$log_like_val)
  expect_identical(
    aic_bic[["bic"]],
    3 * log(nrow(ratcliff_synth_data)) - 2 * a_model$log_like_val
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
    obs_data = ratcliff_synth_data
  )
  a_model <- set_model_prms(a_model, c(b = 2, a = 1, c = 3), eval_model = F)
  expect_null(a_model$log_like_val)
  expect_null(a_model$ic_vals)
  expect_identical(unname(a_model$prms_model), c(1, 2, 3))

  # after re-evaluating
  a_model <- set_model_prms(a_model, c(c = 3, b = 2, a = 1), eval_model = T)
  expect_true(!is.null(a_model$log_like_val))

  # errors and with unmatching prms
  my_prms <- c("a" = 2, "b" = 3, "c" = 4)
  conds <- c("null")
  a_model <- drift_dm(
    prms_model = my_prms, conds = conds,
    free_prms = c("a", "c"), sigma = 1, t_max = 1, dt = .01,
    dx = .01
  )
  a_model <- set_model_prms(a_model, c(a = 8, c = 4), eval_model = F)
  expect_identical(unname(a_model$prms_model), c(8, 3, 4))
  a_model$log_like_val <- 23
  a_model <- set_model_prms(a_model, c(a = 8, c = 4), eval_model = T)
  expect_null(a_model$log_like_val)
  expect_null(a_model$ic_vals)

  expect_error(
    set_model_prms(a_model, NULL, eval_model = F),
    "numeric vector"
  )
  expect_error(
    set_model_prms(a_model, c(1), eval_model = F),
    "is a named vector"
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
    obs_data = ratcliff_synth_data
  )
  a_model <- set_free_prms(a_model, c("b", "a"))
  expect_identical(a_model$free_prms, c("a", "b"))

  expect_warning(set_free_prms(a_model, c("c", "a")), "corrected")
  a_model <- suppressWarnings(set_free_prms(a_model, c("c", "a")))
  expect_identical(a_model$free_prms, c("a", "cd"))

  a_model <- set_free_prms(a_model, new_fixed_prms = c("b"))
  expect_identical(a_model$free_prms, c("a", "cd"))

  expect_error(set_free_prms(a_model, c("x", "a")), "arg")

  expect_error(
    set_free_prms("hallo", c("b", "a")),
    "drift_dm_obj"
  )

  expect_error(set_free_prms(a_model, NULL, NULL), "Neither")
  expect_error(set_free_prms(a_model, NULL, 1), "character")
  expect_error(set_free_prms(a_model, c(1, 2), NULL), "character")
})


test_that("set_solver_settings works as expected", {
  # before evaluating
  my_prms <- c("a" = 2, "b" = 3, "cd" = 4)
  conds <- c("null")
  a_model <- drift_dm(
    prms_model = my_prms, conds = conds,
    sigma = 1, t_max = 1, dt = .01, dx = .01,
    obs_data = ratcliff_synth_data
  )
  a_model <- set_solver_settings(a_model, c(
    t_max = 3, dt = .1, dx = .1,
    sigma = 2
  ), eval_model = F)
  expect_identical(
    a_model$prms_solve,
    c(sigma = 2, t_max = 3, dt = .1, dx = .1, nt = 30, nx = 20)
  )
  a_model <- set_solver_settings(a_model, c(solver = "bla"),
    eval_model = F
  )
  expect_identical(a_model$solver, "bla")


  # errors and warnings
  expect_error(
    set_solver_settings(a_model, c(solver = "bla"),
      eval_model = T
    ),
    "not implemented yet"
  )
  a_model$solver <- "kfe"
  a_model <- suppressWarnings(
    set_solver_settings(a_model, new_solver_vals = c(dx = .5, dt = .5))
  )
  a_model$obs_data <- NULL
  expect_warning(re_evaluate_model(a_model), "negative density values")
  a_model <- set_solver_settings(a_model, new_solver_vals = c(dx = .1, dt = .1))

  # further errors
  expect_error(
    set_solver_settings(a_model, c("t_max", "dx")),
    "must be a named vector"
  )
  expect_warning(
    set_solver_settings(a_model, c(sol = 2)),
    "Automatically corrected"
  )
  expect_error(set_solver_settings(
    a_model, c(sol = NULL),
    "not of type character"
  ))
  expect_error(
    set_solver_settings(a_model, c(t_max = list(4))),
    "type numeric or character"
  )
  expect_error(
    set_solver_settings("hallo", "t_max"),
    "drift_dm_obj"
  )

  expect_error(
    set_solver_settings(a_model, c(t_max = 3, 3)),
    "'arg' should be one of"
  )
})




test_that("set_obs_data and check_raw_data throw expected errors", {
  my_prms <- c("a" = 2, "b" = 3, "cd" = 4)
  conds <- c("null")
  a_model <- drift_dm(prms_model = my_prms, conds = conds)

  # wrong inputs to set_obs_data
  expect_error(set_obs_data("uff", ratcliff_synth_data), "not of type drift_dm")
  temp_data <- ratcliff_synth_data
  temp_data <- rbind(temp_data, data.frame(RT = 0.4, Error = 1, Cond = "foo"))
  expect_warning(
    set_obs_data(a_model, temp_data),
    "not listed in the model's conditions"
  )
  a_model$conds <- c("null", "foo")
  expect_error(
    set_obs_data(a_model, ratcliff_synth_data),
    "not part of the Cond column"
  )

  a_model$conds <- c("null")
  expect_error(set_obs_data(a_model, list()), "not a data frame")

  temp_data <- data.frame(RT = 1, Error = "0", Cond = "null")
  expect_warning(set_obs_data(a_model, temp_data), "type numeric")

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
  expect_error(set_obs_data(a_model, temp_data), "only contain 0 and 1")
  temp_data <- data.frame(RT = -1, Error = 1, Cond = "null")
  expect_error(set_obs_data(a_model, temp_data), "not >= 0")
})

test_that("set_b_encoding works as expected", {
  # model with other b_encoding
  a_model <- dmc_dm(dt = .005, dx = .005)
  a_model <- set_b_encoding(
    drift_dm_obj = a_model,
    b_encoding = list(
      column = "Test",
      u_name_value = c("foo" = "a"),
      l_name_value = c("bar" = "c")
    )
  )
  some_data <- data.frame(
    RT = c(0.1, 0.2, 0.3, 0.4),
    Test = c("a", "c", "c", "a"),
    Cond = c("comp", "incomp", "comp", "incomp")
  )
  a_model <- set_obs_data(a_model, obs_data = some_data)
  a_model <- re_evaluate_model(a_model)

  # model with default encoding
  b_model <- dmc_dm(dt = .005, dx = .005)
  some_data <- data.frame(
    RT = c(0.1, 0.2, 0.3, 0.4),
    Error = c(0, 1, 1, 0),
    Cond = c("comp", "incomp", "comp", "incomp")
  )
  b_model <- set_obs_data(b_model, obs_data = some_data)
  b_model <- re_evaluate_model(b_model)

  expect_identical(unlist(a_model), unlist(b_model))

  # some stats with other b_encoding
  some_data <- dmc_synth_data
  colnames(some_data)[2] <- "Test"
  some_data$Test <- ifelse(some_data$Test == 0, "a", "c")
  a_model <- set_obs_data(a_model, some_data)
  stats1 <- calc_stats(a_model, type = c("quantiles", "cafs"))
  expect_equal(colnames(stats1$quantiles)[4:5], c("Quant_foo", "Quant_bar"))
  expect_equal(colnames(stats1$cafs)[4], c("P_foo"))

  # some stats with default encoding
  some_data <- dmc_synth_data
  b_model <- set_obs_data(b_model, some_data)
  stats2 <- calc_stats(b_model, type = c("quantiles", "cafs"))
  expect_equal(stats2$quantiles$Quant_corr, stats1$quantiles$Quant_foo)
  expect_equal(stats2$quantiles$Quant_err, stats1$quantiles$Quant_bar)
  expect_equal(stats2$cafs$P_corr, stats1$cafs$P_foo)
})

test_that("set_b_encoding errs as expected", {
  a_model <- dmc_dm()

  expect_error(
    set_b_encoding(drift_dm_obj = NULL), "not of type drift_dm"
  )

  expect_error(
    set_b_encoding(drift_dm_obj = a_model, b_encoding = "a"), "not a list"
  )

  expect_error(
    set_b_encoding(
      drift_dm_obj = a_model,
      b_encoding = list(
        column = "Error",
        u_name_value = c("foo" = 1),
        l_name_value = c("bar" = 1),
        test = 3
      )
    ),
    "unexpected entries"
  )

  expect_error(
    set_b_encoding(
      drift_dm_obj = a_model,
      b_encoding = list(
        column = "Error",
        bla = c("foo" = 1),
        l_name_value = c("bar" = 1)
      )
    ),
    "unexpected entries"
  )

  expect_error(
    set_b_encoding(
      drift_dm_obj = a_model,
      b_encoding = list(
        column = "Error",
        u_name_value = c("foo" = "1"),
        l_name_value = c("bar" = 1)
      )
    ),
    "not of the same type"
  )

  expect_error(
    set_b_encoding(
      drift_dm_obj = a_model,
      b_encoding = list(
        column = list(123),
        u_name_value = c("foo" = 1, "bla" = 3),
        l_name_value = c("bar" = 1)
      )
    ),
    "not a single character"
  )


  expect_error(
    set_b_encoding(
      drift_dm_obj = a_model,
      b_encoding = list(
        column = c("foo", "bar"),
        u_name_value = c("foo" = 1, "bla" = 3),
        l_name_value = c("bar" = 1)
      )
    ),
    "not a single character"
  )

  expect_error(
    set_b_encoding(
      drift_dm_obj = a_model,
      b_encoding = list(
        column = "Error",
        u_name_value = c(1),
        l_name_value = c("bar" = 1)
      )
    ),
    "not a named vector"
  )

  expect_error(
    set_b_encoding(
      drift_dm_obj = a_model,
      b_encoding = list(
        column = "Error",
        u_name_value = c("foo" = 1),
        l_name_value = c(1)
      )
    ),
    "not a named vector"
  )
})

test_that("setting model component functions work as expected", {
  my_prms <- c("a" = 2, "b" = 3, "cd" = 4)
  conds <- c("null")
  a_model <- drift_dm(prms_model = my_prms, conds = conds)

  mu <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
    rnorm(1)
  }
  a_model <- set_comp_funs(a_model, list(mu_fun = mu))
  mu_int <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
    rnorm(2)
  }
  a_model <- set_comp_funs(a_model, list(mu_int_fun = mu_int))
  x <- function(prms_model, prms_solve, x_vec, one_cond, ddm_opts) {
    rnorm(3)
  }
  a_model <- set_comp_funs(a_model, list(x_fun = x))
  b <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
    rnorm(4)
  }
  dt_b <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
    rnorm(5)
  }
  a_model <- set_comp_funs(a_model, list(b_fun = b, dt_b_fun = dt_b))
  nt <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
    rnorm(6)
  }
  a_model <- set_comp_funs(a_model, list(nt_fun = nt))

  expect_identical(a_model$comp_funs$mu_fun, mu)
  expect_identical(a_model$comp_funs$mu_int_fun, mu_int)
  expect_identical(a_model$comp_funs$b_fun, b)
  expect_identical(a_model$comp_funs$dt_b_fun, dt_b)
  expect_identical(a_model$comp_funs$x, x)
  expect_identical(a_model$comp_funs$nt, nt)

  # input checks
  expect_error(set_comp_funs("bla", b), "not of type drift_dm")
  expect_error(set_comp_funs(a_model, "bla"), "not a list")
  expect_error(set_comp_funs(a_model, list()), "empty")
  expect_error(set_comp_funs(a_model, list(b)), "not named")
})


test_that("simulate_traces works as expected", {
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

  out <- simulate_traces(drift_dm_obj = a_model, k = 2, conds = "null")
  expect_identical(out[1, ], out[2, ])
  expect_equal(dim(out), c(2, 1 / .01 + 1))

  one_trace <- na.omit(out[1, ])
  expect_equal(
    length(one_trace),
    ceiling(standard_boundary() / standard_drift() / .01) + 1
  )

  # with noise and seed
  a_model$prms_solve[["sigma"]] <- 1.5
  out <- simulate_traces(
    drift_dm_obj = a_model, k = 1, conds = "null",
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
  expect_error(simulate_traces(
    drift_dm_obj = a_model, k = 1, conds = "null",
    seed = c(1, 2, 3)
  ), "seed must be a single numeric")

  expect_error(simulate_traces(
    drift_dm_obj = a_model, k = 1, conds = "null",
    add_x = NULL
  ), "logical")

  expect_error(
    simulate_traces(drift_dm_obj = a_model, k = 1, conds = "foo"),
    "not in the model's conds"
  )
  expect_error(
    simulate_traces(drift_dm_obj = a_model, k = 1, conds = 1),
    "must be a character"
  )
  expect_error(
    simulate_traces(
      drift_dm_obj = a_model, k = list(1),
      conds = 1
    ),
    "must be a numeric > 0"
  )
  expect_error(
    simulate_traces(drift_dm_obj = "hallo", k = 1, conds = "null"),
    "not of type drift_dm"
  )

  # no boundary hit
  a_model <- set_solver_settings(a_model, c(t_max = 0.13))
  a_model$prms_solve[["sigma"]] <- 0
  expect_warning(
    simulate_traces(
      drift_dm_obj = a_model, k = 1,
      conds = "null"
    ),
    "no boundary hit"
  )


  # call with multiple conds
  a_model$prms_solve[["sigma"]] <- 1
  a_model <- set_solver_settings(a_model, c(t_max = 1))
  a_model$conds <- c("foo", "bar")
  set_one <- simulate_traces(
    drift_dm_obj = a_model, k = 1,
    conds = NULL
  )
  expect_equal(names(set_one), c("foo", "bar"))


  set_two <- simulate_traces(
    drift_dm_obj = a_model, k = 1,
    conds = c("foo", "bar")
  )
  expect_equal(names(set_two), c("foo", "bar"))
})


test_that("simualte_data works as expected", {
  # standard behavior
  my_prms <- c("a" = 2, "b" = 3, "cd" = 4)
  conds <- c("null")
  a_model <- drift_dm(
    prms_model = my_prms, conds = conds, dx = .005,
    dt = .001, t_max = 1.5
  )
  a_model <- set_b_encoding(a_model, b_encoding = list(
    column = "col",
    u_name_value = c(a = 0),
    l_name_value = c(b = -1)
  ))
  sim_data <- simulate_data(a_model, 100000, seed = 1)
  check_raw_data(sim_data,
    b_encoding_column = "col", u_name_value = 0,
    l_name_value = -1
  )

  # correct quantiles
  sim_quantiles_a <- quantile(sim_data$RT[sim_data$col == 0],
    probs = seq(0.1, 0.9, 0.1)
  )
  exp_quantiles <- calc_stats(a_model, type = "quantiles", source = "pred")
  exp_quantiles_a <- round(exp_quantiles$Quant_a, 3)
  expect_true(all(abs(sim_quantiles_a - exp_quantiles_a) <= 1))

  # error quantiles
  sim_quantiles_b <- quantile(sim_data$RT[sim_data$col == -1],
    probs = seq(0.1, 0.9, 0.1)
  )
  exp_quantiles_b <- round(exp_quantiles$Quant_b, 3)
  expect_true(all(abs(exp_quantiles_b - exp_quantiles_b) <= 1))

  # basic diffusion model has symmetric pdfs
  expect_true(all(
    abs(exp_quantiles$Quant_a * 1000 - exp_quantiles$Quant_b * 1000) <=
      drift_dm_small_approx_error()
  ))

  # input checks
  expect_error(simulate_data(a_model, -1), "> 0")
  expect_error(simulate_data("hello", 1), "not of type drift_dm")
  expect_error(simulate_data(a_model, 10, seed = c("1", "2")), "single numeric")
  expect_error(simulate_data(a_model, 10, seed = c(1, 2)), "single numeric")
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
    a_model$obs_data$rts_u$c,
    rts[err == 0 & conds == "c"]
  )
  expect_identical(
    a_model$obs_data$rts_u$i,
    rts[err == 0 & conds == "i"]
  )
  expect_identical(
    a_model$obs_data$rts_l$c,
    rts[err == 1 & conds == "c"]
  )
  expect_identical(
    a_model$obs_data$rts_l$i,
    rts[err == 1 & conds == "i"]
  )
})
