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

  # create failing cases
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
  def_mu <- mu(a_model, t_vec, one_cond = "WAYNE")
  def_mu_int <- mu_int(a_model, t_vec, one_cond = "WAYNE")
  def_b <- b(a_model, t_vec, one_cond = "WAYNE")
  def_dtb <- dt_b(a_model, t_vec, one_cond = "WAYNE")
  def_nt <- nt(a_model, t_vec, one_cond = "WAYNE")
  def_x <- x(a_model, x_vec, one_cond = "WAYNE")

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
})


test_that("AIC and BIC calculation works as expected", {
  aic_bic <- get_ic(ll = 2, k = 3, n = 300)
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
  expect_identical(aic_bic[["BIC"]],
                   3 * log(nrow(ratcliff_data)) - 2 * a_model$log_like_val)

  expect_error(re_evaluate_model(NULL), "drift_dm_obj")
})


test_that("set_model_prms works as expected", {

  # before evaluating
  my_prms <- c("a" = 2, "b" = 3, "c" = 4)
  conds <- c("null")
  a_model <- drift_dm(prms_model = my_prms, conds = conds,
                      sigma = 1, t_max = 1, dt = .01, dx = .01,
                      obs_data = ratcliff_data
  )
  a_model = set_model_prms(a_model, c(1,2,3), eval_model = F)
  expect_null(a_model$log_like_val)
  expect_null(a_model$ic_vals)
  expect_identical(unname(a_model$prms_model), c(1,2,3))

  # after re-evaluating
  a_model = set_model_prms(a_model, c(1,2,3), eval_model = T)
  expect_true(!is.null(a_model$log_like_val))

  # errors and with fewer free_prms
  my_prms <- c("a" = 2, "b" = 3, "c" = 4)
  conds <- c("null")
  a_model <- drift_dm(prms_model = my_prms, conds = conds,
                      free_prms = c("a", "c"), sigma = 1, t_max = 1, dt = .01,
                      dx = .01
  )
  a_model = set_model_prms(a_model, c(8,4), eval_model = F)
  expect_identical(unname(a_model$prms_model), c(8,3,4))
  a_model$log_like_val = 23
  a_model = set_model_prms(a_model, c(8,4), eval_model = T)
  expect_null(a_model$log_like_val)
  expect_null(a_model$ic_vals)

  expect_error(set_model_prms(a_model, NULL, eval_model = F),
               "new_model_prms are not of type numeric")
  expect_error(set_model_prms(a_model, c(1), eval_model = F),
               "new_prms don't match the number of free_prms")
  expect_error(set_model_prms("hallo", c(1,3,4), eval_model = F),
               "drift_dm_obj")
  expect_error(set_model_prms(a_model, c(1,3,4), eval_model = NULL),
               "eval_model")
})
