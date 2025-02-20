# DRIFT_DM AND DEFAULT COMPONENTS -----------------------------------------



test_that("creating a drift_dm object", {
  # no data
  my_prms <- c("a" = 2, "b" = 3, "c" = 4)
  conds <- c("a")
  a_model <- drift_dm(prms_model = my_prms, conds = conds, subclass = "test")
  expect_identical(
    a_model$flex_prms_obj,
    flex_prms(my_prms, conds = conds)
  )
  expect_identical(a_model$prms_solve, default_solver_prms())
  expect_identical(a_model$solver, default_solver())
  expect_identical(class(a_model), c("test", "drift_dm"))


  # with data
  some_data <- ratcliff_synth_data
  my_prms <- c("a" = 2, "b" = 3, "c" = 4)
  conds <- c("null")
  a_model <- drift_dm(
    prms_model = my_prms, conds = conds, subclass = "test",
    obs_data = some_data, sigma = 2,
    t_max = 4, dt = .01, dx = .05
  )
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

test_that("input checks dm_drift", {
  # create failing cases
  my_prms <- c("a" = 2, "b" = 3, "c" = 4)
  conds <- c("null")
  expect_error(drift_dm(numeric(), conds, "test"), "prms_model")
  expect_error(drift_dm(my_prms, NULL, "test"), "conds")
  expect_error(drift_dm(my_prms, conds, "test", sigma = -1), regexp = "sigma")
  expect_error(drift_dm(my_prms, conds, "test", t_max = -1), regexp = "t_max")
  expect_error(drift_dm(my_prms, conds, "test", dx = -1), regexp = "dx")
  expect_error(drift_dm(my_prms, conds, "test", dt = -1), regexp = "dt")
  expect_error(drift_dm(my_prms, conds, "test", solver = "foo"), regexp = "solver")
})

test_that("validate_model fails as expected", {
  my_prms <- c("a" = 2, "b" = 3, "c" = 4)
  conds <- c("null")
  a_model <- drift_dm(prms_model = my_prms, conds = conds, "test")

  temp <- a_model
  temp$flex_prms_obj <- c()
  expect_error(validate_drift_dm(temp), "flex_prms")


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
  obs_data(temp) <- some_data
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

  temp <- a_model
  temp$comp_funs$special_fun <- x_beta
  expect_error(
    validate_drift_dm(temp),
    "unexpected entry in comp_funs"
  )

  temp <- a_model
  temp$comp_funs$mu_fun <- NULL
  expect_error(
    validate_drift_dm(temp),
    "some comp_funs are missing"
  )


  temp <- a_model
  temp$comp_funs$mu_fun <- "a"
  expect_error(
    validate_drift_dm(temp),
    "mu_fun listed in comp_funs is not a function"
  )


  # check pdf labeling and length
  a_model <- dmc_dm(dx = .01, dt = .005)
  a_model <- re_evaluate_model(a_model)

  temp <- a_model
  temp$pdfs$bla <- temp$pdfs$comp
  expect_error(
    validate_drift_dm(temp),
    "not labeled like the conditions"
  )

  temp <- a_model
  names(temp$pdfs) <- c("bla", "foo")
  expect_error(
    validate_drift_dm(temp),
    "not labeled like the conditions"
  )

  temp <- a_model
  names(temp$pdfs$comp) <- c("pdf_l", "pdf_asd")
  expect_error(
    validate_drift_dm(temp),
    "not named pdf_u and pdf_l"
  )

  temp <- a_model
  temp$pdfs$comp$pdf_l <- temp$pdfs$comp$pdf_l[1:19]
  temp$pdfs$incomp$pdf_u <- temp$pdfs$incomp$pdf_u[1:19]
  expect_error(
    validate_drift_dm(temp),
    "one of the pdf vectors has not the expected size"
  )

  temp <- a_model
  temp$pdfs$comp$pdf_l <- as.character(temp$pdfs$comp$pdf_l)
  expect_error(
    validate_drift_dm(temp),
    "vectors is not of type numeric"
  )


  # check log_like
  temp <- a_model
  temp$log_like_val <- "ba"
  expect_error(
    validate_drift_dm(temp),
    "not a single numeric"
  )
  a_model$log_like_val <- c(1, 2)
  expect_error(
    validate_drift_dm(temp),
    "not a single numeric"
  )
})

test_that("standard components for the ddm components work as expected", {
  my_prms <- c("a" = 2, "b" = 3, "c" = 4)
  conds <- c("null")
  a_model <- drift_dm(
    prms_model = my_prms, conds = conds, subclass = "test",
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



# RE_EVALUATE -------------------------------------------------------------


test_that("re_evaluate_model works as expected", {
  my_prms <- c("a" = 2, "b" = 3, "c" = 4)
  conds <- c("null")
  a_model <- drift_dm(
    prms_model = my_prms, conds = conds, subclass = "foo",
    sigma = 1, t_max = 3, dt = .01, dx = .01
  )
  expect_null(a_model$log_like_val)

  obs_data(a_model, eval_model = T) <- ratcliff_synth_data
  log_like_val <- a_model$log_like_val
  expect_true(!is.null(a_model$log_like_val))

  expect_error(re_evaluate_model(NULL), "drift_dm_obj")
})


# REPLACEMENT AND EXTRACTOR FUNCTIONS --------------------------------------

test_that("flex_prms -> extractor and replacement works as expected", {
  my_prms <- c("a" = 2, "b" = 3, "c" = 4)
  conds <- c("null")
  a_model <- drift_dm(
    prms_model = my_prms, conds = conds, subclass = "foo",
    sigma = 1, t_max = 3, dt = .01, dx = .01
  )

  exp_flex_prms <- flex_prms(my_prms, conds = conds)
  expect_equal(flex_prms(a_model), exp_flex_prms)
  expect_equal(flex_prms(a_model$flex_prms_obj), exp_flex_prms)

  # replace old one
  new_flex_prms_obj <- flex_prms(c(a = 2, d = 4), conds = c("incomp", "comp"))
  flex_prms(a_model) <- new_flex_prms_obj
  expect_equal(new_flex_prms_obj, a_model$flex_prms_obj)
})

test_that("prms_solve -> extractor and replacement works as expected", {
  # before evaluating
  a_model <- readRDS(test_path("fixtures", "ratcliff.rds"))

  prms_solve(a_model) <- c(t_max = 3, dt = .1, dx = .1, sigma = 2)
  expect_identical(
    prms_solve(a_model),
    c(sigma = 2, t_max = 3, dt = .1, dx = .1, nt = 30, nx = 20)
  )


  suppressWarnings(
    prms_solve(a_model)[c("dx", "dt")] <- c(dx = .5, dt = .5)
  )
  expect_warning(re_evaluate_model(a_model), "negative density values")
  prms_solve(a_model)[c("dx", "dt")] <- c(dx = .1, dt = .1)

  # further errors
  expect_error(
    prms_solve(a_model) <- c(t_max = "2"),
    "must be a valid numeric"
  )
  expect_warning(
    prms_solve(a_model) <- c(1),
    "is not named"
  )
  expect_error(
    prms_solve(a_model)[7] <- c(foo = 1),
    "too many supplied values"
  )
  expect_error(
    prms_solve(a_model) <- c(t_max = 3, 3),
    "'arg' should be one of"
  )

  # extractor with drift_dm_fits
  all_fits <- load_fits_ids(
    path = test_path("fixtures"),
    fit_procedure_name = "test_case_saved"
  )
  expect_equal(
    prms_solve(all_fits),
    all_fits$drift_dm_fit_info$drift_dm_obj$prms_solve
  )
})

test_that("solver -> extractor and replacement works as expected", {
  a_model <- readRDS(test_path("fixtures", "ratcliff.rds"))

  expect_equal(solver(a_model), "kfe")
  expect_identical(
    solver(a_model) <- "im_zero",
    "im_zero"
  )

  expect_error(
    solver(a_model) <- "foo",
    "should be either"
  )

  expect_error(
    solver(a_model)[2] <- "foo",
    "must be of length 1"
  )

  expect_error(
    solver(a_model) <- 1,
    "character"
  )

  # extractor with drift_dm_fits
  all_fits <- load_fits_ids(
    path = test_path("fixtures"),
    fit_procedure_name = "test_case_saved"
  )
  expect_equal(
    solver(all_fits),
    all_fits$drift_dm_fit_info$drift_dm_obj$solver
  )
})

test_that("obs_data -> extractor and replacement works as expected", {
  a_model <- readRDS(test_path("fixtures", "ssp.rds"))

  # test extractor
  obs_data(a_model) <- dmc_synth_data[sample(1:nrow(dmc_synth_data)), ]
  expect_message(
    obs_data(a_model),
    "sorted differently"
  )
  extr_rts <- suppressMessages(obs_data(a_model)$RT)
  expect_equal(
    sort(extr_rts),
    sort(dmc_synth_data$RT)
  )

  # test with simulated data
  synth_data <- simulate_data(a_model, n = c(50, 100))
  obs_data(a_model) <- synth_data[sample(1:nrow(synth_data)), ]
  extr_data <- obs_data(a_model, messaging = F)
  expect_equal(table(extr_data), table(synth_data))

  # extractor with drift_dm_fits
  all_fits <- load_fits_ids(
    path = test_path("fixtures"),
    fit_procedure_name = "test_case_saved"
  )
  expect_equal(
    obs_data(all_fits),
    all_fits$drift_dm_fit_info$obs_data_ids
  )

  # wrong inputs to set_obs_data
  temp_data <- dmc_synth_data
  temp_data <- rbind(temp_data, data.frame(RT = 0.4, Error = 1, Cond = "foo"))
  expect_warning(
    obs_data(a_model) <- temp_data,
    "not listed in the model's conditions"
  )
  obs_data(a_model) <- NULL
  flex_prms(a_model) <- flex_prms(coef(a_model, select_unique = F)[1, ],
    conds = c("comp", "foo"),
    instr = "sign ~ foo == -(sign ~ comp)"
  )
  expect_error(
    obs_data(a_model) <- dmc_synth_data,
    "not part of the Cond column"
  )


  expect_error(
    obs_data(a_model) <- list(),
    "data.frame"
  )

  # now with ratcliff model for easier generation of test cases
  a_model <- ratcliff_dm()
  temp_data <- data.frame(RT = 1, Error = "0", Cond = "null")
  expect_warning(obs_data(a_model) <- temp_data, "type numeric")

  temp_data <- data.frame(RT = "1", Error = 1, Cond = "null")
  expect_warning(obs_data(a_model) <- temp_data, "not of type numeric")

  obs_data(a_model) <- NULL
  flex_prms(a_model) <- flex_prms(coef(a_model, select_unique = F)[1, ],
    conds = c("1")
  )
  temp_data <- data.frame(RT = 1, Error = 1, Cond = 1)
  expect_warning(obs_data(a_model) <- temp_data, "not of type character")

  temp_data <- data.frame(RT = 1, Error = 1)
  expect_error(obs_data(a_model) <- temp_data, "No Cond")

  obs_data(a_model) <- NULL
  flex_prms(a_model) <- flex_prms(coef(a_model, select_unique = F)[1, ],
    conds = c("null")
  )
  temp_data <- data.frame(RT = 1, Cond = "null")
  expect_error(obs_data(a_model) <- temp_data, "no Error")
  temp_data <- data.frame(Error = 1, Cond = "null")
  expect_error(obs_data(a_model) <- temp_data, "no RT")

  temp_data <- data.frame(RT = 1, Error = -1, Cond = "null")
  expect_error(obs_data(a_model) <- temp_data, "only contain 0 or 1")
  temp_data <- data.frame(RT = -1, Error = 1, Cond = "null")
  expect_error(obs_data(a_model) <- temp_data, "not >= 0")
})

test_that("comp_funs -> extractor and replacement functions work as expected", {
  my_prms <- c("a" = 2, "b" = 3, "cd" = 4)
  conds <- c("null")
  a_model <- drift_dm(prms_model = my_prms, conds = conds, "test")

  mu <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
    rnorm(1)
  }
  comp_funs(a_model)[["mu_fun"]] <- mu
  mu_int <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
    rnorm(2)
  }
  comp_funs(a_model)[["mu_int_fun"]] <- mu_int
  x <- function(prms_model, prms_solve, x_vec, one_cond, ddm_opts) {
    rnorm(3)
  }
  comp_funs(a_model)[["x_fun"]] <- x
  b <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
    rnorm(4)
  }
  dt_b <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
    rnorm(5)
  }
  comp_funs(a_model) <- list(b_fun = b, dt_b_fun = dt_b)
  nt <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
    rnorm(6)
  }
  comp_funs(a_model)["nt_fun"] <- list(nt_fun = nt)

  expect_identical(a_model$comp_funs$mu_fun, mu)
  expect_identical(a_model$comp_funs$mu_int_fun, mu_int)
  expect_identical(a_model$comp_funs$b_fun, b)
  expect_identical(a_model$comp_funs$dt_b_fun, dt_b)
  expect_identical(a_model$comp_funs$x, x)
  expect_identical(a_model$comp_funs$nt, nt)

  expect_identical(comp_funs(a_model)[["mu_fun"]], mu)
  expect_identical(comp_funs(a_model)[["mu_int_fun"]], mu_int)
  expect_identical(comp_funs(a_model)[["b_fun"]], b)
  expect_identical(comp_funs(a_model)[["dt_b_fun"]], dt_b)
  expect_identical(comp_funs(a_model)[["x_fun"]], x)
  expect_identical(comp_funs(a_model)[["nt_fun"]], nt)


  # input checks
  expect_error(
    comp_funs(a_model) <- "bla",
    "not a list"
  )
  expect_error(
    comp_funs(a_model) <- list(),
    "empty"
  )
  expect_error(
    comp_funs(a_model) <- list(b),
    "not named"
  )
  expect_error(
    comp_funs(a_model) <- list(ba = "a"),
    "should be one of"
  )
  expect_error(
    comp_funs(a_model)[["b_fun"]] <- "a",
    "not a function"
  )
})

test_that("b_coding -> extractor and replacement functions work as expected", {
  # check default b_coding and replacement drift_dm
  a_model <- readRDS(test_path("fixtures", "dmc.rds"))
  prms_solve(a_model)[c("dt", "dx")] <- c(.005, .005)
  expect_equal(b_coding(a_model), drift_dm_default_b_coding())

  new_coding <- list(
    column = "Test",
    u_name_value = c(foo = "a"),
    l_name_value = c(bar = "c")
  )
  b_coding(a_model) <- new_coding
  expect_equal(b_coding(a_model), new_coding)


  # check for fits_ids_dm
  all_fits <- load_fits_ids(test_path("fixtures"),
    fit_procedure_name = "test_case_saved"
  )

  expect_equal(b_coding(all_fits), drift_dm_default_b_coding())
})

test_that("coef<- works as expected", {
  a_model <- readRDS(test_path("fixtures", "ratcliff.rds"))

  coef(a_model) <- c(b = 1, muc = 5, non_dec = 0.4)
  expect_identical(coef(a_model), c(muc = 5, b = 1, non_dec = 0.4))

  # errors
  expect_error(
    coef(a_model) <- c(b = 8, c = 4),
    "unexpected number of entries"
  )

  expect_error(
    coef(a_model) <- c(b = 8, muc = 4, d = 2),
    "Check the names and values"
  )

  expect_error(
    coef(a_model) <- c(b = 8, muc = 4, non_dec = Inf),
    "Check the names and values"
  )

  expect_error(
    coef(a_model) <- c(b = 8, muc = 4, non_dec = "Inf"),
    "Check the names and values"
  )

  expect_error(
    coef(a_model, eval_model = list(1)) <- coef(a_model),
    "eval_model"
  )
})

test_that("conds -> extractor works as expected", {
  a_model <- readRDS(file = test_path("fixtures", "dmc.rds"))
  expect_equal(conds(a_model), c("comp", "incomp"))

  all_fits <- load_fits_ids(test_path("fixtures"),
    fit_procedure_name = "test_case_saved"
  )
  expect_equal(conds(all_fits), c("null"))

  some_stats <- calc_stats(all_fits, type = "cafs")
  expect_equal(conds(some_stats), c("null"))

  temp_data <- data.frame(RT = 1, Error = 0, Cond = c("null", "foo", "bar"))
  expect_equal(conds(temp_data), c("null", "foo", "bar"))

  some_traces <- simulate_traces(a_model, k = 1)
  expect_equal(conds(some_traces), c("comp", "incomp"))
})


# HELPER FUNCTIONS --------------------------------------------------------

test_that("check_raw_data specific input checks", {
  def_b_coding <- drift_dm_default_b_coding()

  temp_data <- data.frame(ID = c(1, NA), RT = 1, Error = 0, Cond = "null")
  expect_warning(
    check_raw_data(
      temp_data, def_b_coding$column, def_b_coding$u_name_value,
      def_b_coding$l_name_value
    ),
    "missing values"
  )

  temp_data <- data.frame(ID = 1, RT = 1, Error = 0, Cond = "null")
  expect_error(
    check_raw_data(
      temp_data, def_b_coding$column, def_b_coding$u_name_value,
      as.character(def_b_coding$l_name_value)
    ),
    "must be of the same type"
  )

  temp_data <- data.frame(ID = c(1, 2), RT = 1, Error = 0, Cond = "null")
  temp_data$ID <- factor(temp_data$ID, levels = c(1, 2, 3))

  expect_warning(
    check_raw_data(
      temp_data, def_b_coding$column, def_b_coding$u_name_value,
      def_b_coding$l_name_value
    ),
    "unused factor levels"
  )
})


test_that("comp_vals -> return vectors fail as expected", {
  # check that comp_vals returns reasonable values done when testing DMC
  # here only the warnings/stops checks about the component functions
  a_model <- drift_dm(
    prms_model = c(a = 2, b = 3), conds = c("foo", "bar"),
    subclass = "test", dt = .005, dx = .005, t_max = 1
  )


  # general
  temp <- a_model
  temp$comp_funs$mu_fun <- function(foo, prms_solve, t_vec, one_cond, ddm_opts) {
    mu_vec <- rep(3, length(t_vec) + 1)
    return(mu_vec)
  }
  expect_error(
    re_evaluate_model(temp),
    "mu_fun provided an unexpected number of values"
  )


  temp <- a_model
  temp$comp_funs$mu_fun <- function(foo, prms_solve, t_vec, one_cond, ddm_opts) {
    mu_vec <- rep(3, length(t_vec))
    return(as.character(mu_vec))
  }
  expect_error(re_evaluate_model(temp), "mu_fun provided non-numeric values")


  # NAs infinite
  temp <- a_model
  temp$comp_funs$b_fun <- function(foo, prms_solve, t_vec, one_cond, ddm_opts) {
    b_vec <- rep(3, length(t_vec))
    b_vec[1] <- Inf
    return(b_vec)
  }
  expect_error(re_evaluate_model(temp), "infinite")

  temp <- a_model
  temp$comp_funs$dt_b_fun <- function(foo, prms_solve, t_vec, one_cond, ddm_opts) {
    b_vec <- rep(3, length(t_vec))
    b_vec[1] <- NA
    return(b_vec)
  }
  expect_error(re_evaluate_model(temp), "infinite values or NAs")


  # some tests with x_fun
  temp <- a_model
  temp$comp_funs$x_fun <- function(foo, prms_solve, x_vec, one_cond, ddm_opts) {
    x_vec <- rep(1, length(x_vec) + 1)
    x_vec[1] <- NA
    return(x_vec)
  }
  expect_error(re_evaluate_model(temp), "infinite values or NAs")

  temp <- a_model
  temp$comp_funs$x_fun <- function(foo, prms_solve, x_vec, one_cond, ddm_opts) {
    x_vec <- rep(1, length(x_vec) + 1)
    x_vec[5] <- -1
    return(x_vec)
  }
  expect_error(re_evaluate_model(temp), "negative values")

  temp <- a_model
  temp$comp_funs$x_fun <- function(foo, prms_solve, x_vec, one_cond, ddm_opts) {
    x_vec <- rep(1, length(x_vec) + 1)
    return(x_vec)
  }
  expect_error(re_evaluate_model(temp), "integrate to 1")

  # nt_fun specific
  temp <- a_model
  temp$comp_funs$nt_fun <- function(foo, prms_solve, t_vec, one_cond, ddm_opts) {
    t_vec <- rep(1, length(t_vec))
    t_vec[1] <- -1
    return(t_vec)
  }
  expect_error(re_evaluate_model(temp), "negative values")

  temp <- a_model
  temp$comp_funs$nt_fun <- function(foo, prms_solve, t_vec, one_cond, ddm_opts) {
    t_vec <- rep(2, length(t_vec))
    return(t_vec)
  }
  expect_error(re_evaluate_model(temp), "integrate to 1")
})


# Simulate Functions ------------------------------------------------------

test_that("simulate_traces -> works as expected", {
  dt <- .01
  t_max <- 1

  # standard behavior
  my_prms <- c("a" = 2, "b" = 3, "cd" = 4)
  conds <- c("null")
  a_model <- ratcliff_dm(var_start = T, t_max = t_max, dt = dt)

  out <- simulate_traces(a_model, k = 2, conds = "null", sigma = 0)
  test <- suppressWarnings(unpack_traces(out))
  out <- unpack_obj(out)
  expect_identical(test, out)

  expect_identical(out[1, ], out[2, ])
  expect_equal(dim(out), c(2, 1 / .01 + 1))

  one_trace <- na.omit(out[1, ])
  expect_equal(
    length(one_trace),
    unname(ceiling(coef(a_model)["b"] / coef(a_model)["muc"] / .01) + 1)
  )

  # with noise and seed
  out <- simulate_traces(a_model, k = 1, seed = 1, add_x = T, unpack = T)
  expect_true(is.matrix(out))

  withr::local_seed(1)
  # draw from pdf to ensure the same rng behvaior
  xx <- seq(-1, 1, length.out = a_model$prms_solve[["nx"]] + 1)
  pdf_x <- dunif(xx,
    min = -coef(a_model)["range_start"] / 2,
    max = coef(a_model)["range_start"] / 2
  )
  xx <- xx * coef(a_model)["b"]
  samp_x <- draw_from_pdf(a_pdf = pdf_x, x_def = xx, k = 1)

  # now do the actual trace simulation as in Ulrich et al. Appendix C
  t <- seq(0, t_max, dt)
  mu <- coef(a_model)["muc"]
  dX <- mu * dt + 1 * sqrt(dt) * rnorm(length(t)) # see
  X <- c(0, cumsum(dX)) + samp_x
  X <- X[1:min(which(X > coef(a_model)["b"]))]
  expect_identical(out[!is.na(out)], X)



  # call with multiple conditions and with multiple ks
  # here test also for the presence of the attributes
  a_model <- dmc_dm(t_max = 1, dt = .005, dx = .005)
  out <- simulate_traces(a_model, k = c(comp = 2, incomp = 3), add_x = c(T, F),
                         sigma = c(0,1))

  expect_equal(class(out), "traces_dm_list")
  expect_equal(names(out), c("comp", "incomp"))
  expect_equal(class(out$comp), class(out$incomp))
  expect_equal(class(out$comp), "traces_dm")
  expect_equal(names(attributes(out)), c("names", "class", "t_vec"))
  expect_equal(attributes(out)$t_vec, seq(0, 1, 0.005))

  # traces_dm specific
  expect_equal(
    names(attributes(out$comp)),
    c("dim", "class", "t_vec", "mu_vals", "b_vals", "samp_x", "add_x",
      "orig_model_class", "orig_prms", "b_coding", "prms_solve")
  )
  expect_equal(dim(out$comp), c(2, 201))
  expect_equal(attr(out$comp, "add_x"), T)
  expect_equal(attr(out$comp, "prms_solve")["sigma"], c(sigma = 0))

  expect_equal(dim(out$incomp), c(3, 201))
  expect_equal(attr(out$incomp, "add_x"), F)
  expect_equal(attr(out$incomp, "prms_solve")["sigma"], c(sigma = 1))




  ## test the fits_ids_dm method
  all_fits <- load_fits_ids(
    path = test_path("fixtures"),
    fit_procedure_name = "test_case_saved"
  )

  traces_obj <- simulate_traces(all_fits, k = 1, unpack = T, sigma = 0)
  traces_obj <- na.omit(traces_obj[1, ])
  attributes(traces_obj) <- NULL

  # check
  mean_vals <- colMeans(coef(all_fits))
  t <- seq(0, prms_solve(all_fits)[["t_max"]], prms_solve(all_fits)[["dt"]])
  X <- t * mean_vals[["muc"]]
  fpt <- which(X > mean_vals[["b"]])
  X <- X[1:min(fpt)]
  expect_equal(traces_obj, X)
})

test_that("simulate_traces -> input checks", {
  a_model <- dmc_dm()

  # expected errors
  expect_error(simulate_traces(
    a_model,
    k = 1, conds = "null",
    seed = c(1, 2, 3)
  ), "seed must be a single numeric")

  expect_error(simulate_traces(
    a_model,
    k = 1, conds = "null",
    add_x = 1
  ), "logical")

  expect_error(
    simulate_traces(a_model, k = 1, conds = "foo"),
    "not in the model's conds"
  )
  expect_error(
    simulate_traces(a_model, k = 1, conds = 1),
    "must be a character"
  )
  expect_error(
    simulate_traces(
      a_model,
      k = -1,
    ),
    "k must be numeric > 0"
  )
  expect_error(
    simulate_traces(
      a_model,
      k = c(comp = 1, foo = 3),
    ),
    "names in k don't "
  )

  # no boundary hit
  temp <- a_model
  coef(temp)["muc"] <- 0
  expect_warning(
    simulate_traces(
      temp,
      k = 1, sigma = 0,
      conds = "incomp"
    ),
    "no boundary hit"
  )
})

test_that("simulate_data.drift_dm -> single data set works as expected", {
  # standard behavior
  a_model <- readRDS(file = test_path("fixtures", "ratcliff.rds"))
  b_coding(a_model) <- list(
    column = "col", u_name_value = c(a = 0),
    l_name_value = c(b = -1)
  )
  sim_data <- simulate_data(a_model, n = 100000, seed = 1)
  test <- check_raw_data(sim_data,
    b_coding_column = "col", u_value = 0,
    l_value = -1
  ) # only to check for unexpected errors

  # correct quantiles
  sim_quantiles_a <- quantile(sim_data$RT[sim_data$col == 0],
    probs = seq(0.1, 0.9, 0.1)
  )
  exp_quantiles <- calc_stats(a_model, type = "quantiles")
  expect_true(all(abs(sim_quantiles_a - exp_quantiles$Quant_a) <= 0.003))

  # error quantiles
  sim_quantiles_b <- quantile(sim_data$RT[sim_data$col == -1],
    probs = seq(0.1, 0.9, 0.1)
  )
  expect_true(all(abs(sim_quantiles_b - exp_quantiles$Quant_b) <= 0.004))
})

test_that("simulate_data.drift_dm -> multiple data sets works as expected", {
  # standard behavior
  a_model <- ratcliff_dm(dt = .005, dx = .01)

  df_prms <- data.frame(
    b = c(0.5, 0.6),
    ID = c("foo", 2),
    muc = c(3, 4),
    non_dec = c(0.3, 0.4)
  )

  # generate some data using df_prms
  sim_data <- simulate_data(a_model,
    n = 1000, df_prms = df_prms,
    seed = 1
  )
  expect_equal(sim_data$prms, df_prms[c("ID", names(coef(a_model)))])
  sim_data <- sim_data$synth_data

  # check validity of results
  withr::local_preserve_seed()
  set.seed(1)
  coef(a_model) <- c(muc = 3, b = 0.5, non_dec = 0.3)
  exp_data_1 <- simulate_data(a_model, n = 1000)
  coef(a_model) <- c(muc = 4, b = 0.6, non_dec = 0.4)
  exp_data_2 <- simulate_data(a_model, n = 1000)

  expect_equal(unique(sim_data$ID), c("foo", "2"))
  sim_data_1 <- sim_data[sim_data$ID == "foo", ][c("RT", "Error", "Cond")]
  expect_equal(sim_data_1, exp_data_1)
  sim_data_2 <- sim_data[sim_data$ID == "2", ][c("RT", "Error", "Cond")]
  rownames(sim_data_2) <- 1:nrow(sim_data_2)
  expect_equal(sim_data_2, exp_data_2)


  ## now do the same but with lower/upper
  lower <- list(default_values = c(b = 0.4, muc = 2, non_dec = 0.2))
  upper <- list(default_values = c(b = 1, muc = 6, non_dec = 0.5))
  all_data <- simulate_data(a_model,
    n = 100, lower = lower, upper = upper,
    k = 2, seed = 1
  )
  sim_data <- all_data$synth_data

  # check validity of results
  withr::local_preserve_seed()
  set.seed(1)
  trash <- runif(6)
  coef(a_model) <- unlist(all_data$prms[1, -1])
  exp_data_1 <- simulate_data(a_model, n = 100)
  coef(a_model) <- unlist(all_data$prms[2, -1])
  exp_data_2 <- simulate_data(a_model, n = 100)



  expect_equal(unique(sim_data$ID), c(1, 2))
  sim_data_1 <- sim_data[sim_data$ID == 1, ][c("RT", "Error", "Cond")]
  expect_equal(sim_data_1, exp_data_1)
  sim_data_2 <- sim_data[sim_data$ID == 2, ][c("RT", "Error", "Cond")]
  rownames(sim_data_2) <- 1:nrow(sim_data_2)
  expect_equal(sim_data_2, exp_data_2)
})

test_that("simulate_data.drift_dm -> input checks", {
  a_model <- readRDS(file = test_path("fixtures", "dmc.rds"))

  # input checks
  expect_error(simulate_data(a_model, n = -1), "> 0")
  expect_error(
    simulate_data(a_model, n = c(2, 3, 4)),
    "must have as many entries"
  )
  expect_error(
    simulate_data(a_model, n = c(comp = 2, ho = 2)),
    "specify names"
  )

  expect_error(
    simulate_data(a_model, n = 10, seed = c("1", "2")),
    "single numeric"
  )
  expect_error(simulate_data(a_model, n = 10, seed = c(1, 2)), "single numeric")
  expect_error(simulate_data(a_model, n = 10, seed = 1, verbose = 2), "0 or 1")


  # input checks with respect to lower/upper and df_prms
  a_model <- readRDS(file = test_path("fixtures", "ratcliff.rds"))


  expect_error(
    simulate_data(a_model, 10,
      lower = c(1, 2, 3), upper = c(1, 2, 3),
      df_prms = data.frame()
    ),
    "lower/upper OR df_prms, not both"
  )
  expect_error(
    simulate_data(a_model, 10, k = 2),
    "lower/upper OR df_prms"
  )

  expect_error(
    simulate_data(a_model, 10, k = 2, df_prms = list()),
    "must be a data.frame"
  )
  expect_error(
    simulate_data(
      a_model, 10,
      k = 2,
      df_prms = data.frame(ID = c(), muc = c(), b = c(), non_dec = c())
    ),
    "at least one row"
  )

  expect_error(
    simulate_data(
      a_model, 10,
      k = 2,
      df_prms = data.frame(muc = 1, b = 2, non_dec = 3)
    ),
    "no ID column"
  )

  expect_error(
    simulate_data(
      a_model, 10,
      k = 2,
      df_prms = data.frame(ID = 1, b = 2, non_dec = 3)
    ),
    "don't match"
  )
  expect_error(
    simulate_data(
      a_model, 10,
      k = 2,
      df_prms = data.frame(ID = 1, b = 2, non_dec = 3, muc = 3, foo = 4)
    ),
    "don't match"
  )
  expect_error(
    simulate_data(
      a_model, 10,
      k = 2,
      df_prms = data.frame(ID = 1, b = 2, non_dec = 3, muc = "3")
    ),
    "must be valid numbers"
  )
})
