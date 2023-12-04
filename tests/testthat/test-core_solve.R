test_that("force negative pdf values", {
  a_model <- ratcliff_dm(dt = .01, dx = .01, obs_data = ratcliff_data)
  a_model <- set_model_prms(a_model, c(7, 0.1, 0.3))
  retur_val <- suppressWarnings(calc_log_like(a_model))
  expect_identical(-Inf, retur_val)
  expect_warning(
    calc_pdfs(a_model, one_cond = "null", solver = "kfe"),
    "subst. negative density"
  )

  a_model <- set_model_prms(a_model, c(7, 0.14, 0.3))
  retur_val <- suppressWarnings(calc_log_like(a_model))
  expect_identical(-Inf, retur_val)
  expect_warning(
    expect_warning(
      expect_warning(
        expect_warning(
          calc_log_like(a_model), "negative density values"
        ), "NaNs"
      ), "NaNs"
    ), "when calculating the log-likelihood"
  )

  a_model <- ratcliff_dm(dt = .01, dx = .01)
  retur_val <- suppressWarnings(calc_log_like(a_model))
  expect_identical(-Inf, retur_val)
  expect_warning(calc_log_like(a_model), "no data is provided")
})






test_that("call pdfs with unreasonable components", {
  a_model <- ratcliff_dm()
  class(a_model) <- c("foo", class(a_model))

  # get an x of the wrong dimension
  x_foo <- function(drift_dm_obj, x_vec, one_cond) {
    dx <- drift_dm_obj$prms_solve[["dx"]]
    vec <- c(1, 1, 1, 1) / (4 * dx)
    return(vec)
  }
  temp <- a_model
  temp$comp_funs$x_fun <- x_foo
  expect_error(
    calc_pdfs(temp, one_cond = "null", solver = "kfe"),
    "unexpected length of x_vals"
  )

  # get an x that is not normalized
  x_foo <- function(drift_dm_obj, x_vec, one_cond) {
    vec <- numeric(length(x_vec))
    vec[30] <- 1
    return(vec)
  }
  temp <- a_model
  temp$comp_funs$x_fun <- x_foo
  expect_error(
    calc_pdfs(temp, one_cond = "null", solver = "kfe"),
    "doesn't integrate to 1"
  )

  # get an x that is contains NAs
  x_foo <- function(drift_dm_obj, x_vec, one_cond) {
    dx <- drift_dm_obj$prms_solve[["dx"]]
    vec <- numeric(length(x_vec))
    vec[10] <- 1 / dx
    vec[1] <- NA
    return(vec)
  }
  temp <- a_model
  temp$comp_funs$x_fun <- x_foo
  expect_error(
    calc_pdfs(temp, one_cond = "null", solver = "kfe"),
    "NAs"
  )


  # get wrong mu dimension
  mu_foo <- function(drift_dm_obj, t_vec, one_cond) {
    mu <- standard_drift()
    if (!is.numeric(t_vec) | length(t_vec) <= 1) {
      stop("t_vec is not a vector")
    }
    mu <- rep(mu, length(t_vec) - 3)
    return(mu)
  }
  temp <- a_model
  temp$comp_funs$mu_fun <- mu_foo
  expect_error(
    calc_pdfs(temp, one_cond = "null", solver = "kfe"),
    "unexpected length of mu_vals"
  )

  # get wrong mu values
  mu_foo <- function(drift_dm_obj, t_vec, one_cond) {
    mu <- NA
    if (!is.numeric(t_vec) | length(t_vec) <= 1) {
      stop("t_vec is not a vector")
    }
    mu <- rep(mu, length(t_vec))
    return(mu)
  }
  temp <- a_model
  temp$comp_funs$mu_fun <- mu_foo
  expect_error(
    calc_pdfs(temp, one_cond = "null", solver = "kfe"),
    "or NAs"
  )

  # get wrong mu values
  mu_foo <- function(drift_dm_obj, t_vec, one_cond) {
    mu <- Inf
    if (!is.numeric(t_vec) | length(t_vec) <= 1) {
      stop("t_vec is not a vector")
    }
    mu <- rep(mu, length(t_vec))
    return(mu)
  }
  temp <- a_model
  temp$comp_funs$mu_fun <- mu_foo
  expect_error(
    calc_pdfs(temp, one_cond = "null", solver = "kfe"),
    "infinite"
  )



  # get wrong b
  b_foo <- function(drift_dm_obj, t_vec, one_cond) {
    b <- standard_boundary()
    b <- rep(b, length(t_vec) - 3)
    return(b)
  }
  temp <- a_model
  temp$comp_funs$b_fun <- b_foo
  expect_error(
    calc_pdfs(temp, one_cond = "null", solver = "kfe"),
    "unexpected length of b_vals"
  )

  # get wrong b values
  b_foo <- function(drift_dm_obj, t_vec, one_cond) {
    b <- NA
    b <- rep(b, length(t_vec))
    return(b)
  }
  temp <- a_model
  temp$comp_funs$b_fun <- b_foo
  expect_error(
    calc_pdfs(temp, one_cond = "null", solver = "kfe"),
    "or NAs"
  )


  # get wrong b values
  b_foo <- function(drift_dm_obj, t_vec, one_cond) {
    b <- -Inf
    b <- rep(b, length(t_vec))
    return(b)
  }
  temp <- a_model
  temp$comp_funs$b_fun <- b_foo
  expect_error(
    calc_pdfs(temp, one_cond = "null", solver = "kfe"),
    "or NAs"
  )


  # get wrong dt_b
  dt_b_foo <- function(drift_dm_obj, t_vec, one_cond) {
    dt_b <- rep(0, length(t_vec) - 3)
    return(dt_b)
  }
  temp <- a_model
  temp$comp_funs$dt_b_fun <- dt_b_foo
  expect_error(
    calc_pdfs(temp, one_cond = "null", solver = "kfe"),
    "unexpected length of dt_b_vals"
  )


  # get wrong dt_b values
  dt_b_foo <- function(drift_dm_obj, t_vec, one_cond) {
    dt_b <- rep(0, length(t_vec))
    dt_b[29] <- NA
    return(dt_b)
  }
  temp <- a_model
  temp$comp_funs$dt_b_fun <- dt_b_foo
  expect_error(
    calc_pdfs(temp, one_cond = "null", solver = "kfe"),
    "NAs"
  )


  # get wrong dt_b values
  dt_b_foo <- function(drift_dm_obj, t_vec, one_cond) {
    dt_b <- rep(0, length(t_vec))
    dt_b[29] <- Inf
    return(dt_b)
  }
  temp <- a_model
  temp$comp_funs$dt_b_fun <- dt_b_foo
  expect_error(
    calc_pdfs(temp, one_cond = "null", solver = "kfe"),
    "infinite"
  )

  # wrong nt dimension
  a_model <- set_solver_settings(a_model, c("dx", "dt"), c(0.01, 0.01))
  nt_foo <- function(drift_dm_obj, t_vec, one_cond) {
    d_nt <- dnorm(t_vec[1:100], 0.4, 0.1)
    return(d_nt)
  }
  temp <- a_model
  temp$comp_funs$nt_fun <- nt_foo
  expect_error(
    calc_pdfs(temp, one_cond = "null", solver = "kfe"),
    "don't have the same dimension"
  )

  # wrong nt values
  nt_foo <- function(drift_dm_obj, t_vec, one_cond) {
    d_nt <- dnorm(t_vec, 0.4, 0.1) * 0.5
    return(d_nt)
  }
  temp <- a_model
  temp$comp_funs$nt_fun <- nt_foo
  expect_warning(
    calc_pdfs(temp, one_cond = "null", solver = "kfe"),
    "don't integrate to the same value"
  )

  # wrong nt values
  nt_foo <- function(drift_dm_obj, t_vec, one_cond) {
    d_nt <- dnorm(t_vec, 0.4, 0.1) * 0.5
    d_nt[213] <- NA
    return(d_nt)
  }
  temp <- a_model
  temp$comp_funs$nt_fun <- nt_foo
  expect_error(
    calc_pdfs(temp, one_cond = "null", solver = "kfe"),
    "or NAs"
  )
})

test_that("test log_like", {
  data <- data.frame(
    RT = c(0.431, 0.402, 0.344, 0.3322),
    Error = c(0, 0, 1, 0),
    Cond = c("null", "null", "null", "foo")
  )

  a_model <- drift_dm(c("a" = 4),
    conds = c("null", "foo"), dx = 0.005,
    dt = 0.005, t_max = 1
  )
  a_model <- set_obs_data(a_model, data)
  pdfs_null <- calc_pdfs(a_model, "null", "kfe")
  pdfs_foo <- calc_pdfs(a_model, "foo", "kfe")

  # log_like_value calculated by hand
  t_vec <- seq(0, 1, 0.005)
  d_1 <- (pdfs_null$pdf_u[88] - pdfs_null$pdf_u[87]) * 0.2 + pdfs_null$pdf_u[87]
  d_2 <- (pdfs_null$pdf_u[82] - pdfs_null$pdf_u[81]) * 0.4 + pdfs_null$pdf_u[81]
  d_3 <- (pdfs_null$pdf_l[70] - pdfs_null$pdf_l[69]) * 0.8 + pdfs_null$pdf_l[69]
  d_4 <- (pdfs_foo$pdf_u[68] - pdfs_foo$pdf_u[67]) * 0.44 + pdfs_foo$pdf_u[67]
  d_1 <- d_1 + drift_dm_robust_prm()
  d_2 <- d_2 + drift_dm_robust_prm()
  d_3 <- d_3 + drift_dm_robust_prm()
  d_4 <- d_4 + drift_dm_robust_prm()
  for_test_log_like <- log(d_1) + log(d_2) + log(d_3) + log(d_4)

  # calc_log_like
  by_model_log_like <- calc_log_like(a_model)

  expect_equal(by_model_log_like, for_test_log_like)
})

# for get_pdf -> see test_models

test_that("add_residual works as expected", {
  time <- seq(0, 0.9, 0.0001)
  d1 <- dnorm(time, mean = 0.3, sd = 0.01)
  d2 <- dnorm(time, mean = 0.4, sd = 0.02)

  d3_test <- add_residual(
    pdf_nt = d1, pdf_u = d2 * (1 - 1e-30),
    pdf_l = d2 * 1e-30, dt = 0.0001
  )[[1]]

  d3 <- dnorm(time, mean = 0.7, sd = sqrt(0.01^2 + 0.02^2))
  expect_true(all(abs(d3 - d3_test) < 0.1))
})
