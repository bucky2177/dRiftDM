# -> log_like is tested in calc_stats

# -> kfe was originally tested against code by Thomas, but since we now
#  have the more stable method implemented, checks are only done against
# DMCfun

# for get_pdf -> see test_models
test_that("add_residual works as expected", {
  time <- seq(0, 0.9, 0.01)
  d1 <- dnorm(time, mean = 0.3, sd = 0.01)
  d2 <- dnorm(time, mean = 0.4, sd = 0.02)

  d3_test <- add_residual(
    pdf_nt = d1, pdf_u = d2 * (1 - 1e-30),
    pdf_l = d2 * 1e-30, dt = 0.01, nt = 0.9 / 0.01
  )[[1]]

  d3 <- dnorm(time, mean = 0.7, sd = sqrt(0.01^2 + 0.02^2))
  expect_equal(d3, d3_test, tolerance = 0.00001)

  # input checks
  expect_error(
    add_residual(
      pdf_nt = d1, pdf_u = d2[-1],
      pdf_l = d2 * 1e-30, dt = 0.01, nt = 0.9 / 0.01
    ),
    "pdf_u and pdf_nt don't have the same length"
  )

  expect_error(
    add_residual(
      pdf_nt = d1, pdf_u = d2,
      pdf_l = d2[-1] * 1e-30, dt = 0.01, nt = 0.9 / 0.01
    ),
    "pdf_l and pdf_nt don't have the same length"
  )

  expect_warning(
    add_residual(
      pdf_nt = d1, pdf_u = d2,
      pdf_l = d2*0.5, dt = 0.01, nt = 0.9 / 0.01
    ),
    "don't integrate to the same value"
  )

})



test_that("test im_zero", {
  a_model <- dmc_dm(t_max = 1, dx = .001, dt = .001, var_start = F)
  solver(a_model) <- "im_zero"

  pdf_u <- numeric(1001)
  pdf_l <- numeric(1001)

  t_vec <- seq(0, 1, 0.001)

  comp_vals <- comp_vals(a_model)
  cpp_imzero(
    pdf_u = pdf_u, pdf_l = pdf_l,
    nt = 1000, dt = .001,
    sigma = 1, b_vals = comp_vals$comp$b_vals,
    mu_vals = comp_vals$comp$mu_vals,
    mu_int_vals = comp_vals$comp$mu_int_vals,
    dt_b_vals = comp_vals$comp$dt_b_vals, t_vec = t_vec
  )

  # load values provided by Thomas' python code
  test_u_pdf <- read.table(file = test_path("fixtures", "test_im_zero_u.txt"))[[1]]
  test_l_pdf <- read.table(file = test_path("fixtures", "test_im_zero_l.txt"))[[1]]

  expect_equal(test_u_pdf, pdf_u)
  expect_equal(test_l_pdf, pdf_l)

  # do the same but with re_evaluate_model
  a_model <- dmc_dm(
    t_max = 1, dx = .001, dt = .001, var_start = F,
    var_non_dec = F
  )
  solver(a_model) <- "im_zero"
  coef(a_model)["non_dec"] <- 0
  a_model <- re_evaluate_model(a_model)
  expect_equal(test_u_pdf, a_model$pdfs$comp$pdf_u, tolerance = .0001)
  expect_equal(test_l_pdf, a_model$pdfs$comp$pdf_l, tolerance = .0001)


  # input length checks
  nt = prms_solve(a_model)["nt"]
  dt = prms_solve(a_model)["dt"]
  sigma = prms_solve(a_model)["sigma"]
  t_max = prms_solve(a_model)["t_max"]

  comp_vals = comp_vals(a_model)

  pdf_u = numeric(nt+1)
  pdf_l = numeric(nt+1)
  t_vec = seq(0, t_max, length.out = nt+1)

  expect_error(
    cpp_imzero(pdf_u = pdf_u[-1], pdf_l = pdf_l, nt = nt, dt = dt, sigma = sigma,
               b_vals = comp_vals$comp$b_vals, mu_vals = comp_vals$comp$mu_vals,
               mu_int_vals = comp_vals$comp$mu_int_vals,
               dt_b_vals = comp_vals$comp$dt_b_vals, t_vec = t_vec),
    "pdf-upper has wrong size"
  )

  expect_error(
    cpp_imzero(pdf_u = pdf_u, pdf_l = pdf_l[-1], nt = nt, dt = dt, sigma = sigma,
               b_vals = comp_vals$comp$b_vals, mu_vals = comp_vals$comp$mu_vals,
               mu_int_vals = comp_vals$comp$mu_int_vals,
               dt_b_vals = comp_vals$comp$dt_b_vals, t_vec = t_vec),
    "pdf-lower has wrong size"
  )

  expect_error(
    cpp_imzero(pdf_u = pdf_u, pdf_l = pdf_l, nt = nt, dt = dt, sigma = sigma,
               b_vals = comp_vals$comp$b_vals[-1], mu_vals = comp_vals$comp$mu_vals,
               mu_int_vals = comp_vals$comp$mu_int_vals,
               dt_b_vals = comp_vals$comp$dt_b_vals, t_vec = t_vec),
    "b_vals has wrong size"
  )

  expect_error(
    cpp_imzero(pdf_u = pdf_u, pdf_l = pdf_l, nt = nt, dt = dt, sigma = sigma,
               b_vals = comp_vals$comp$b_vals, mu_vals = comp_vals$comp$mu_vals[-1],
               mu_int_vals = comp_vals$comp$mu_int_vals,
               dt_b_vals = comp_vals$comp$dt_b_vals, t_vec = t_vec),
    "mu_vals has wrong size"
  )


  expect_error(
    cpp_imzero(pdf_u = pdf_u, pdf_l = pdf_l, nt = nt, dt = dt, sigma = sigma,
               b_vals = comp_vals$comp$b_vals, mu_vals = comp_vals$comp$mu_vals,
               mu_int_vals = comp_vals$comp$mu_int_vals[-1],
               dt_b_vals = comp_vals$comp$dt_b_vals, t_vec = t_vec),
    "mu_int_vals has wrong size"
  )


  expect_error(
    cpp_imzero(pdf_u = pdf_u, pdf_l = pdf_l, nt = nt, dt = dt, sigma = sigma,
               b_vals = comp_vals$comp$b_vals, mu_vals = comp_vals$comp$mu_vals,
               mu_int_vals = comp_vals$comp$mu_int_vals,
               dt_b_vals = comp_vals$comp$dt_b_vals[-1], t_vec = t_vec),
    "dt_b_vals has wrong size"
  )



  expect_error(
    cpp_imzero(pdf_u = pdf_u, pdf_l = pdf_l, nt = nt, dt = dt, sigma = sigma,
               b_vals = comp_vals$comp$b_vals, mu_vals = comp_vals$comp$mu_vals,
               mu_int_vals = comp_vals$comp$mu_int_vals,
               dt_b_vals = comp_vals$comp$dt_b_vals, t_vec = t_vec[-1]),
    "t_vec has wrong size"
  )

})



test_that("kfe input checks", {

  a_model = readRDS(test_path("fixtures", "ratcliff.rds"))

  # input checks
  nt = prms_solve(a_model)["nt"]
  nx = prms_solve(a_model)["nx"]
  dt = prms_solve(a_model)["dt"]
  dx = prms_solve(a_model)["dx"]
  sigma = prms_solve(a_model)["sigma"]
  t_max = prms_solve(a_model)["t_max"]

  comp_vals = comp_vals(a_model)[[1]]

  pdf_u = numeric(nt+1)
  pdf_l = numeric(nt+1)
  t_vec = seq(0, t_max, length.out = nt+1)
  x_vec = seq(-1, 1, length.out = nx+1)

  expect_error(
    cpp_kfe(pdf_u = pdf_u[-1], pdf_l = pdf_l, xx = comp_vals$x_vals,
            nt = nt, nx = nx, dt = dt, dx = dx, sigma = sigma,
            b_vals = comp_vals$b_vals, mu_vals = comp_vals$mu_vals,
            dt_b_vals = comp_vals$dt_b_vals, x_vec = x_vec),
    "pdf-upper has wrong size"
  )

  expect_error(
    cpp_kfe(pdf_u = pdf_u, pdf_l = pdf_l[-1], xx = comp_vals$x_vals,
            nt = nt, nx = nx, dt = dt, dx = dx, sigma = sigma,
            b_vals = comp_vals$b_vals, mu_vals = comp_vals$mu_vals,
            dt_b_vals = comp_vals$dt_b_vals, x_vec = x_vec),
    "pdf-lower has wrong size"
  )

  expect_error(
    cpp_kfe(pdf_u = pdf_u, pdf_l = pdf_l, xx = comp_vals$x_vals[-1],
            nt = nt, nx = nx, dt = dt, dx = dx, sigma = sigma,
            b_vals = comp_vals$b_vals, mu_vals = comp_vals$mu_vals,
            dt_b_vals = comp_vals$dt_b_vals, x_vec = x_vec),
    "xx has wrong size"
  )



  expect_error(
    cpp_kfe(pdf_u = pdf_u, pdf_l = pdf_l, xx = comp_vals$x_vals,
            nt = nt, nx = nx, dt = dt, dx = dx, sigma = sigma,
            b_vals = comp_vals$b_vals[-1], mu_vals = comp_vals$mu_vals,
            dt_b_vals = comp_vals$dt_b_vals, x_vec = x_vec),
    "b_vals has wrong size"
  )


  expect_error(
    cpp_kfe(pdf_u = pdf_u, pdf_l = pdf_l, xx = comp_vals$x_vals,
            nt = nt, nx = nx, dt = dt, dx = dx, sigma = sigma,
            b_vals = comp_vals$b_vals, mu_vals = comp_vals$mu_vals[-1],
            dt_b_vals = comp_vals$dt_b_vals, x_vec = x_vec),
    "mu_vals has wrong size"
  )



  expect_error(
    cpp_kfe(pdf_u = pdf_u, pdf_l = pdf_l, xx = comp_vals$x_vals,
            nt = nt, nx = nx, dt = dt, dx = dx, sigma = sigma,
            b_vals = comp_vals$b_vals, mu_vals = comp_vals$mu_vals,
            dt_b_vals = comp_vals$dt_b_vals[-1], x_vec = x_vec),
    "dt_b_vals has wrong size"
  )


  expect_error(
    cpp_kfe(pdf_u = pdf_u, pdf_l = pdf_l, xx = comp_vals$x_vals,
            nt = nt, nx = nx, dt = dt, dx = dx, sigma = sigma,
            b_vals = comp_vals$b_vals, mu_vals = comp_vals$mu_vals,
            dt_b_vals = comp_vals$dt_b_vals, x_vec = x_vec[-1]),
    "x_vec has wrong size"
  )


  test_rhs = comp_vals$x_vals
  test_rhs[1] = 1
  expect_error(
    cpp_kfe(pdf_u = pdf_u, pdf_l = pdf_l, xx = test_rhs,
            nt = nt, nx = nx, dt = dt, dx = dx, sigma = sigma,
            b_vals = comp_vals$b_vals, mu_vals = comp_vals$mu_vals,
            dt_b_vals = comp_vals$dt_b_vals, x_vec = x_vec),
    "rhs not zero on thresholds"
  )

})



test_that("input checks calc_pdfs", {

  a_model = ratcliff_dm(var_drift = T)

  # no muc parameter
  temp = a_model
  flex_prms(temp) = flex_prms(
    coef(a_model)[names(coef(a_model)) != "muc"],
    conds = "null"
  )

  expect_error(
    calc_pdfs(temp, x_vec = NULL, t_vec = NULL, prms_solve = NULL),
    "sd_muc found, but no parameter muc"
  )

  # no dRiftDM drift rate
  temp = a_model
  comp_funs(temp)[["mu_fun"]] = mu_dmc
  expect_error(
    calc_pdfs(temp, x_vec = NULL, t_vec = NULL, prms_solve = NULL),
    "requires dRiftDM's mu_constant function"
  )

  temp = a_model
  solver(temp) = "im_zero"
  temp$comp_funs$mu_int_fun = mu_int_dmc
  expect_error(
    calc_pdfs(temp, x_vec = NULL, t_vec = NULL, prms_solve = NULL),
    "requires dRiftDM's mu_int_constant function"
  )

  # weird solver
  temp = a_model
  temp$solver = "foo"
  expect_error(
    calc_pdfs(temp, x_vec = NULL, t_vec = NULL, prms_solve = NULL),
    "solver foo not implemented"
  )
})


test_that("subst. negative PDF values test", {
  # before evaluating
  a_model <- readRDS(test_path("fixtures", "ratcliff.rds"))
  obs_data(a_model) = ratcliff_synth_data

  prms_solve(a_model) <- c(t_max = 3, dt = .1, dx = .1, sigma = 2)
  expect_identical(
    prms_solve(a_model),
    c(sigma = 2, t_max = 3, dt = .1, dx = .1, nt = 30, nx = 20)
  )

  # negative density warnings
  # add a data point to trigger the warning twice: when calculating the density
  # values and when computing the log_likelihood
  a_model$obs_data$rts_l$null = c(2.5, a_model$obs_data$rts_l$null)
  suppressWarnings(
    prms_solve(a_model)[c("dx", "dt")] <- c(dx = .5, dt = .5)
  )
  expect_warning(
    expect_warning(
      expect_warning(re_evaluate_model(a_model), "negative density values"),
      "NaNs"
    ), "when calculating the log-likelihood"
  )

})


test_that("untreated error - log_like_heart", {

  expect_warning(
    log_like_heart(pdf_u = NULL, pdf_l = NULL, t_vec = NULL, rts_u = NULL,
                   rts_l = NULL),
    "untreated error"
  )

})
