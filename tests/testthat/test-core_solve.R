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
  expect_true(all(abs(d3 - d3_test) < 0.00001))
})



test_that("test im_zero", {

  a_model = dmc_dm(t_max = 1, dx = .001, dt = .001, var_start = F)
  solver(a_model) <- "im_zero"

  pdf_u = numeric(1001)
  pdf_l = numeric(1001)

  t_vec = seq(0, 1, 0.001)

  comp_vals = comp_vals(a_model)
  cpp_imzero(pdf_u = pdf_u, pdf_l = pdf_l,
             nt = 1000, nx = 2000, dt = .001, dx = .001,
             sigma = 1, b_vals = comp_vals$comp$b_vals,
             mu_vals =  comp_vals$comp$mu_vals,
             mu_int_vals = comp_vals$comp$mu_int_vals,
             dt_b_vals = comp_vals$comp$dt_b_vals, t_vec = t_vec)

  # load values provided by Thomas' python code
  test_u_pdf = read.table(file = test_path("fixtures", "test_im_zero_u.txt"))[[1]]
  test_l_pdf = read.table(file = test_path("fixtures", "test_im_zero_l.txt"))[[1]]

  expect_equal(test_u_pdf, pdf_u)
  expect_equal(test_l_pdf, pdf_l)

})
