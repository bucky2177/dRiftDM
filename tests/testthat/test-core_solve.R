# -> log_like is tested in calc_stats

# -> kfe was originally tested against code by Thomas, but since we now
#  have the more stable method implemented, checks are only done against
# DMCfun

# -> im_zero once implemented

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
