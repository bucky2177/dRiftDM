test_that("force negative pdf values", {
  a_model <- ratcliff_dm(dt = .01, dx = .01, obs_data = ratcliff_synth_data)
  a_model <- set_model_prms(a_model, c(muc = 7, b = 0.1, non_dec = 0.3))
  a_model <- suppressWarnings(re_evaluate_model(a_model))
  expect_identical(-Inf, a_model$log_like_val)
  expect_warning(
    expect_warning(
      expect_warning(
        expect_warning(
          re_evaluate_model(a_model), "NaNs"
        ), "NaNs"
      ), "when calculating the log-likelihood"
    ), "when calculating the pdf"
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
  a_model <- set_obs_data(a_model, data, eval_model = T)
  a_model <- re_evaluate_model(a_model)
  pdfs_null <- a_model$pdfs$null
  pdfs_foo <- a_model$pdfs$foo

  # log_like_value calculated by hand
  t_vec <- seq(0, 1, 0.005)
  d_1 <- (pdfs_null$pdf_u[88] - pdfs_null$pdf_u[87]) * 0.2 + pdfs_null$pdf_u[87]
  d_2 <- (pdfs_null$pdf_u[82] - pdfs_null$pdf_u[81]) * 0.4 + pdfs_null$pdf_u[81]
  d_3 <- (pdfs_null$pdf_l[70] - pdfs_null$pdf_l[69]) * 0.8 + pdfs_null$pdf_l[69]
  d_4 <- (pdfs_foo$pdf_u[68] - pdfs_foo$pdf_u[67]) * 0.44 + pdfs_foo$pdf_u[67]
  d_1 <- d_1
  d_2 <- d_2
  d_3 <- d_3
  d_4 <- d_4
  for_test_log_like <- log(d_1) + log(d_2) + log(d_3) + log(d_4)
  # calc_log_like
  expect_equal(a_model$log_like, for_test_log_like)
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
