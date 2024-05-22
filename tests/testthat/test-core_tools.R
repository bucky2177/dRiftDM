test_that("draw_from_pdf works as expected", {
  quants <- qnorm(seq(0.1, 0.9, 0.1))

  x_def <- seq(-4, 4, 0.005)
  pdf <- dnorm(x_def)
  samp_quants <- quantile(
    draw_from_pdf(
      a_pdf = pdf, x_def = x_def,
      k = 50000, seed = 1
    ),
    probs = seq(0.1, 0.9, 0.1)
  )
  expect_true(all(abs(quants - samp_quants) < .01))
})

test_that("input checks for draw_from_pdf", {
  x_def <- seq(-4, 4, 0.01)
  pdf <- dnorm(x_def)
  expect_error(
    draw_from_pdf(a_pdf = numeric(), x_def = x_def, k = 1),
    "numeric vector of length > 0"
  )
  expect_error(
    draw_from_pdf(a_pdf = character(), x_def = x_def, k = 1),
    "numeric vector of length > 0"
  )
  expect_error(
    draw_from_pdf(a_pdf = pdf, x_def = numeric(), k = 1),
    "numeric vector of length > 0"
  )
  expect_error(
    draw_from_pdf(a_pdf = pdf, x_def = character(), k = 1),
    "numeric vector of length > 0"
  )

  expect_error(
    draw_from_pdf(a_pdf = pdf, x_def = c(1, 2, 3), k = 1),
    "don't match"
  )

  expect_error(
    draw_from_pdf(a_pdf = pdf, x_def = x_def, k = c(1, 1)),
    "single numeric"
  )
  expect_error(
    draw_from_pdf(a_pdf = pdf, x_def = x_def, k = -1),
    "must be >= 0"
  )

  expect_error(
    draw_from_pdf(a_pdf = pdf, x_def = x_def, k = 3, seed = "1"),
    "must be a single numeric"
  )

  run_1 <- draw_from_pdf(a_pdf = pdf, x_def = x_def, k = 3, seed = 1)
  run_2 <- draw_from_pdf(a_pdf = pdf, x_def = x_def, k = 3, seed = 1)
  expect_identical(run_1, run_2)

  expect_identical(
    draw_from_pdf(a_pdf = pdf, x_def = x_def, k = 0),
    numeric()
  )

  pdf <- pdf - 0.1
  expect_warning(draw_from_pdf(pdf, x_def, 1), "negative pdf values")
})

