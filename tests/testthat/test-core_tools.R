test_that("draw_from_pdf works as expected", {

  quants <- qnorm(seq(0.1, 0.9, 0.1))
  x_def <- seq(-4, 4, 0.01)
  pdf <- dnorm(x_def)

  # discrete
  samp_quants <- quantile(
    draw_from_pdf(
      a_pdf = pdf, x_def = x_def,
      k = 50000, seed = 1, method = "discr"
    ),
    probs = seq(0.1, 0.9, 0.1)
  )
  expect_true(all(abs(quants - samp_quants) < .01))


  # linear
  samp_quants <- quantile(
    draw_from_pdf(
      a_pdf = pdf, x_def = x_def,
      k = 50000, seed = 1, method = "linear"
    ),
    probs = seq(0.1, 0.9, 0.1)
  )
  expect_true(all(abs(quants - samp_quants) < .01))


  # rounding
  test = draw_from_pdf(a_pdf = pdf, x_def = x_def, k = 50000,
                method = "linear", round_to = 1)
  expect_equal(min(diff(sort(unique(test)))), 0.1)
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
    "single valid numeric"
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

test_that("simulate_values works as expected", {
  withr::local_preserve_seed()
  set.seed(1)

  probs <- c(0.15, 0.3, 0.5, 0.7, 0.85)

  # uniform
  dat <- simulate_values(
    lower = c(1, 2), upper = c(2, 5),
    k = 10000
  )

  test1 <- runif(n = 10000, min = 1, max = 2)
  test2 <- runif(n = 10000, min = 2, max = 5)
  test1 <- quantile(test1, probs = probs)
  test2 <- quantile(test2, probs = probs)

  expect_true(
    all(abs(quantile(dat$V1, probs = probs) - test1) < .1)
  )

  expect_true(
    all(abs(quantile(dat$V2, probs = probs) - test2) < .1)
  )


  # truncated normal
  dat <- simulate_values(
    lower = c(1, 2), upper = c(2, 5),
    k = 10000, distr = "tnorm",
    means = c(1.3, 3), sds = c(0.4, 0.1)
  )

  test1 <- truncnorm::rtruncnorm(n = 10000, a = 1, b = 2, mean = 1.3, sd = 0.4)
  test2 <- truncnorm::rtruncnorm(n = 10000, a = 2, b = 5, mean = 3, sd = 0.1)
  test1 <- quantile(test1, probs = probs)
  test2 <- quantile(test2, probs = probs)

  expect_true(
    all(abs(quantile(dat$V1, probs = probs) - test1) < .1)
  )

  expect_true(
    all(abs(quantile(dat$V2, probs = probs) - test2) < .1)
  )

  # returned value checks
  dat <- simulate_values(
    lower = c(a = 1, b = 2), upper = c(a = 2, b = 5),
    k = 2, distr = "tnorm",
    means = c(a = 1.3, b = 3), sds = c(a = 0.4, b = 0.1)
  )
  expect_equal(colnames(dat), c("a", "b", "ID"))
  expect_true(is.data.frame(dat))


  dat <- simulate_values(
    lower = c(a = 1, b = 2), upper = c(a = 2, b = 5),
    k = 2, distr = "tnorm",
    means = c(a = 1.3, b = 3), sds = c(a = 0.4, b = 0.1),
    cast_to_data_frame = F, add_id_column = "none"
  )
  expect_equal(colnames(dat), c("a", "b"))
  expect_true(is.matrix(dat))


  dat <- simulate_values(
    lower = c(a = 1, b = 2), upper = c(a = 2, b = 5),
    k = 2, distr = "tnorm",
    means = c(a = 1.3, b = 3), sds = c(a = 0.4, b = 0.1),
    cast_to_data_frame = F, add_id_column = FALSE
  )
  expect_equal(colnames(dat), c("a", "b"))
  expect_true(is.matrix(dat))


  dat <- simulate_values(
    lower = c(a = 1, b = 2), upper = c(a = 2, b = 5),
    k = 2, distr = "tnorm",
    means = c(a = 1.3, b = 3), sds = c(a = 0.4, b = 0.1),
    cast_to_data_frame = T, add_id_column = "character"
  )
  expect_equal(colnames(dat), c("a", "b", "ID"))
  expect_true(is.data.frame(dat))
  expect_true(is.character(dat[,"ID"]))


  # check the seed
  withr::local_preserve_seed()
  set.seed(1)
  test1 <- simulate_values(lower = c(1, 2), upper = c(2, 3), k = 2)
  test2 <- simulate_values(lower = c(1, 2), upper = c(2, 3), k = 2, seed = 1)
  expect_equal(test1, test2)
})

test_that("input checks for simulate_values", {
  # input checks
  expect_error(
    simulate_values(lower = c(), upper = 3, k = 2),
    "length >= 1"
  )
  expect_error(
    simulate_values(lower = c(1), upper = c(), k = 2),
    "length >= 1"
  )
  expect_error(
    simulate_values(lower = c(1), upper = c(2, 2), k = 2),
    "not of the same length"
  )
  expect_error(
    simulate_values(lower = c(a = 1, b = 1), upper = c(a = 2, c = 2), k = 2),
    "don't match"
  )
  expect_error(
    simulate_values(
      lower = c(a = 1, b = 1), upper = c(a = 2, b = 2),
      k = 2, distr = "foo"
    ),
    "should be one of"
  )
  expect_error(
    simulate_values(
      lower = c(a = 1, b = 1), upper = c(a = 2, b = 2),
      k = NULL, distr = "foo"
    ),
    "a single numeric"
  )
  expect_error(
    simulate_values(
      lower = c(a = 1, b = 1), upper = c(a = 2, b = 2),
      k = "a", distr = "foo"
    ),
    "a single numeric"
  )

  expect_error(
    simulate_values(
      lower = c(a = "a", b = 1), upper = c(a = 2, b = 2),
      k = "a", distr = "foo"
    ),
    "numeric"
  )
  expect_error(
    simulate_values(
      lower = c(a = "a", b = 1), upper = c(a = 2, b = 2),
      k = c(1, 2), distr = "foo"
    ),
    "numeric"
  )
  expect_error(
    simulate_values(
      lower = c(a = 1, b = 1), upper = c(a = 2, b = 2),
      k = 2, cast_to_data_frame = NULL
    ),
    "a single logical"
  )
  expect_error(
    simulate_values(
      lower = c(a = 1, b = 1), upper = c(a = 2, b = 2),
      k = 2, cast_to_data_frame = c(T, F)
    ),
    "a single logical"
  )

  expect_error(
    simulate_values(
      lower = c(a = 1, b = 1), upper = c(a = 2, b = 2),
      k = 2, add_id_column = "foo"
    ),
    "should be one of"
  )

  expect_error(
    simulate_values(
      lower = c(a = 1, b = 1), upper = c(a = 2, b = 2),
      k = 2, seed = "foo"
    ),
    "must be a single numeric"
  )

  expect_error(
    simulate_values(
      lower = c(a = 1, b = 1), upper = c(a = 2, b = 2),
      k = 2, seed = c(1, 2)
    ),
    "must be a single numeric"
  )

  expect_error(
    simulate_values(
      lower = c(a = 2, b = 1), upper = c(a = 2, b = 2),
      k = 2
    ),
    "values in lower are not always smaller"
  )


  expect_error(
    simulate_values(
      lower = c(a = 1, b = 1), upper = c(a = 2, b = 2),
      k = 2, distr = "tnorm"
    ),
    "no means argument"
  )

  expect_error(
    simulate_values(
      lower = c(a = 1, b = 1), upper = c(a = 2, b = 2),
      k = 2, distr = "tnorm", means = 1.5,
    ),
    "no sds argument"
  )

  expect_error(
    simulate_values(
      lower = c(a = 1, b = 1), upper = c(a = 2, b = 2),
      k = 2, distr = "tnorm", means = 1.5, sds = 0.4
    ),
    "means is not a valid numeric vector with length equal to lower/upper"
  )

  expect_error(
    simulate_values(
      lower = c(a = 1, b = 1), upper = c(a = 2, b = 2),
      k = 2, distr = "tnorm", means = "1.5", sds = 0.4
    ),
    "means is not a valid numeric vector with length equal to lower/upper"
  )

  expect_error(
    simulate_values(
      lower = c(a = 1, b = 1), upper = c(a = 2, b = 2),
      k = 2, distr = "tnorm", means = c(1.5, 1.5), sds = 0.4
    ),
    "sds is not a valid numeric vector with length equal to lower/upper"
  )

  expect_error(
    simulate_values(
      lower = c(a = 1, b = 1), upper = c(a = 2, b = 2),
      k = 2, distr = "tnorm", means = c(1.5, 1.5), sds = "0.4"
    ),
    "sds is not a valid numeric vector with length equal to lower/upper"
  )

  expect_error(
    simulate_values(
      lower = c(a = 1, b = 1), upper = c(a = 2, b = 2),
      k = 2, distr = "tnorm", means = c(a = 1.5, 1.5), sds = c(0.4, 0.4)
    ),
    "labels provided in means and sds don't match"
  )

  expect_error(
    simulate_values(
      lower = c(a = 1, b = 1), upper = c(a = 2, b = 2),
      k = 2, distr = "tnorm", means = c(c = 1.5, 1.5), sds = c(c = 0.4, 0.4)
    ),
    "labels provided in means/sds don't match with lower/upper"
  )

})
