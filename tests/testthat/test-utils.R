test_that("global variables are as expected", {
  expect_identical(drift_dm_approx_error(), 1e-20)
  expect_identical(drift_dm_small_approx_error(), .01)
  expect_identical(drift_dm_rough_approx_error(), .1)
  expect_identical(drift_dm_robust_prm(), 1e-12)
  expect_identical(drift_dm_default_rounding(), 3)
})


test_that("prms_to_str works as expected", {
  withr::local_seed(1)
  expect_snapshot(
    prms_to_str(rnorm(3), c("as", "bas", "mu_"))
  )
})

test_that("check_if_named_vector input checks", {
  expect_error(
    check_if_named_numeric_vector(c("1", 2, 3), "x"),
    "numeric vector"
  )
  expect_error(
    check_if_named_numeric_vector(c(1, 2, 3), "x", length = 2),
    "2 entries"
  )
  expect_error(
    check_if_named_numeric_vector(c(1, 2, 3), "x", length = 3),
    "ensure that x is a named vector"
  )
  expect_error(
    check_if_named_numeric_vector(c(a = 1, b = 2, c = 3), "x",
      labels = c("a", "x", "z")
    ),
    "can not be adressed"
  )

  expect_error(
    check_if_named_numeric_vector(c(a = NA, b = 2, c = 3), "x"),
    "NAs"
  )
})

test_that("prms_to_str input checks", {
  expect_error(prms_to_str(
    prms = rnorm(3), names_prms = c("as", "bas", "mu_"),
    round_digits = NA
  ), "not of type numeric")
  expect_error(prms_to_str(
    prms = rnorm(3), names_prms = c(1, 2, 3),
    round_digits = 3
  ), "not of type character")
  expect_error(prms_to_str(
    prms = c("1", 2, 3), names_prms = c("a", "b", "c"),
    round_digits = 3
  ), "not of type numeric")

  expect_error(prms_to_str(
    prms = c(1, 2, 3), names_prms = c("a", "b"),
    round_digits = 3
  ), "don't match")

  expect_error(prms_to_str(
    prms = numeric(), names_prms = character(),
    round_digits = 3
  ), "are of length zero")

  expect_error(
    prms_to_str(
      prms = c(1, 2, 3), names_prms = c("a", "b", "d"),
      round_digits = 3, collapse = NA
    ),
    "not of type character"
  )

  expect_identical(
    prms_to_str(
      prms = c(1, 2, 3), names_prms = c("a", "b", "d"),
      round_digits = 3, collapse = c(";", "!"),
      sep = c("!", "#")
    ),
    "a!1;b!2;d!3"
  )
})
