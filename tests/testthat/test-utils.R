test_that("global variables are as expected", {
  expect_identical(drift_dm_approx_error(), 1e-20)
  expect_identical(drift_dm_small_approx_error(), .01)
  expect_identical(drift_dm_rough_approx_error(), .1)
  expect_identical(drift_dm_robust_prm(), 1e-10)
  expect_identical(drift_dm_default_rounding(), 3)
})


test_that("prms_to_str works as expected", {
  withr::local_seed(1)
  expect_snapshot(
    prms_to_str(rnorm(3), c("as", "bas", "mu_"))
  )
})
