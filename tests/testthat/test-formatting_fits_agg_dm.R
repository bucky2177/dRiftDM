test_that("print.fits_agg_dm prints header via summary and returns invisibly", {
  fits_agg <- get_example_fits("fits_agg")

  # snapshot the printed header produced by print.fits_agg_dm
  expect_snapshot(print(fits_agg))
})

test_that("print.summary.fits_agg_dm prints header only when requested", {
  fits_agg <- get_example_fits("fits_agg")
  sum_obj  <- summary(fits_agg)

  # header only
  expect_snapshot(print(sum_obj, just_header = TRUE))

  # full print with default rounding
  expect_snapshot(print(sum_obj))

  # full print with specific rounding
  expect_snapshot(print(sum_obj, round_digits = 2))
})


test_that("summary.fits_agg_dm works as expected", {
  fits_agg <- get_example_fits("fits_agg")
  sum_obj <- summary(fits_agg)

  # class and names
  expect_identical(class(sum_obj), "summary.fits_agg_dm")
  expect_identical(
    names(sum_obj),
    c("summary_drift_dm_obj", "prms", "obs_data")
  )

  # content: matches underlying objects/helpers
  expect_identical(
    sum_obj$summary_drift_dm_obj,
    unclass(summary(fits_agg$drift_dm_obj))
  )
  expect_identical(
    sum_obj$prms,
    coef(fits_agg$drift_dm_obj, select_unique = FALSE)
  )

  expected_obs <- get_avg_trials(fits_agg$obs_data_ids)
  expect_identical(sum_obj$obs_data, expected_obs)
})

test_that("summary.fits_agg_dm respects select_unique", {
  fits_agg <- get_example_fits("fits_agg")

  sum_all   <- summary(fits_agg, select_unique = FALSE)
  sum_uniq  <- summary(fits_agg, select_unique = TRUE)

  expect_identical(
    sum_all$prms,
    coef(fits_agg$drift_dm_obj, select_unique = FALSE)
  )
  expect_identical(
    sum_uniq$prms,
    coef(fits_agg$drift_dm_obj, select_unique = TRUE)
  )

  # other fields are unaffected by select_unique
  expect_identical(sum_all$summary_drift_dm_obj, sum_uniq$summary_drift_dm_obj)
  expect_identical(sum_all$obs_data, sum_uniq$obs_data)
})
