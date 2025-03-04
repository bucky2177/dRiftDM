# PRINT FUNCTIONS ---------------------------------------------------------


test_that("print.stats_dm works as expected", {
  some_stats <- calc_stats(
    dmc_dm(dx = .005, dt = .005),
    type = "cafs"
  )

  class(some_stats) <- class(some_stats)[-c(1, 2)]
  withr::local_preserve_seed()
  set.seed(1)
  expect_snapshot(
    print(some_stats,
      some = TRUE,
      print_rows = 3, show_header = FALSE, show_note = FALSE
    )
  )
})


test_that("print.cafs works as expected", {
  cafs_obj <- calc_stats(
    dmc_dm(dx = .005, dt = .005),
    type = "cafs"
  )
  expect_snapshot(
    print(cafs_obj)
  )
})


test_that("print.quantiles works as expected", {
  quantiles_obj <- calc_stats(
    dmc_dm(dx = .005, dt = .005),
    type = "quantiles"
  )
  expect_snapshot(
    print(quantiles_obj)
  )
})


test_that("print.delta_funs works as expected", {
  delta_funs_obj <- calc_stats(
    dmc_dm(dx = .005, dt = .005),
    type = "delta_funs", minuends = "incomp",
    subtrahends = "comp"
  )
  expect_snapshot(
    print(delta_funs_obj)
  )
})


test_that("print.fit_stats works as expected", {
  fits_ids <- load_fits_ids(test_path("fixtures"),
    fit_procedure_name = "test_case_saved"
  )
  fit_stats_obj <- calc_stats(fits_ids, type = "fit_stats")
  expect_snapshot(
    print(fit_stats_obj)
  )
})



test_that("print.stats_dm_list works as expected", {
  fits_ids <- load_fits_ids(test_path("fixtures"),
    fit_procedure_name = "test_case_saved"
  )
  stats_dm_list_obj <- calc_stats(fits_ids,
    type = c("fit_stats", "quantiles")
  )

  expect_snapshot(
    print(stats_dm_list_obj)
  )
})



# SUMMARY FUNCTIONS -------------------------------------------------------

test_that("summary.stats_dm works as expected", {
  fits_ids <- get_example_fits_ids()
  some_stats <- calc_stats(fits_ids,
    type = "fit_stats"
  )
  class(some_stats) <- class(some_stats)[-1]
  summary_stats <- summary(some_stats)

  # Check class
  expect_s3_class(summary_stats, "summary.stats_dm")

  # Check stored attributes
  expect_identical(summary_stats$type, "stats_dm")
  expect_type(summary_stats$summary_dataframe, "character")
  expect_s3_class(summary_stats$summary_dataframe, "table")

  # If ID column exists, check n_ids
  expect_identical(summary_stats$n_ids, length(unique(some_stats$ID)))

  # Check print output snapshot
  expect_snapshot(print(summary_stats))
})


test_that("summary.sum_dist works as expected", {
  some_stats <- calc_stats(dmc_dm(dx = .005, dt = .005), type = "cafs")
  class(some_stats) <- class(some_stats)[-1]
  summary_stats <- summary(some_stats)

  # Check class
  expect_s3_class(summary_stats, "summary.sum_dist")

  # Check stored attributes
  expect_identical(summary_stats$type, "sum_dist")
  expect_s3_class(summary_stats$summary_dataframe, "table")
  expect_identical(summary_stats$source, unique(some_stats$Source))

  # Check print output snapshot
  expect_snapshot(print(summary_stats))
})


test_that("summary.cafs works as expected", {
  fits_ids <- load_fits_ids(test_path("fixtures"),
    fit_procedure_name = "test_case_saved"
  )
  some_stats <- calc_stats(fits_ids, type = "cafs")
  summary_stats <- summary(some_stats, round_digits = 2)

  # Check class
  expect_s3_class(summary_stats, "summary.cafs")

  # Check stored attributes
  expect_identical(summary_stats$type, "cafs")
  expect_identical(summary_stats$bins, unique(some_stats$Bin))
  expect_identical(summary_stats$conds, unique(some_stats$Cond))
  expect_s3_class(summary_stats$summary_dataframe, "table")
  expect_identical(summary_stats$source, c("obs", "pred"))
  expect_identical(summary_stats$n_ids, length(unique(some_stats$ID)))


  # Check print output snapshot
  expect_snapshot(print(summary_stats))
})


test_that("summary.quantiles works as expected", {
  some_stats <- calc_stats(ulrich_flanker_data, type = "quantiles")
  summary_stats <- summary(some_stats)

  # Check class
  expect_s3_class(summary_stats, "summary.quantiles")

  # Check stored attributes
  expect_identical(summary_stats$type, "quantiles")
  expect_identical(summary_stats$probs, unique(some_stats$Prob))
  expect_identical(summary_stats$conds, conds(some_stats))
  expect_s3_class(summary_stats$summary_dataframe, "table")
  expect_identical(summary_stats$n_ids, length(unique(some_stats$ID)))
  expect_identical(summary_stats$source, "obs")

  # Check print output snapshot
  expect_snapshot(print(summary_stats))
})


test_that("summary.delta_funs works as expected", {
  some_stats <- calc_stats(dmc_dm(dt = .005, dx = .01),
    type = "delta_funs",
    minuends = "incomp", subtrahends = "comp"
  )
  summary_stats <- summary(some_stats)

  # Check class
  expect_s3_class(summary_stats, "summary.delta_funs")

  # Check stored attributes
  expect_identical(summary_stats$type, "delta_funs")
  expect_identical(summary_stats$probs, unique(some_stats$Prob))
  expect_identical(summary_stats$conds, conds(some_stats))
  expect_identical(summary_stats$source, "pred")
  expect_s3_class(summary_stats$summary_dataframe, "table")
  expect_identical(summary_stats$n_ids, NULL)


  # Check print output snapshot
  expect_snapshot(print(summary_stats))
})


test_that("summary.fit_stats works as expected", {
  fits_ids <- load_fits_ids(test_path("fixtures"),
    fit_procedure_name = "test_case_saved"
  )
  some_stats <- calc_stats(fits_ids, type = "fit_stats")
  summary_stats <- summary(some_stats)

  # Check class
  expect_s3_class(summary_stats, "summary.fit_stats")

  # Check stored attributes
  expect_identical(summary_stats$type, "fit_stats")
  expect_s3_class(summary_stats$summary_dataframe, "table")
  expect_identical(summary_stats$n_ids, length(unique(some_stats$ID)))

  # Check print output snapshot
  expect_snapshot(print(summary_stats))
})


test_that("summary.stats_dm_list works as expected", {
  some_stats_list <- calc_stats(dmc_dm(dx = .01, dt = .005),
    type = c("quantiles", "cafs")
  )
  summary_list <- summary(some_stats_list)

  # Check class
  expect_s3_class(summary_list, "summary.stats_dm_list")

  # Ensure each element is a summary object
  expect_s3_class(summary_list$quantiles, "summary.quantiles")
  expect_s3_class(summary_list$cafs, "summary.cafs")
  expect_length(summary_list, 2)


  # Check print output snapshot
  expect_snapshot(print(summary_list))
})
