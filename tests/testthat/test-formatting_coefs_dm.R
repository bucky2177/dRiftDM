test_that("print.coefs_dm works as expected", {
  fits_ids <- load_fits_ids(
    test_path("fixtures"),
    fit_procedure_name = "test_case_saved"
  )
  coefs <- coef(fits_ids)

  expect_snapshot(
    print(coefs, round_digits = 2)
  )

  withr::local_preserve_seed()
  set.seed(1)
  coefs = rbind(coefs, coefs, coefs)
  coefs$ID = 1:nrow(coefs)

  expect_snapshot(
    print(coefs, round_digits = 2, some = TRUE, print_rows = 3)
  )

})


test_that("summary.coefs_dm works as expected", {
  fits_ids <- load_fits_ids(
    test_path("fixtures"),
    fit_procedure_name = "test_case_saved"
  )
  coefs <- coef(fits_ids)
  summary_coefs <- summary(coefs)

  # Check class
  expect_s3_class(summary_coefs, "summary.coefs_dm")

  # Check stored attributes
  expect_identical(summary_coefs$type, "coefs_dm")
  expect_s3_class(summary_coefs$summary_dataframe, "table")
  expect_identical(summary_coefs$n_ids, 2L)

  # Check print output snapshot
  expect_snapshot(print(summary_coefs))

  # Check structure
  expect_identical(
    names(summary_coefs),
    c("type", "summary_dataframe", "n_ids")
  )
})
