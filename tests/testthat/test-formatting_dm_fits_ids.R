test_that("print.drift_dm_fits works as expected", {
  dm_fits_ids <- load_fits_ids(
    path = test_path("fixtures", "dm_fits_ids"),
    detailed_info = F,
    fit_procedure_name = "test_case_saved", check_data = F
  )

  # standard print
  expect_snapshot(
    print(dm_fits_ids)
  )
})


test_that("summary.drift_dm_fits and print", {

  dm_fits_ids <- load_fits_ids(
    path = test_path("fixtures", "dm_fits_ids"),
    detailed_info = F,
    fit_procedure_name = "test_case_saved", check_data = F
  )

  # results from summary -> data frame of parameters
  sum_obj <- summary(dm_fits_ids)


  expect_identical(
    names(sum_obj),
    c("fit_procedure_name", "time_call", "lower", "upper", "model_type", "prms",
      "stats", "N")
  )

  expect_identical(
    sum_obj$lower,
    c(muc = 1, b = 0.3, non_dec = 0.1)
  )

  expect_identical(
    sum_obj$upper,
    c(muc = 5, b = 0.8, non_dec = 0.5)
  )

  prms <- coef(dm_fits_ids, select_unique = F)
  expect_identical(sum_obj$prms, prms)

  expect_identical(sum_obj$N, 2L)

  # one stats
  m_b = mean(sapply(dm_fits_ids$all_fits,
                    \(x) x$flex_prms_obj$prms_matrix[1,"b"]))

  err_b = sd(sapply(dm_fits_ids$all_fits,
                      \(x) x$flex_prms_obj$prms_matrix[1,"b"])) / sqrt(2)

  expect_identical(sum_obj$stats$null[["b"]], c(m_b, err_b))

  # print of the summray object
  expect_snapshot(
    print(sum_obj)
  )
})
