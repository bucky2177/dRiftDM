test_that("drift_dm summary and print format", {
  dm_fits_ids <- load_fits_ids(
    path = test_path("fixtures", "dm_fits_ids"),
    detailed_info = F,
    fit_procedure_name = "test_case_saved", check_data = F
  )

  # standard print
  expect_snapshot(
    print(dm_fits_ids)
  )


  # results from summary -> data frame of parmaeters
  sum_obj <- summary(dm_fits_ids)


  prms <- as.data.frame(rbind(
    dm_fits_ids$all_fits$`1`$prms_model,
    dm_fits_ids$all_fits$`2`$prms_model
  ))
  prms <- cbind(ID = c(1, 2), prms)
  prms <- cbind(prms, log_like = c(
    dm_fits_ids$all_fits$`1`$log_like_val,
    dm_fits_ids$all_fits$`2`$log_like_val
  ))
  prms <- cbind(prms, aic = c(
    dm_fits_ids$all_fits$`1`$ic_vals[["aic"]],
    dm_fits_ids$all_fits$`2`$ic_vals[["aic"]]
  ))
  prms <- cbind(prms, bic = c(
    dm_fits_ids$all_fits$`1`$ic_vals[["bic"]],
    dm_fits_ids$all_fits$`2`$ic_vals[["bic"]]
  ))
  expect_identical(sum_obj$prms, prms)

  # print of the summray object
  expect_snapshot(
    print(sum_obj)
  )
})

