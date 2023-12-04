test_that("drift_dm summary and print format", {
  dm_fits_subjects <- load_fits_subjects(
    path = test_path("fixtures", "dm_fits_subjects"),
    detailed_info = F,
    fit_procedure_name = "test_case_saved", check_data = F
  )

  # standard print
  expect_snapshot(
    print(dm_fits_subjects)
  )


  # results from summary -> data frame of parmaeters
  sum_obj <- summary(dm_fits_subjects)


  prms <- as.data.frame(rbind(
    dm_fits_subjects$all_fits$`1`$prms_model,
    dm_fits_subjects$all_fits$`2`$prms_model
  ))
  prms <- cbind(Subject = c("1", "2"), prms)
  prms <- cbind(prms, log_like = c(
    dm_fits_subjects$all_fits$`1`$log_like_val,
    dm_fits_subjects$all_fits$`2`$log_like_val
  ))
  prms <- cbind(prms, AIC = c(
    dm_fits_subjects$all_fits$`1`$ic_vals[["AIC"]],
    dm_fits_subjects$all_fits$`2`$ic_vals[["AIC"]]
  ))
  prms <- cbind(prms, BIC = c(
    dm_fits_subjects$all_fits$`1`$ic_vals[["BIC"]],
    dm_fits_subjects$all_fits$`2`$ic_vals[["BIC"]]
  ))
  expect_identical(sum_obj$prms, prms)

  # print of the summray object
  expect_snapshot(
    print(sum_obj)
  )
})
