test_that("AIC, BIC, logLik, coef works", {
  test_fits <- load_fits_ids(test_path("fixtures", "dm_fits_ids"),
    fit_procedure_name = "test_case_saved"
  )
  one_model <- test_fits$all_fits[[1]]

  # AIC
  expect_equal(one_model$ic_vals[["aic"]], AIC(one_model))

  # BIC
  expect_equal(one_model$ic_vals[["bic"]], BIC(one_model))


  # logLik
  expect_equal(one_model$log_like_val, logLik(one_model))


  # coefs
  expect_equal(coef(one_model), one_model$prms_model)
})
