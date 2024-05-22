test_that("AIC, BIC, logLik, coef works", {
  test_fits =  load_fits_ids(test_path("fixtures", "dm_fits_ids"),
                             fit_procedure_name = "test_case_saved"
  )

  # AIC
  aics = AIC(test_fits)
  expect_equal(data.frame(ID = c(1,2),
             aic = c(test_fits$all_fits[["1"]]$ic_vals[["aic"]],
                     test_fits$all_fits[["2"]]$ic_vals[["aic"]])),
             aics)

  # BIC
  bics = BIC(test_fits)
  expect_equal(data.frame(ID = c(1,2),
                          bic = c(test_fits$all_fits[["1"]]$ic_vals[["bic"]],
                                  test_fits$all_fits[["2"]]$ic_vals[["bic"]])),
               bics)

  # logLik
  log_likes = logLik(test_fits)
  expect_equal(data.frame(ID = c(1,2),
                          log_like = c(test_fits$all_fits[["1"]]$log_like_val,
                                  test_fits$all_fits[["2"]]$log_like_val)),
               log_likes)

  # coefs
  coefs = coef(test_fits)
  gathered_coefs = gather_parameters(test_fits, fit_stats = F)
  expect_equal(coefs, gathered_coefs)
})
