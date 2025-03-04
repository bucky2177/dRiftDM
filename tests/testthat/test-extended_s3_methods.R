# DRIFT_DM ----------------------------------------------------------------

test_that("nobs.drift_dm works as expected", {
  a_model <- readRDS(test_path("fixtures", "ssp.rds"))
  some_data <- dRiftDM::ulrich_flanker_data[dRiftDM::ulrich_flanker_data$ID == 7, ]
  obs_data(a_model) <- some_data
  expect_identical(nobs(a_model), nrow(some_data))
})




# function testing done when testing the log_like function itself
test_that("logLik is formatted as expected", {
  a_model <- readRDS(test_path("fixtures", "dmc.rds"))
  log_like_obj <- logLik(a_model)

  expect_identical(class(log_like_obj), "logLik")
  expect_identical(attr(log_like_obj, "nobs"), 600L)
  expect_identical(attr(log_like_obj, "df"), 7)
  expect_identical(length(attributes(log_like_obj)), 3L)
})



test_that("coef.drift_dm returns values as expected", {
  a_model <- readRDS(test_path("fixtures", "dmc.rds"))

  coefs_unique <- coef(a_model)
  expect_equal(
    coefs_unique,
    c(
      muc = 4, b = 0.6, non_dec = 0.3, sd_non_dec = 0.02, tau = 0.04,
      A = 0.1, alpha = 4
    )
  )


  prms_matrix <- a_model$flex_prms_obj$prms_matrix
  expect_identical(
    coef(a_model, select_unique = FALSE, select_custom_prms = FALSE),
    prms_matrix
  )
  prms_matrix <- a_model$flex_prms_obj$prms_matrix
  prms_matrix <- cbind(prms_matrix, peak_l = prms_matrix[, "tau"])
  expect_identical(
    coef(a_model, select_unique = FALSE, select_custom_prms = TRUE),
    prms_matrix
  )
})



# FITS_IDS_DM -------------------------------------------------------------


test_that("coef.fits_ids_dm returns values as expected", {
  all_fits <- load_fits_ids(
    path = test_path("fixtures"),
    fit_procedure_name = "test_case_saved"
  )

  coefs_unique <- coef(all_fits)
  expect_true(is.data.frame(coefs_unique))
  expect_equal(names(coefs_unique), c("ID", "muc", "b", "non_dec"))

  coefs_all <- coef(all_fits, select_unique = F)
  expect_true(is.data.frame(coefs_all))
  expect_equal(names(coefs_all), c("ID", "Cond", "muc", "b", "non_dec"))
})


test_that("logLik|AIC|BIC.fits_ids_dm return values as expected", {
  all_fits <- load_fits_ids(
    path = test_path("fixtures"),
    fit_procedure_name = "test_case_saved"
  )

  logs <- logLik(all_fits)
  aics <- AIC(all_fits)
  bics <- BIC(all_fits)

  expect_true(is.data.frame(logs))
  expect_equal(names(logs), c("ID", "Log_Like"))

  expect_true(is.data.frame(aics))
  expect_equal(names(aics), c("ID", "AIC"))


  expect_true(is.data.frame(bics))
  expect_equal(names(bics), c("ID", "BIC"))
})



# UNPACK COEFS_DM ---------------------------------------------------------
