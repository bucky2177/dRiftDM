test_that("print.drift_dm works as expected", {
  a_dmc_model <- readRDS(test_path("fixtures", "dmc.rds"))
  expect_snapshot(
    print(a_dmc_model)
  )
})


test_that("summary.drift_dm works as expected", {
  a_dmc_model <- readRDS(test_path("fixtures", "dmc.rds"))
  summary_model <- summary(a_dmc_model)
  expect_identical(summary_model$class, c("dmc_dm", "drift_dm"))
  expect_identical(
    summary_model$summary_flex_prms,
    summary(a_dmc_model$flex_prms_obj) # checked in respective file
  )
  expect_identical(summary_model$prms_solve, a_dmc_model$prms_solve)
  expect_identical(summary_model$solver, a_dmc_model$solver)
  expect_identical(
    summary_model$fit_stats,
    c(
      "Log_Like" = logLik(a_dmc_model),
      "AIC" = AIC(a_dmc_model),
      "BIC" = BIC(a_dmc_model)
    )
  )

  summary_data <- cbind(
    summary(a_dmc_model$obs_data$rts_u$comp),
    summary(a_dmc_model$obs_data$rts_u$incomp),
    summary(a_dmc_model$obs_data$rts_l$comp),
    summary(a_dmc_model$obs_data$rts_l$incomp)
  )
  summary_data <- t(summary_data)
  colnames(summary_data) <- tolower(colnames(summary_data))
  rownames(summary_data) <- c(
    "corr comp", "corr incomp",
    "err comp", "err incomp"
  )
  summary_data <- cbind(
    summary_data,
    c(
      length(a_dmc_model$obs_data$rts_u$comp),
      length(a_dmc_model$obs_data$rts_u$incomp),
      length(a_dmc_model$obs_data$rts_l$comp),
      length(a_dmc_model$obs_data$rts_l$incomp)
    )
  )
  colnames(summary_data)[7] <- "n"
  expect_identical(summary_model$obs_data, summary_data)
  expect_snapshot(
    print(summary_model)
  )

  expect_identical(
    names(summary_model),
    c("class", "summary_flex_prms", "prms_solve", "solver", "obs_data",
      "fit_stats")
  )
})
