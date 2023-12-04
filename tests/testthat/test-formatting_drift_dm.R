test_that("print works as expected", {
  a_dmc_model <- dmc_dm()
  expect_snapshot(
    print(a_dmc_model)
  )
})


test_that("summary works as expected", {
  a_dmc_model <- dmc_dm(obs_data = simon_data, dt = .005, dx = .005)
  a_dmc_model <- re_evaluate_model(a_dmc_model)
  summary_model <- summary(a_dmc_model)
  expect_identical(summary_model$class, c("dmc_dm", "drift_dm"))
  expect_identical(
    summary_model$nPrms,
    c(
      "total" = length(a_dmc_model$prms_model),
      "free" = length(a_dmc_model$free_prms)
    )
  )
  prms <- t(as.matrix(a_dmc_model$prms_model))
  rownames(prms) <- ""
  expect_identical(summary_model$prms_model, prms)
  expect_identical(summary_model$conds, a_dmc_model$conds)
  expect_identical(summary_model$prms_solve, a_dmc_model$prms_solve)
  expect_identical(summary_model$free_prms, a_dmc_model$free_prms)
  expect_identical(summary_model$solver, a_dmc_model$solver)
  expect_identical(
    summary_model$fit_stats,
    c(
      "log(like)" = a_dmc_model$log_like_val,
      "aic" = a_dmc_model$ic_vals[["AIC"]],
      "bic" = a_dmc_model$ic_vals[["BIC"]]
    )
  )

  summary_data <- cbind(
    summary(a_dmc_model$obs_data$rts_corr$comp),
    summary(a_dmc_model$obs_data$rts_corr$incomp),
    summary(a_dmc_model$obs_data$rts_err$comp),
    summary(a_dmc_model$obs_data$rts_err$incomp)
  )
  summary_data <- t(summary_data)
  colnames(summary_data) <- tolower(colnames(summary_data))
  rownames(summary_data) <- c(
    "correct comp", "correct incomp",
    "error comp", "error incomp"
  )
  summary_data <- cbind(
    summary_data,
    c(
      length(a_dmc_model$obs_data$rts_corr$comp),
      length(a_dmc_model$obs_data$rts_corr$incomp),
      length(a_dmc_model$obs_data$rts_err$comp),
      length(a_dmc_model$obs_data$rts_err$incomp)
    )
  )
  colnames(summary_data)[7] <- "n"
  expect_identical(summary_model$obs_data, summary_data)
  expect_snapshot(
    print(summary_model)
  )
})
