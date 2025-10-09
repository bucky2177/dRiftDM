test_that("print.drift_dm (old) works as expected", {
  # old variant -> deprecated way a model is structured
  a_model <- readRDS(
    test_path("fixtures", "drift_dm_fits", "test_case_saved_v022", "1.rds")
  )
  expect_snapshot(
    print(a_model)
  )
})

test_that("print.drift_dm (new) works as expected", {
  # new variant -> new way a model is structured
  a_model <- get_example_fits("fits_ids")[[2]][[1]]
  expect_snapshot(
    print(a_model)
  )
})

test_that("summary.drift_dm (old) works as expected", {
  a_model <- readRDS(
    test_path("fixtures", "drift_dm_fits", "test_case_saved_v022", "1.rds")
  )
  summary_model <- summary(a_model)
  expect_identical(summary_model$class, c("ratcliff_dm", "drift_dm"))
  expect_identical(
    summary_model$summary_flex_prms,
    summary(a_model$flex_prms_obj) # checked in respective file
  )
  expect_identical(summary_model$prms_solve, a_model$prms_solve)
  expect_identical(summary_model$solver, a_model$solver)
  expect_identical(summary_model$b_coding, attr(a_model, "b_coding"))

  summary_data <- cbind(
    summary(a_model$obs_data$rts_u$null),
    summary(a_model$obs_data$rts_l$null)
  )
  summary_data <- t(summary_data)
  colnames(summary_data) <- tolower(colnames(summary_data))
  rownames(summary_data) <- c("corr null", "err null")
  summary_data <- cbind(
    summary_data,
    c(length(a_model$obs_data$rts_u$null), length(a_model$obs_data$rts_l$null))
  )
  colnames(summary_data)[7] <- "n"
  expect_identical(summary_model$obs_data, summary_data)
  expect_snapshot(
    print(summary_model)
  )

  expect_identical(
    names(summary_model),
    c(
      "class", "summary_flex_prms", "prms_solve", "solver", "b_coding",
      "obs_data", "fit_stats"
    )
  )
})
