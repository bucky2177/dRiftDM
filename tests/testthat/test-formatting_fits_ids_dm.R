test_that("print.fits_ids_dm (old) works as expected", {

  # old test cases (v. 0.2.2, deprecated)
  expect_warning(
    suppress_lifecycle_deprecated(
      drift_dm_fits <- load_fits_ids(
        path = test_path("fixtures", "drift_dm_fits"),
        detailed_info = FALSE,
        fit_procedure_name = "test_case_saved", check_data = TRUE
      )
    ), "Validating model"
  )
  expect_snapshot(
    print(drift_dm_fits)
  )
})

test_that("print.fits_ids_dm (new) works as expected", {

  # new test cases
  drift_dm_fits <- get_example_fits("fits_ids")
  expect_snapshot(
    print(drift_dm_fits)
  )
})


test_that("summary.fits_ids_dm (old) and print", {

  # old test cases (v. 0.2.2, deprecated)
  suppress_lifecycle_deprecated(
    drift_dm_fits <- load_fits_ids(
      path = test_path("fixtures", "drift_dm_fits"),
      detailed_info = FALSE, fit_procedure_name = "test_case_saved",
      check_data = FALSE
    )
  )

  # results from summary -> data frame of parameters
  sum_obj <- summary(drift_dm_fits)


  expect_identical(
    names(sum_obj),
    c(
      "fit_procedure_name", "time_call", "lower", "upper", "model_type", "prms",
      "stats", "N"
    )
  )

  expect_identical(
    sum_obj$lower,
    c(muc = 1, b = 0.3, non_dec = 0.1)
  )

  expect_identical(
    sum_obj$upper,
    c(muc = 5, b = 0.8, non_dec = 0.5)
  )

  prms <- coef(drift_dm_fits, select_unique = FALSE)
  expect_identical(sum_obj$prms, prms)

  expect_identical(sum_obj$N, 2L)

  # one stats
  m_b <- mean(sapply(
    drift_dm_fits$all_fits,
    \(x) x$flex_prms_obj$prms_matrix[1, "b"]
  ))

  err_b <- sd(sapply(
    drift_dm_fits$all_fits,
    \(x) x$flex_prms_obj$prms_matrix[1, "b"]
  )) / sqrt(2)

  expect_identical(sum_obj$stats$null[["b"]], c(m_b, err_b))

  # print of the summray object
  expect_snapshot(
    print(sum_obj)
  )
})

test_that("summary.fits_ids_dm (new) and print", {

  # new test cases (v. >0.3.x)
  fits_ids <- get_example_fits("fits_ids")
  sum_obj <- summary(fits_ids)

  expect_s3_class(sum_obj, "summary.fits_ids_dm")
  exp_entries <- c("prms", "stats", "summary_drift_dm_obj", "optimizer",
                   "conv_info", "obs_data")
  expect_true(all(exp_entries %in% names(sum_obj)))

  # check prms
  exp_prms <- coef(fits_ids, select_unique = FALSE)
  expect_identical(exp_prms, sum_obj$prms)

  # check summary model
  exp_sum_model <- unclass(summary(fits_ids$drift_dm_fit_info$drift_dm_obj))
  expect_identical(exp_sum_model, sum_obj$summary_drift_dm_obj)

  # Check means and standard error for one cond and one prm
  test_cond <- "comp"
  stats <- sum_obj$stats[[test_cond]]
  test_coef <- "non_dec"
  m <- mean(sapply(
    fits_ids$all_fits,
    \(x) x$flex_prms_obj$prms_matrix[test_cond, test_coef]
  ))

  err <- sd(sapply(
    fits_ids$all_fits,
    \(x) x$flex_prms_obj$prms_matrix[test_cond, test_coef]
  )) / sqrt(length(fits_ids$all_fits))

  expect_equal(stats[[test_coef]], c(m, err))

  # obs_data summary
  # -> N
  expect_equal(sum_obj$obs_data$N, length(fits_ids$all_fits))

  # -> avg trials
  all_data = lapply(names(fits_ids$all_fits), \(x, ...) {
    data = obs_data(fits_ids$all_fits[[x]], ...)
    data$ID = x
    return(data)
  }, messaging = FALSE)
  all_data = do.call(rbind, all_data)
  exp_avg = colMeans(table(all_data$ID, all_data$Cond))
  expect_equal(sum_obj$obs_data$avg_trials, exp_avg)

  # optimizer
  expect_true(is.character(sum_obj$optimizer))
  expect_equal(length(sum_obj$optimizer), 1L)

  # conv_infos
  expect_equal(names(sum_obj$conv_info), c("not_conv", "messages"))
})


test_that("get_avg_trials computes average trials correctly", {
  df <- data.frame(
    ID   = c(1,1,2,2,3,3,3,3),
    Cond = c("A","B","A","B","A","A","B","B")
  )
  # ID 1: A=1, B=1
  # ID 2: A=1, B=1
  # ID 3: A=2, B=2
  # averages: A = (1+1+2)/3 = 1.33..., B = (1+1+2)/3 = 1.33...

  res <- get_avg_trials(df)

  expect_equal(res$N, 3)
  expect_named(res$avg_trials, c("A","B"))
  expect_equal(unname(res$avg_trials["A"]), (1+1+2)/3)
  expect_equal(unname(res$avg_trials["B"]), (1+1+2)/3)
})
