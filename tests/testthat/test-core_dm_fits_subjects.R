test_that("gathering_parameters works as expected", {
  fits <- load_fits_subjects(test_path("fixtures", "dm_fits_subjects"),
    fit_procedure_name = "test_case_saved"
  )

  prms <- gather_parameters(fits)
  prms_1 <- c(fits$all_fits$`1`$prms_model,
    log_like = fits$all_fits$`1`$log_like_val,
    fits$all_fits$`1`$ic_vals
  )
  prms_2 <- c(fits$all_fits$`2`$prms_model,
    log_like = fits$all_fits$`2`$log_like_val,
    fits$all_fits$`2`$ic_vals
  )

  prms_test <- rbind(prms_1, prms_2)
  prms_test <- as.data.frame(prms_test)
  prms_test <- data.frame(Subject = c("1", "2"), prms_test)
  rownames(prms_test) <- 1:2
  expect_identical(prms, prms_test)
})



test_that("gathering_stats works as expected", {
  fits <- load_fits_subjects(test_path("fixtures", "dm_fits_subjects"),
    fit_procedure_name = "test_case_saved"
  )


  # quantiles
  quantiles <- gather_stats(fits, type = "quantiles")

  quantiles_1 <- calc_stats(fits$all_fits$`1`, type = "quantiles")
  quantiles_1 <- data.frame(Subject = "1", quantiles_1)
  quantiles_2 <- calc_stats(fits$all_fits$`2`, type = "quantiles")
  quantiles_2 <- data.frame(Subject = "2", quantiles_2)
  quantiles_test <- rbind(quantiles_1, quantiles_2)
  quantiles_test <- quantiles_test[order(quantiles_test$Subject), ]
  expect_identical(quantiles, quantiles_test)


  # cafs
  cafs <- gather_stats(fits, type = "cafs")

  cafs_1 <- calc_stats(fits$all_fits$`1`, type = "cafs")
  cafs_1 <- data.frame(Subject = "1", cafs_1)
  cafs_2 <- calc_stats(fits$all_fits$`2`, type = "cafs")
  cafs_2 <- data.frame(Subject = "2", cafs_2)
  cafs_test <- rbind(cafs_1, cafs_2)
  cafs_test <- cafs_test[order(cafs_test$Subject), ]
  expect_identical(cafs, cafs_test)


  # both cafs and quantiles
  both <- gather_stats(fits, type = c("cafs", "quantiles"))
  expect_identical(
    list(cafs = cafs, quantiles = quantiles),
    both
  )

  # only one type of information
  # quantiles
  quantiles <- gather_stats(fits, type = "quantiles", source = "obs")
  expect_snapshot(print(quantiles))

  quantiles_1 <- calc_stats(fits$all_fits$`1`,
    type = "quantiles",
    source = "obs"
  )
  quantiles_1 <- data.frame(Subject = "1", quantiles_1)
  quantiles_2 <- calc_stats(fits$all_fits$`2`,
    type = "quantiles",
    source = "obs"
  )
  quantiles_2 <- data.frame(Subject = "2", quantiles_2)
  quantiles_test <- rbind(quantiles_1, quantiles_2)
  quantiles_test <- quantiles_test[order(quantiles_test$Subject), ]
  expect_identical(quantiles, quantiles_test)
})
