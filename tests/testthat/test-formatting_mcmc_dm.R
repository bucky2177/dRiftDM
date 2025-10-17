test_that("print.mcmc works as expected", {
  # old variant -> deprecated way a model is structured
  mcmc_obj <- get_example_fits("mcmc_dm")
  expect_snapshot(
    print(mcmc_obj)
  )
})


test_that("summary.mcmc_dm works as expected (full object)", {
  mcmc_obj <- get_example_fits("mcmc_dm")
  summary_obj <- summary(mcmc_obj)

  # basic structure
  expect_identical(class(summary_obj), "summary.mcmc_dm")
  expect_identical(
    names(summary_obj),
    c("general", "statistics", "quantiles", "gr", "eff_n")
  )

  # general info
  chains <- get_subset_chains(chains_obj = mcmc_obj, id = NULL)
  expect_null(summary_obj$general$id)
  expect_identical(summary_obj$general$sampler, attr(mcmc_obj, "sampler"))
  expect_identical(
    summary_obj$general$hierarchical,
    attr(mcmc_obj, "hierarchical")
  )
  expect_identical(summary_obj$general$n_param, dim(chains)[1])
  expect_identical(summary_obj$general$n_chains, dim(chains)[2])
  expect_identical(summary_obj$general$n_iter, dim(chains)[3])

  # coda-based summaries match what we would compute ourselves
  mcmc_list <- mcmc_dm_to_coda_mcmc(chains)
  summary_coda <- summary(mcmc_list)
  expect_identical(summary_obj$statistics, summary_coda$statistics)
  expect_identical(summary_obj$quantiles, summary_coda$quantiles)

  gr_expected <- coda::gelman.diag(mcmc_list, autoburnin = FALSE)$psrf
  effn_expected <- coda::effectiveSize(mcmc_list)
  expect_identical(summary_obj$gr, gr_expected)
  expect_identical(summary_obj$eff_n, effn_expected)

  # snapshot the human-readable printout
  expect_snapshot(print(summary_obj))
})


test_that("summary.mcmc_dm passes ... to coda::summary (quantiles)", {
  mcmc_obj <- get_example_fits("mcmc_dm")

  # choose non-default quantiles to verify passthrough
  probs <- c(0.10, 0.50, 0.90)
  summary_custom <- summary(mcmc_obj, quantiles = probs)

  # columns should reflect our requested quantiles
  expect_identical(colnames(summary_custom$quantiles), c("10%", "50%", "90%"))

  # and match a direct call to coda with the same probs
  chains <- get_subset_chains(chains_obj = mcmc_obj, id = NULL)
  mcmc_list <- mcmc_dm_to_coda_mcmc(chains)
  summary_coda_custom <- summary(mcmc_list, quantiles = probs)
  expect_identical(summary_custom$quantiles, summary_coda_custom$quantiles)
  expect_identical(summary_custom$statistics, summary_coda_custom$statistics)
})
