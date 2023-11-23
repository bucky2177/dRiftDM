test_that("input checks estimate_model", {

  # first tests without data
  a_model = ratcliff_dm()

  expect_error(estimate_model("hallo"), "drift_dm")
  expect_warning(estimate_model(a_model, lower = c(1,0.2, 0.1),
                              upper = c(7, 0.8, 0.6)), "No data set")
  return_val = suppressWarnings(estimate_model(a_model, lower = c(1,0.2, 0.1),
                                               upper = c(7, 0.8, 0.6)))
  expect_identical(return_val, a_model)


  # first tests with data
  a_model = ratcliff_dm(obs_data = ratcliff_data)
  expect_error(estimate_model(a_model, lower = c(0.2, 0.1),
                              upper = c(7, 0.8, 0.6)), "length")

  expect_error(estimate_model(a_model, lower = c("1", 0.2, 0.1),
                              upper = c(7, 0.8, 0.6)), "numeric")

  expect_error(estimate_model(a_model, lower = c(0.2, 0.1),
                              upper = c(7, 0.8)), "number of free_prms")

  expect_error(estimate_model(a_model, lower = c(1,0.2, 0.1),
                              upper = c(7, 0.8,0.8), verbose = "a"),
               "logical")
  expect_error(estimate_model(a_model, lower = c(1,0.2, 0.1),
                              upper = c(7, 0.8,0.8), use_de_optim = "a"),
               "logical")
  expect_error(estimate_model(a_model, lower = c(1,0.2, 0.1),
                              upper = c(7, 0.8,0.8), use_nmkb = "a"),
               "logical")

  expect_warning(estimate_model(a_model, lower = c(1,0.2, 0.1),
                              upper = c(7, 0.8,0.8),
                              use_nmkb = F, use_de_optim = F),
               "No estimation done")

  return_val = suppressWarnings(estimate_model(a_model, lower = c(1,0.2, 0.1),
                                               upper = c(7, 0.8,0.8),
                                               use_nmkb = F, use_de_optim = F))
  expect_identical(return_val, a_model)

  expect_error(estimate_model(a_model, lower = c(1,0.2, 0.1),
                              upper = c(7, 0.8, 0.6), seed = "a"), "seed")
  expect_error(estimate_model(a_model, lower = c(1,0.2, 0.1),
                              upper = c(7, 0.8, 0.6), de_n_cores = "a"),
               "de_n_cores")

  expect_error(estimate_model(a_model, lower = c(1,0.2, 0.1),
                              upper = c(7, 0.8, 0.6), de_control = "a"),
               "de_control")
  expect_error(estimate_model(a_model, lower = c(1,0.2, 0.1),
                              upper = c(7, 0.8, 0.6), nmkb_control = "a"),
               "nmkb_control")

  # tests that use parallel processing etc.
  skip_on_cran()
  new_data = simulate_data(a_model, n = 3000, seed = 1)
  a_model = set_obs_data(a_model, new_data)
  expect_snapshot(
  estimate_model(a_model, lower = c(1,0.2, 0.1), upper = c(7, 0.8, 0.6),
                 use_de_optim = F, use_nmkb = T, verbose = T)
  )
})
