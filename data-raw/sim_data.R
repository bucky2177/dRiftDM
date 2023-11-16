# create a sample data with just one condition ("null)
a_ratcliff_model <- ratcliff_dm(dt = 0.001, dx = 0.01)

a_ratcliff_model <- set_model_prms(
  a_ratcliff_model,
  c(3, 0.4, 0.3)
)

ratcliff_data <- simulate_data(a_ratcliff_model, n = 300, seed = 2)

usethis::use_data(ratcliff_data, overwrite = TRUE)


# create a sample data with two conditions ("comp" and "incomp")
a_dmc_model <- dmc_dm(dt = 0.001, dx = 0.01)
a_dmc_model <- set_model_prms(
  a_dmc_model,
  c(3.5, 0.5, 0.3, 0.02, 0.04, 0.1, 4)
)
simon_data <- simulate_data(a_dmc_model, n = 300, seed = 2)
usethis::use_data(simon_data, overwrite = TRUE)
