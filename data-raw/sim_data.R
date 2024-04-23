# create a sample data with just one condition ("null)
a_ratcliff_model <- ratcliff_dm(dt = 0.001, dx = 0.001)

a_ratcliff_model <- set_model_prms(
  a_ratcliff_model,
  c(muc = 3, b = 0.4, non_dec = 0.3)
)
ratcliff_synth_data <- simulate_data(a_ratcliff_model, n = 300, seed = 2)

usethis::use_data(ratcliff_synth_data, overwrite = TRUE)


# create a sample data with two conditions ("comp" and "incomp")
a_dmc_model <- dmc_dm(dt = 0.001, dx = 0.001)
a_dmc_model <- set_model_prms(
  a_dmc_model,
  c(muc = 3.5, b = 0.5, non_dec = 0.3, sd_non_dec = 0.02, tau = 0.04,
    A = 0.1, alpha = 4)
)
dmc_synth_data <- simulate_data(a_dmc_model, n = 300, seed = 2)
usethis::use_data(dmc_synth_data, overwrite = TRUE)


# get data of DMCfun
ulrich_flanker_data = DMCfun::flankerData$data
ulrich_flanker_data = ulrich_flanker_data[c("Subject", "RT", "Error", "Comp")]
names(ulrich_flanker_data) = c("Subject", "RT", "Error", "Cond")
usethis::use_data(ulrich_flanker_data, overwrite = TRUE)

ulrich_simon_data = DMCfun::simonData$data
ulrich_simon_data = ulrich_simon_data[c("Subject", "RT", "Error", "Comp")]
names(ulrich_simon_data) = c("Subject", "RT", "Error", "Cond")
usethis::use_data(ulrich_simon_data, overwrite = TRUE)
