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
  c(
    muc = 3.5, b = 0.5, non_dec = 0.4, sd_non_dec = 0.04, tau = 0.08,
    A = 0.15, alpha = 4
  )
)
dmc_synth_data <- simulate_data(a_dmc_model, n = 300, seed = 4)
usethis::use_data(dmc_synth_data, overwrite = TRUE)


# get data of DMCfun
ulrich_flanker_data <- DMCfun::flankerData$data
ulrich_flanker_data <- ulrich_flanker_data[!ulrich_flanker_data$outlier, ]
ulrich_flanker_data <- ulrich_flanker_data[c("Subject", "RT", "Error", "Comp")]
names(ulrich_flanker_data) <- c("ID", "RT", "Error", "Cond")
ulrich_flanker_data$RT <- ulrich_flanker_data$RT / 1000
ulrich_flanker_data$Error <- as.numeric(ulrich_flanker_data$Error)
usethis::use_data(ulrich_flanker_data, overwrite = TRUE)

ulrich_simon_data <- DMCfun::simonData$data
ulrich_simon_data <- ulrich_simon_data[!ulrich_simon_data$outlier, ]
ulrich_simon_data <- ulrich_simon_data[c("Subject", "RT", "Error", "Comp")]
names(ulrich_simon_data) <- c("ID", "RT", "Error", "Cond")
ulrich_simon_data$RT <- ulrich_simon_data$RT / 1000
ulrich_simon_data$Error <- as.numeric(ulrich_simon_data$Error)
usethis::use_data(ulrich_simon_data, overwrite = TRUE)
