

# FITS_SEP ----------------------------------------------------------------


ddm = dmc_dm(dx = 0.01, dt = .005)
ulrich_flanker_data = ulrich_flanker_data
ulrich_flanker_data = ulrich_flanker_data[ulrich_flanker_data$ID %in% c(1,2,3),]
fits_ids = estimate_dm(drift_dm_obj = ddm, obs_data = ulrich_flanker_data,
                       verbose = 0, progress = 1)

use_directory("inst")
saveRDS(object = fits_ids, file = file.path("inst", "example_fits_ids.rds"))

# FITS_AGG ----------------------------------------------------------------

ddm = ratcliff_dm()
lower = c(4, 0.4, 0.2)
upper = c(6, 0.7, 0.4)
obs_data = simulate_data(ddm, n = 100, k = 3, lower = lower, upper = upper)

prms_solve(ddm)[c("dx", "dt")] = .005
fits_agg = estimate_dm(drift_dm_obj = ddm,
                       obs_data = obs_data$synth_data,
                       approach = "aggregated",
                       lower = lower, upper = upper,
                       verbose = 0)


use_directory("inst")
saveRDS(object = fits_agg, file = file.path("inst", "example_fits_agg.rds"))

# MCMC --------------------------------------------------------------------

ddm <- ratcliff_dm(dx = .005, dt = .005)

obs_data(ddm) <- ratcliff_synth_data

chains = estimate_dm(drift_dm_obj = ddm, framework = "bayesian",
                     burn_in = 200, samples = 200)

use_directory("inst")
saveRDS(object = chains, file = file.path("inst", "example_mcmc.rds"))
