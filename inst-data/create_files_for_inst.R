# FITS_SEP ----------------------------------------------------------------

ddm = dmc_dm(dx = .01, dt = .01)
ulrich_flanker_data = ulrich_flanker_data
ulrich_flanker_data = ulrich_flanker_data[
  ulrich_flanker_data$ID %in% c(1, 2, 3),
]
l_u = get_lower_upper(ddm)
fits_ids = estimate_dm(
  drift_dm_obj = ddm,
  obs_data = ulrich_flanker_data,
  optimizer = "nmkb",
  lower = l_u$lower,
  upper = l_u$upper,
  verbose = 0,
  progress = 1,
  n_cores = 3
)

use_directory("inst")
saveRDS(object = fits_ids, file = file.path("inst", "example_fits_ids.rds"))

# FITS_AGG ----------------------------------------------------------------

ddm = ratcliff_dm()
lower = c(2, 0.4, 0.2)
upper = c(6, 0.7, 0.4)
obs_data = simulate_data(ddm, n = 100, k = 3, lower = lower, upper = upper)

prms_solve(ddm)[c("dx", "dt")] = .005
fits_agg = estimate_dm(
  drift_dm_obj = ddm,
  obs_data = obs_data$synth_data,
  optimizer = "nmkb",
  approach = "agg_c",
  lower = lower,
  upper = upper,
  verbose = 0
)


use_directory("inst")
saveRDS(object = fits_agg, file = file.path("inst", "example_fits_agg.rds"))

# MCMC --------------------------------------------------------------------

ddm <- ratcliff_dm(dx = .01, dt = .01)

obs_data(ddm) <- ratcliff_synth_data

chains = estimate_dm(
  drift_dm_obj = ddm,
  approach = "sep_b",
  burn_in = 200,
  samples = 200,
  n_chains = 20,
  seed = 1
)

use_directory("inst")
saveRDS(object = chains, file = file.path("inst", "example_mcmc.rds"))

# MCMC HIER ----------------------------------------------------------------

ddm <- ratcliff_dm(dx = .01, dt = .01)

data <- simulate_data(
  ddm,
  k = 6,
  n = 100,
  lower = c(2, 0.4, 0.2),
  upper = c(5, 0.8, 0.4)
)

chains = estimate_dm(
  drift_dm_obj = ddm,
  obs_data = data$synth_data,
  approach = "hier_b",
  burn_in = 200,
  samples = 200,
  n_chains = 20,
  seed = 1,
  n_cores = 3
)

use_directory("inst")
saveRDS(object = chains, file = file.path("inst", "example_mcmc_hier.rds"))
