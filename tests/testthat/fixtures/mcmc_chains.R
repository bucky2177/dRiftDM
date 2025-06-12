# create some mcmc samples (and just save the sampled thetas) for fast testing

my_model = ratcliff_dm(
  dt = .0125,
  dx = .0125,
  t_max = 1.5,
  obs_data = ratcliff_synth_data
)

chains = estimate_model_bayesian(
  drift_dm_obj = my_model,
  n_chains = 40,
  burn_in = 10,
  samples = 50,
  seed = 1
)


summary(chains)
