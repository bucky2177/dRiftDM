# Conditional Log-Posterior Distributions for MCMC Sampling

These functions compute conditional log-posterior distributions used in
a (hierarchical) MCMC sampler.

## Usage

``` r
log_posterior_hyper(
  phi_j_mat,
  theta_j_mat,
  log_prior_hyper_fun,
  log_prior_lower_fun,
  temperatures,
  suppress_warnings = TRUE
)

log_posterior_lower(
  thetas_one_subj_mat,
  all_phis_mat,
  model_subj,
  log_prior_lower_funs,
  temperatures,
  suppress_warnings = TRUE
)
```

## Arguments

- phi_j_mat:

  a numeric matrix of current group-level parameters for one
  individual-level parameter. It must be 2 x n_chains and provide the
  mean and standard deviation; in that order.

- theta_j_mat:

  a numeric matrix of individual-level parameter values across all
  individuals and chains for one model parameter. Must be n_chains x
  n_subj.

- log_prior_hyper_fun:

  a function that returns the log-prior density of the hyperparameters.
  Must be a single function (not a list of functions) and it must accept
  `phi_j_mat` as input.

- log_prior_lower_fun:

  a function that returns the log-prior density of individual parameter
  values given the mean and standard deviation at the group-level (as
  stored in `phi_j_mat`). Must be a single function (not a list of
  functions) and it must accept `theta_j_mat` as a first argument and
  input, and the mean and standard deviation with arguments `mean` and
  `sd` (vectorized).

- temperatures:

  a numeric vector of temperature scaling values, one per chain, used
  when applying tempered inference (e.g., in TIDE).

- suppress_warnings:

  logical, if TRUE, warnings created from `log_prior_hyper_fun` and
  `log_prior_lower_fun(s)` are suppressed. The default is true, because
  in the beginning of an MCMC sampler implausible proposals are provided
  which can yield missing values and warnings.

- thetas_one_subj_mat:

  a named matrix of lower-level parameters for a single participant.
  Each row represents one parameter, and each column one chain.

- all_phis_mat:

  a named matrix of all current group-level parameters. Each mean
  group-level parameter must be named `"M-<param>"` and each standard
  deviation `"S-<param>"`. The `<param>` part must match the
  individual-level parameters in `thetas_one_subj_mat`. If this argument
  is `NULL`, this indicates that the estimation is done in a
  non-hierarchical fashion. Each row represents a hyper-parameter, and
  each column one chain.

- model_subj:

  a `drift_dm` object, containing an individual's data.

- log_prior_lower_funs:

  a named list of functions, one per parameter stored in
  `thetas_one_subj_mat`, returning the log-prior densities. It is
  assumed that each function can take one type of parameter across
  chains (i.e., a vector). In the non-hierarchical case, each function
  can only accept a single vector (for the respective individual-level
  parameter across chains). In the hierarchical case, each function must
  also support the arguments `mean` and `sd` for vectorized prior
  computation.

## Value

A list with two elements:

- `posterior_vals`, the total log-posterior values (log-likelihood +
  log-prior) per chain.

- `log_like_vals`, the log-likelihood components only, per chain.

## Details

`log_posterior_hyper()` computes the conditional log-posterior for a
group-level hyperparameter matrix `phi_j_mat`, given the
individual-level parameters across subjects `theta_j_mat` (for one type
of model parameter).

`log_posterior_lower()` computes the conditional log-posterior for an
individual participant’s parameter matrix `thetas_one_subj_mat`, given
prior distributions. In the hierarchical setting, the prior
distributions are conditioned on the group-level parameters.
