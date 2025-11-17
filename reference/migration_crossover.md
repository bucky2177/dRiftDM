# Perform a Migration Step Between Chains

In a migration step, a random subset of chains is selected. Each
selected chain `k` receives a proposal based on the next chain `(k + 1)`
in the sequence (cycling back to the first chain after the last). The
proposed parameters are slightly perturbed by uniform noise controlled
by `b`. All proposals are then evaluated simultaneously using Metropolis
acceptance probabilities via
[`call_log_posterior_m()`](https://bucky2177.github.io/dRiftDM/reference/call_log_posterior_m.md),
and accepted proposals replace the current values.

## Usage

``` r
migration_crossover(
  prms_across_chains,
  pis_across_chains,
  log_likes_across_chains,
  b = 0.001,
  ...
)
```

## Arguments

- prms_across_chains:

  a numeric matrix of dimension `p × n`, where `p` is the number of
  parameters and `n` is the number of chains. Each column contains the
  current parameter vector of a chain.

- pis_across_chains:

  a numeric vector of length `n`, containing the current log-posterior
  values for each chain.

- log_likes_across_chains:

  a numeric vector of length `n`, containing the current log-likelihood
  values for each chain.

- b:

  a small numeric value used to perturb the proposal parameters to avoid
  degeneracy.

- ...:

  additional arguments passed to
  [`call_log_posterior_m()`](https://bucky2177.github.io/dRiftDM/reference/call_log_posterior_m.md).

## Value

A list with the following components:

- `new_prms_across_chains`: The updated parameter matrix of shape
  `p × n`.

- `new_pis_across_chains`: The updated vector of log-posterior values.

- `new_log_likes_across_chains`: The updated vector of log-likelihood
  values.

## See also

[full_crossover](https://bucky2177.github.io/dRiftDM/reference/full_crossover.md),
[`call_log_posterior_m()`](https://bucky2177.github.io/dRiftDM/reference/call_log_posterior_m.md)
