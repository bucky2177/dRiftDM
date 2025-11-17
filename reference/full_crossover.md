# Perform a Full Crossover Step Using Differential Evolution

This function updates each chain's parameters by proposing new values
using a differential evolution strategy. For each chain `k`, two other
chains `m` and `n` are randomly selected, and a proposal is generated
via: `prms_k + gamma * (prms_m - prms_n) + noise`, where
`gamma = 2.38 / sqrt(2 * n_prms)` and `noise` is uniform perturbation
controlled by `b`. The proposal is accepted with Metropolis probability
via
[`call_log_posterior_m()`](https://bucky2177.github.io/dRiftDM/reference/call_log_posterior_m.md),
and accepted proposals replace the current values.

## Usage

``` r
full_crossover(
  prms_across_chains,
  pis_across_chains,
  log_likes_across_chains,
  gamma = NULL,
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

- gamma:

  a single numeric tuning parameter, that scales the difference between
  parameters. If `NULL`, defaults to `2.38 / sqrt(2 * n_prms)`

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
