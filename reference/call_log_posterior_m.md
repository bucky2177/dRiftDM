# Metropolis Acceptance Step for Vectorized MCMC Sampling

This internal function computes the Metropolis acceptance decision for a
set of MCMC proposals, using either hierarchical or non-hierarchical
posterior evaluation. It returns updated log-posterior and
log-likelihood values, as well as a logical vector indicating accepted
proposals.

## Usage

``` r
call_log_posterior_m(
  proposal_mat,
  prev_prms_mat,
  prev_pis,
  prev_lls,
  level,
  re_eval,
  ...
)
```

## Arguments

- proposal_mat:

  a numeric matrix of proposed parameter values. Each column corresponds
  to one chain; rows represent parameters.

- prev_prms_mat:

  a numeric matrix of current (previous) parameter values. Must have the
  same dimensions as `proposal_mat`.

- prev_pis:

  a numeric vector of current log-posterior values for each chain.

- prev_lls:

  a numeric vector of current log-likelihood values for each chain.

- level:

  a character string specifying the sampling level, either `"lower"`,
  `"hyper"`, or `"none"`. Determines whether to call
  [`log_posterior_lower()`](https://bucky2177.github.io/dRiftDM/reference/log_posterior_hyper.md)
  or
  [`log_posterior_hyper()`](https://bucky2177.github.io/dRiftDM/reference/log_posterior_hyper.md).

- re_eval:

  logical. If `TRUE`, the log-posterior and log-likelihood for the
  current parameters are re-evaluated.

- ...:

  Additional arguments passed to
  [`log_posterior_lower()`](https://bucky2177.github.io/dRiftDM/reference/log_posterior_hyper.md)
  or
  [`log_posterior_hyper()`](https://bucky2177.github.io/dRiftDM/reference/log_posterior_hyper.md),
  depending on the `level`.

## Value

A list with three elements:

- `pis`:

  A numeric vector of updated log-posterior values.

- `lls`:

  A numeric vector of updated log-likelihood values.

- `accept`:

  A logical vector of length equal to the number of chains, indicating
  which proposals were accepted.

## Details

This function implements a vectorized Metropolis acceptance step for
multiple MCMC chains simultaneously. The posterior is calculated using
either
[`log_posterior_lower()`](https://bucky2177.github.io/dRiftDM/reference/log_posterior_hyper.md)
for subject-level parameters or
[`log_posterior_hyper()`](https://bucky2177.github.io/dRiftDM/reference/log_posterior_hyper.md)
for group-level parameters.

Log-posterior and log-likelihood values are only updated where proposals
were accepted. In cases where proposals yield invalid posteriors (i.e.,
`NA`), they are automatically rejected.
