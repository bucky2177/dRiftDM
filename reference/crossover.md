# Perform Crossover Between Chains

This function dispatches to either
[`full_crossover()`](https://bucky2177.github.io/dRiftDM/reference/full_crossover.md)
or
[`migration_crossover()`](https://bucky2177.github.io/dRiftDM/reference/migration_crossover.md)
depending on the `which` argument.

## Usage

``` r
crossover(which, ...)
```

## Arguments

- which:

  character string, Either `"diff"` or `"migration"`.

- ...:

  Further arguments passed to the underlying crossover function.

## Value

A list with the following components:

- `new_prms_across_chains`: The updated parameter matrix of shape
  `p × n`.

- `new_pis_across_chains`: The updated vector of log-posterior values.

- `new_log_likes_across_chains`: The updated vector of log-likelihood
  values.
