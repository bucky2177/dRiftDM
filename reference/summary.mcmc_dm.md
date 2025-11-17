# Summary for `mcmc_dm` Objects

Summary and corresponding print methods for objects of the class
`mcmc_dm`, resulting from a call to
[`estimate_bayesian()`](https://bucky2177.github.io/dRiftDM/reference/estimate_bayesian.md).
`mcmc_dm` objects contain MCMC samples for Bayesian parameter estimation
of
[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
objects. The summary includes basic parameter statistics, quantiles,
Gelman-Rubin diagnostics, and effective sample sizes.

## Usage

``` r
# S3 method for class 'mcmc_dm'
summary(object, ..., id = NULL)

# S3 method for class 'summary.mcmc_dm'
print(
  x,
  ...,
  round_digits = drift_dm_default_rounding(),
  show_statistics = TRUE,
  show_quantiles = FALSE,
  show_gr = TRUE,
  show_eff_n = TRUE
)
```

## Arguments

- object:

  an object of class `mcmc_dm`, as returned by
  [`estimate_bayesian()`](https://bucky2177.github.io/dRiftDM/reference/estimate_bayesian.md)

- ...:

  additional arguments passed forward to
  [`coda::summary.mcmc.list()`](https://rdrr.io/pkg/coda/man/summary.mcmc.html).

- id:

  optional single numeric or character, specifying one or more
  participant IDs to subset `object` in the hierarchical case. Note that
  `id` will be converted to character, because dimension names of the
  chains stored in `object` are character. If `NULL`, then the function
  is applied to group-level parameters.

- x:

  an object of class `summary.mcmc_dm`, as returned by
  `summary.mcmc_dm()`.

- round_digits:

  an integer, defining the number of digits for rounding the output.

- show_statistics:

  a logical, if `TRUE`, print basic parameter statistics (means, SDs,
  standard errors).

- show_quantiles:

  a logical, if `TRUE`, print quantile summary.

- show_gr:

  a logical; if `TRUE`, print Gelman-Rubin convergence diagnostics for
  each parameter.

- show_eff_n:

  a logical, if `TRUE`, print effective sample sizes for each parameter.

## Value

`summary.mcmc_dm()` returns an object of class `summary.mcmc_dm`, which
is a list with the following entries:

- `general`: General information about the MCMC run.

- `statistics`: Basic parameter summary statistics.

- `quantiles`: Quantiles for each parameter.

- `gr`: Gelman-Rubin diagnostics.

- `eff_n`: Effective sample sizes.

`print.summary.mcmc_dm()` prints selected summary components and returns
the input object invisibly.

## Details

The summary and diagnostic statistics of the MCMC chains are obtained
using the R package `coda`.

## See also

[`coda::gelman.diag()`](https://rdrr.io/pkg/coda/man/gelman.diag.html),
[`coda::effectiveSize()`](https://rdrr.io/pkg/coda/man/effectiveSize.html),
[`coda::summary.mcmc.list()`](https://rdrr.io/pkg/coda/man/summary.mcmc.html)

## Examples

``` r
mcmc_obj <- get_example_fits("mcmc_dm")
print(mcmc_obj)
#> Sampler: DE-MCMC 
#> Hierarchical: FALSE 
#> No. Parameters: 3 
#> No. Chains: 20 
#> Iterations Per Chain: 200 
summary(mcmc_obj)
#> Sampler: DE-MCMC 
#> Hierarchical: FALSE 
#> No. Parameters: 3 
#> No. Chains: 20 
#> Iterations Per Chain: 200 
#> 
#> -------
#> Parameter Summary: Basic Statistics
#>          Mean    SD Naive SE Time-series SE
#> muc     3.082 0.187    0.003          0.010
#> b       0.411 0.013    0.000          0.001
#> non_dec 0.300 0.003    0.000          0.000
#> 
#> Gelman-Rubin Statistics
#>     muc       b non_dec 
#>   1.039   1.046   1.048 
#> 
#> Effective Sample Size
#>     muc       b non_dec 
#> 363.421 353.772 428.942 
```
