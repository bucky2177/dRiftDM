# Extract Log-Likelihood for a drift_dm Object

This method extracts the log-likelihood for a `drift_dm` object if
possible.

## Usage

``` r
# S3 method for class 'drift_dm'
logLik(object, ...)
```

## Arguments

- object:

  a
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
  object containing observed data

- ...:

  additional arguments

## Value

A `logLik` object containing the log-likelihood value for the
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
object. This value has attributes for the number of observations
(`nobs`) and the number of model parameters (`df`).

Returns `NULL` if the log-likelihood is not available (e.g., when the
model has no observed data attached).

## Examples

``` r
# get a pre-built model and a data set for demonstration purpose
# (when creating the model, set the discretization to reasonable values)
a_model <- dmc_dm()
obs_data(a_model) <- dmc_synth_data
logLik(a_model)
#> 'log Lik.' 124.6413 (df=7)
```
