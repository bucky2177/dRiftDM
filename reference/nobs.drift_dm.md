# Get the Number of Observations for a drift_dm Object

This method retrieves the total number of observations in the `obs_data`
list of a `drift_dm` object.

## Usage

``` r
# S3 method for class 'drift_dm'
nobs(object, ...)
```

## Arguments

- object:

  a
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
  object, which potentially contains the observed data in
  `object$obs_data`.

- ...:

  additional arguments

## Value

An integer representing the total number of observations across all
conditions in `object$obs_data`. If `obs_data` doesn't exist, the
function returns 0

## Details

The function iterates over each element in `object$obs_data`, counts the
entries in each nested component, and returns the cumulative sum as the
total observation count.

It was written to provide an `nobs` method for calculating the
log-likelihood ([logLik](https://rdrr.io/r/stats/logLik.html)), AIC
([stats::AIC](https://rdrr.io/r/stats/AIC.html)), and BIC
([stats::BIC](https://rdrr.io/r/stats/AIC.html)) statistics for objects
of type
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md).

## Examples

``` r
# get a pre-built model and data set for demonstration purpose
a_model <- dmc_dm()
obs_data(a_model) <- dmc_synth_data

# then get the number of observations by accessing the model
nobs(a_model)
#> [1] 600

# same number of observations as in the original data set
nrow(dmc_synth_data)
#> [1] 600
```
