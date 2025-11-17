# Calculate Fit Statistics

Computes/Summarizes multiple fit statistics, inclduing Log-Likelihood,
the Negative Log-Likelihood, the Akaike Information Criterion (AIC), the
Bayesian Information Criterion (BIC), and the Root-Mean Squared-Error
(RMSE) statistic.

## Usage

``` r
calc_fit_stats(drift_dm_obj, k = 2, ...)
```

## Arguments

- drift_dm_obj:

  an object of type
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)

- k:

  a single numeric, scaling the penality of
  [stats::AIC](https://rdrr.io/r/stats/AIC.html))

- ...:

  additional arguments passed forward. Options are `probs`, `n_bins`,
  and `weight_err` for calculating the RMSE.

## Value

A custom object of class `stats_dm` (c("fit_stats", "stats_dm",
"data.frame")). The columns are:

- `Log_Like`: the log-likelihood value

- `Neg_Log_Like`: the negative log-likelihood value

- `AIC`: the calculated AIC value

- `BIC`: the calculated BIC value

- `RMSE_s`: the root-mean-squared error (for RTs in seconds)

- `RMSE_ms`: the root-mean-squared error (for RTs in milliseconds) If a
  respective statistic cannot be calculated, the respective column
  contains `NA`.

## See also

[`new_stats_dm()`](https://bucky2177.github.io/dRiftDM/reference/new_stats_dm.md),
[logLik.drift_dm](https://bucky2177.github.io/dRiftDM/reference/logLik.drift_dm.md)
