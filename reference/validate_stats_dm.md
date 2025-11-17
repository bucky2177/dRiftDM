# Validate a stats_dm Object

`validate_stats_dm` is an internal (i.e., not exported) generic function
to ensure that `stats_dm` objects, as well as their specific subclasses
(`basic_stats`, `cafs`, `quantiles`, `delta_funs`, `sum_dist`, and
`fit_stats`), meet the necessary structural and column requirements.
Each method performs class-specific validation checks.

## Usage

``` r
validate_stats_dm(stat_df)

# S3 method for class 'basic_stats'
validate_stats_dm(stat_df)

# S3 method for class 'cafs'
validate_stats_dm(stat_df)

# S3 method for class 'quantiles'
validate_stats_dm(stat_df)

# S3 method for class 'delta_funs'
validate_stats_dm(stat_df)

# S3 method for class 'densities'
validate_stats_dm(stat_df)

# S3 method for class 'sum_dist'
validate_stats_dm(stat_df)

# S3 method for class 'fit_stats'
validate_stats_dm(stat_df)

# S3 method for class 'stats_dm'
validate_stats_dm(stat_df)
```

## Arguments

- stat_df:

  A `data.frame` of class `stats_dm`, `basic_stats`, `cafs`,
  `quantiles`, `delta_funs`, `sum_dist`, or `fit_stats` containing the
  calculated statistics to be validated.

## Value

Returns the unmodified `stat_df` for convenience.

## Details

The validation process checks for required columns and structure based
on the class of `stat_df`. Each class has specific requirements:

- **`validate_stats_dm.stats_dm`:** Ensures `stat_df` is a `data.frame`.

- **`validate_stats_dm.basic_stats`:** Checks for the presence of
  `"Cond"`, exactly two columns with prefix `"Mean_`, and exactly one
  column prefixed with `"P_"`

- **`validate_stats_dm.cafs`:** Checks for the presence of `"Bin"`,
  `"Cond"`, and exactly one column prefixed with `"P_"`

- **`validate_stats_dm.quantiles`:** Requires `"Prob"`, `"Cond"`, and
  exactly two columns prefixed with `"Quant_"`

- **`validate_stats_dm.delta_funs`:** Ensures `"Prob"` exists, at least
  two columns prefixed with `"Quant_"`, and at least one column each
  `Avg_` and `Delta_`

- **`validate_stats_dm.delta_funs`:** Ensures `"Cond"`, `"Time"`, and
  `"Stat"` exists, and at least two column with `"Dens_"`.

- **`validate_stats_dm.sum_dist`:** Checks for a `"Source"` column.
  Here, it is also checked whether cell combinations appear equally
  often.

- **`validate_stats_dm.fit_stats`:** Checks for if the fit statistics
  summarize a log-likelihood cost function or the RMSE statistic. In the
  former case, the columns `"Log_Like"`, `"Neg_Log_Like"`, `"AIC"`, and
  `"BIC"` are expected. In the latter case, the columns `"RMSE_ms"` and
  `"RMSE_s"` are expected.
