# Update aggregated statistics in a `drift_dm` object

Internal function that creates or updates the aggregated statistics
(`stats_agg` and `stats_agg_info`) in a
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
object, depending on the specified cost function. For maximum likelihood
estimation (`"neg_log_like"`), aggregated statistics are removed,
because the raw RTs are used directly.

## Usage

``` r
update_stats_agg(
  drift_dm_obj,
  which_cost_function,
  probs = NULL,
  n_bins = NULL
)
```

## Arguments

- drift_dm_obj:

  a `drift_dm` object.

- which_cost_function:

  a character string, indicating which cost function is used. Must be
  one of
  [`drift_dm_cost_functions()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm_cost_functions.md).

- probs:

  optional numeric vector of probabilities for quantile calculation. If
  `NULL`, defaults are taken from
  [`drift_dm_default_probs()`](https://bucky2177.github.io/dRiftDM/reference/defaults.md).

- n_bins:

  an optional integer, giving the number of bins for the CAFs. If
  `NULL`, defaults are taken from
  [`drift_dm_default_n_bins()`](https://bucky2177.github.io/dRiftDM/reference/defaults.md).

## Value

the input `drift_dm_obj`, with its `stats_agg` and `stats_agg_info`
entries updated or removed, depending on the cost function and
availability of observed data

## Note

This function is called by
[`obs_data()`](https://bucky2177.github.io/dRiftDM/reference/obs_data.md)
and
[`cost_function()`](https://bucky2177.github.io/dRiftDM/reference/cost_function.md)

## See also

[`obs_data()`](https://bucky2177.github.io/dRiftDM/reference/obs_data.md),
[`cost_function()`](https://bucky2177.github.io/dRiftDM/reference/cost_function.md),
[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
