# Resample Observed and Predicted Statistics for Interval Estimation

Internal methods to generate bootstrap-like intervals for descriptive
statistics derived from either observed data or model predictions. These
methods support both `drift_dm` objects and data.frames containing a
single participant's observed data.

## Usage

``` r
stats_resample_dm(object, conds, type, b_coding, ..., R, interval_level)

# S3 method for class 'drift_dm'
stats_resample_dm(
  object,
  conds,
  type,
  b_coding,
  ...,
  R = 100,
  interval_level = 0.95,
  n_sim = NULL
)

# S3 method for class 'data.frame'
stats_resample_dm(
  object,
  conds,
  type,
  b_coding,
  ...,
  R = 100,
  interval_level = 0.95,
  progress = 0,
  level
)

# S3 method for class 'fits_ids_dm'
stats_resample_dm(
  object,
  conds,
  type,
  b_coding,
  ...,
  R = 100,
  interval_level = 0.95,
  progress = 0,
  level
)
```

## Arguments

- object:

  a `drift_dm` object (for model-based resampling) or a
  [data.frame](https://rdrr.io/r/base/data.frame.html) with observed
  data for a single participant. `drift_dm_stats_types("sum_dists")`,
  such as `"quantiles"` or `"cafs"`.

- conds:

  a character vector indicating the condition(s) for which the
  statistics should be resampled.

- type:

  a character string, specifying the `type` of statistic to calculate

- b_coding:

  a list, specifying the boundary coding, required when calculating the
  statistics.

- ...:

  additional arguments passed to
  [`stats_resample_wrapper()`](https://bucky2177.github.io/dRiftDM/reference/resample_helpers.md)
  and
  [`simulate_data()`](https://bucky2177.github.io/dRiftDM/reference/simulate_data.md).
  Must contain `type` and `b_coding`

- R:

  an integer, number of replications (default is 100).

- interval_level:

  a numeric between 0 and 1, controlling the width of the interval
  (default is 0.95).

- n_sim:

  an optional vector, providing the trial numbers for simulating
  synthetic data under the model. Only relevant when no observed data is
  attached to the model via the `obs_data` entry of the model.

- progress:

  an integer, specifying if a progress bar shall be displayed (`1`) or
  not (`0`).

- level:

  a character string, specifying at which level resampling shall take
  place. `"individual"` will lead to resampling of an individual's data.
  `"group"` will lead to resampling of the entire participant.

## Value

A `stats_dm` object with added column `Estimate` indicating whether the
row represents the lower interval bound, the original value (`"orig"`),
or the upper interval bound. The interval level can be controlled via
the `interval_level` argument.

## Details

The `stats_resample_dm()` generic dispatches to class-specific methods.
For `drift_dm` objects, it generates synthetic data sets under the
model. For raw data, it resamples observations with replacement (i.e, it
performs a bootstrap). In both cases, statistics from the
resampled/generated data are used to compute intervals for the requested
statistic.

Resampling is done for each condition separately.

The function `stats_resample_dm()` is called within
[`calc_stats()`](https://bucky2177.github.io/dRiftDM/reference/calc_stats.md).
