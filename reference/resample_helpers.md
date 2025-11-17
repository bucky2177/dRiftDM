# Internal Helpers for Resampling of Summary Statistics

These functions support the construction of intervals for descriptive
statistics computed from observed or simulated data. They are used
internally by
[`stats_resample_dm()`](https://bucky2177.github.io/dRiftDM/reference/stats_resample_dm.md)
methods.

## Usage

``` r
stats_resample_wrapper(
  obs_data_split,
  one_set_idxs = NULL,
  type,
  b_coding,
  ...
)

resample_assemble(resample_list, level, original)
```

## Arguments

- obs_data_split:

  a named list of [data.frame](https://rdrr.io/r/base/data.frame.html)s,
  containing a single set of observed data, splitted by condition

- one_set_idxs:

  a named list of numeric vectors. Each entry contains indices to
  shuffle the [data.frame](https://rdrr.io/r/base/data.frame.html)s in
  `obs_data_split`. Default `NULL` keeps `obs_data_split` as is.

- type:

  a character, passed to
  [`calc_stats_pred_obs()`](https://bucky2177.github.io/dRiftDM/reference/calc_stats_pred_obs.md).

- b_coding:

  a list with boundary coding information, , required to wrangle rts to
  match with
  [`calc_stats_pred_obs()`](https://bucky2177.github.io/dRiftDM/reference/calc_stats_pred_obs.md).

- ...:

  additional arguments passed to
  [`calc_stats_pred_obs()`](https://bucky2177.github.io/dRiftDM/reference/calc_stats_pred_obs.md).

- resample_list:

  a list of statistics returned by calls to `stats_resample_wrapper()`.

- level:

  a numeric between 0 and 1, controlling the width of the interval.

- original:

  a `stats_dm` object representing the statistic computed from the
  original data set or model prediction.

## Value

- `stats_resample_wrapper()` returns a single `stats_dm` object for one
  sample.

- `resample_assemble()` returns a `stats_dm` object containing the lower
  and upper interval bounds along with the original estimate.

## Details

`stats_resample_wrapper()` wraps a call to
[`calc_stats_pred_obs()`](https://bucky2177.github.io/dRiftDM/reference/calc_stats_pred_obs.md)
for use in resampling.

`resample_assemble()` takes a list of resampled statistics and the
original statistic, and computes lower and upper bounds based on the
requested level. It returns a `stats_dm` object with an added `Estimate`
column.
