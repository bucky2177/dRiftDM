# Set aggregated data to a model object

Helper function that aggregates a data set across all participants and
attaches the resulting group-level summary statistics to a
`drift_dm_obj`. This is required when the user wants to fit aggregated
data (e.g., via the RMSE cost function).

## Usage

``` r
set_agg_data(drift_dm_obj, obs_data_ids, ...)
```

## Arguments

- drift_dm_obj:

  a model object (of class `drift_dm`) to which the aggregated data will
  be attached.

- obs_data_ids:

  a data.frame containing individual-level observations. Must include an
  `ID` column identifying participants.

- ...:

  optional arguments (currently supported are `n_bins` and `probs` which
  are relevant when using the `rmse` cost function)

## Value

The updated `drift_dm_obj` with aggregated `stats_agg` and `obs_data`
set to `NULL`.

## Details

This function is intended for internal use and is called by
[`estimate_dm()`](https://bucky2177.github.io/dRiftDM/reference/estimate_dm.md)
when aggregated model fitting is requested.
