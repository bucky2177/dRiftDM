# Simulate one data set

Function that simulates a single data based on a model (using the prms
set

## Usage

``` r
simulate_one_data_set(drift_dm_obj, n, conds = NULL, round_to = NULL)
```

## Arguments

- drift_dm_obj:

  a
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
  object

- n:

  integer, specifying the number of trials per condition. Can be a
  single integer, or a (named) integer vector with the same length as
  conds

- conds:

  character vector, specifying the conditions to sample from. Default
  `NULL` is equivalent to conds(drift_dm_obj)

- round_to:

  integer, specifying the number of decimal places that the simulated
  RTs should have. Default is `3L`.

## Value

A data.frame with the columns "RT", "\<b_column\>", and "Cond"; and with
n rows.
