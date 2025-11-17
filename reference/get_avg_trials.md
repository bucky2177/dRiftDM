# Compute average trials per condition across individuals

internal helper — assumes each `ID` appears with the same set of
conditions in column `Cond`. counts trials per `ID`×`Cond` and averages
across `ID`.

## Usage

``` r
get_avg_trials(obs_data_ids)
```

## Arguments

- obs_data_ids:

  data frame with columns `ID` and `Cond`; one row per trial.

## Value

list with:

- `N`: number of unique individuals

- `avg_trials`: named numeric vector of average trials per condition
