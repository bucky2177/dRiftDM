# Aggregate Statistics Across ID

`aggregate_stats` is a (not exported) function to aggregate `stats_dm`
objects across `ID`s. Since the column names may vary by the statistic
type, the behavior of aggregate depends on the subclass of `stats_dm`.

## Usage

``` r
aggregate_stats(stat_df)
```

## Arguments

- stat_df:

  A `data.frame` of class `stats_dm` (see
  [`new_stats_dm()`](https://bucky2177.github.io/dRiftDM/reference/new_stats_dm.md))

## Value

Returns the statistics aggregated across the relevant cols.

## Details

`aggregate_stats` calls the
[`internal_aggregate()`](https://bucky2177.github.io/dRiftDM/reference/internal_aggregate.md)
with the relevant arguments

## See also

[new_stats_dm](https://bucky2177.github.io/dRiftDM/reference/new_stats_dm.md),
[calc_stats](https://bucky2177.github.io/dRiftDM/reference/calc_stats.md),
[`internal_aggregate()`](https://bucky2177.github.io/dRiftDM/reference/internal_aggregate.md)
