# Aggregate Data Frame Columns by Group

internal function to aggregate columns of a data frame across "ID" while
considering a set of grouping columns. It retains the class and
attriubtes of the input data.

## Usage

``` r
internal_aggregate(data, group_cols)
```

## Arguments

- data:

  A [data.frame](https://rdrr.io/r/base/data.frame.html) containing the
  data to be aggregated. It should include both the grouping columns, an
  "ID" column, and the columns for which aggregation shall take place.

- group_cols:

  A character vector specifying the names of the columns to group by
  during aggregation.

## Value

A `data.frame` containing the aggregated data.

## Details

`internal_aggregate` identifies DV columns as those not in `group_cols`
or `"ID"`. It then calculates the mean of these DV columns, grouped by
the specified columns. Columns specified in `group_cols` that are not
part of `data` are ignored silently.

## See also

[`aggregate_stats()`](https://bucky2177.github.io/dRiftDM/reference/aggregate_stats.md),
[`calc_stats()`](https://bucky2177.github.io/dRiftDM/reference/calc_stats.md),
[`new_stats_dm()`](https://bucky2177.github.io/dRiftDM/reference/new_stats_dm.md)
