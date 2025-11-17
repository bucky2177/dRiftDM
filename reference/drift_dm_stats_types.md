# Available types of statistics

Internal helper to return supported statistic types depending on the
context (e.g., for observed data.frames or fitted model objects).

## Usage

``` r
drift_dm_stats_types(context = NULL)
```

## Arguments

- context:

  a character string, indicating the context. If `NULL`, all available
  types are returned.

## Value

a character vector of valid statistic types for the given context.
