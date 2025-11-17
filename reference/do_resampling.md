# Temporarily suppress new stats generation during resampling

Internal helper that sets the `skip_new_stats_dm` and
`skip_validate_stats_dm` options to `TRUE` before evaluating an
expression, and resets them to `NULL` afterward (see also
[`stats.options()`](https://bucky2177.github.io/dRiftDM/reference/stats.options.md)).
Intended to prevent the (unncessary) creation/checking of stats_dm
objects during resampling.

## Usage

``` r
do_resampling(x)
```

## Arguments

- x:

  An expression to evaluate.

## Value

The result of evaluated `x`.
