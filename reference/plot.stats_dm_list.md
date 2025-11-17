# Plot Multiple Statistics

This function iterates over a list of statistics data, resulting from a
call to
[`calc_stats()`](https://bucky2177.github.io/dRiftDM/reference/calc_stats.md),
and subsequently plots each statistic. It allows for a simple
arrangement of multiple plots on a single graphics device.

## Usage

``` r
# S3 method for class 'stats_dm_list'
plot(x, ..., mfrow = NULL)
```

## Arguments

- x:

  an object of type `stats_dm_list`, which is essentially a list of
  multiple statistics, resulting from a call to
  [`calc_stats()`](https://bucky2177.github.io/dRiftDM/reference/calc_stats.md).

- ...:

  additional arguments passed to the
  [plot](https://rdrr.io/r/graphics/plot.default.html) function for each
  individual `stats_dm` object in `x`.

- mfrow:

  an optional numeric vector of length 2, specifying the number of rows
  and columns for arranging multiple panels in a single plot (e.g.,
  `c(1, 3)`). Plots are provided sequentially if `NULL` (default), using
  the current graphics layout of a user.

## Value

Nothing (`NULL`; invisibly)

## Details

The `plot.stats_dm_list()` function "merely" iterates over each entry of
`x` and calls the respective
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) method. If
`dRiftDM` doesn't provide a
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for an
object stored in `x`, the respective entry is skipped and a message is
displayed.

When users want more control over each plot, it is best to call the
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) function
separately for each statistic in the list (e.g., `plot(x$cafs)`;
`plot(x$quantiles)`)

## See also

[`plot.cafs()`](https://bucky2177.github.io/dRiftDM/reference/plot.cafs.md),
[`plot.quantiles()`](https://bucky2177.github.io/dRiftDM/reference/plot.quantiles.md),
[`plot.delta_funs()`](https://bucky2177.github.io/dRiftDM/reference/plot.delta_funs.md),
[`plot.densities()`](https://bucky2177.github.io/dRiftDM/reference/plot.densities.md)

## Examples

``` r
# get a list of statistics for demonstration purpose
all_fits <- get_example_fits("fits_ids_dm")
stats <- calc_stats(all_fits, type = c("cafs", "quantiles"))

# then call the plot function.
plot(stats, mfrow = c(1, 2))
#> Aggregating across ID
#> Aggregating across ID

```
