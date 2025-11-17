# Print Functions for Stats Objects

when calling
[`calc_stats()`](https://bucky2177.github.io/dRiftDM/reference/calc_stats.md),
each returned statistic will be a subclass of `stats_dm` and
[data.frame](https://rdrr.io/r/base/data.frame.html). The following
[`print()`](https://rdrr.io/r/base/print.html)` `methods will call the
more generic
[`print.stats_dm()`](https://bucky2177.github.io/dRiftDM/reference/calc_stats.md)
function.

## Usage

``` r
# S3 method for class 'cafs'
print(x, ...)

# S3 method for class 'basic_stats'
print(x, ...)

# S3 method for class 'quantiles'
print(x, ...)

# S3 method for class 'delta_funs'
print(x, ...)

# S3 method for class 'fit_stats'
print(x, ...)

# S3 method for class 'sum_dist'
print(x, ...)
```

## Arguments

- x:

  a subclass of [data.frame](https://rdrr.io/r/base/data.frame.html), as
  returned by
  [`calc_stats()`](https://bucky2177.github.io/dRiftDM/reference/calc_stats.md).

- ...:

  additional arguments passed forward to
  [`print.stats_dm()`](https://bucky2177.github.io/dRiftDM/reference/calc_stats.md).

## Value

`x` (invisibly)
