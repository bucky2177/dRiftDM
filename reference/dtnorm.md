# Truncated Normal Density Function

Computes the probability density function for the truncated normal
distribution. This version supports both vector and matrix input for
`x`.

## Usage

``` r
dtnorm(x, mean = 0, sd = 1, lower = -Inf, upper = Inf, log = FALSE)

rtnorm(n, mean = 0, sd = 1, lower = -Inf, upper = Inf)
```

## Arguments

- x:

  A numeric vector or matrix of values where the density should be
  evaluated.

- mean:

  Mean of the normal distribution. Can be a scalar or vector (recycled
  if necessary).

- sd:

  Standard deviation of the normal distribution. Can be a scalar or
  vector (recycled if necessary).

- lower:

  Lower truncation bound. Can be a scalar or vector (recycled if
  necessary). Default is `-Inf`.

- upper:

  Upper truncation bound. Can be a scalar or vector (recycled if
  necessary). Default is `Inf`.

- log:

  Logical; if `TRUE`, probabilities `p` are given as `log(p)`. Default
  is `FALSE`.

## Value

A numeric vector or matrix of the same shape as `x`, containing the
(possibly log) densities.

## Details

The function evaluates the normal density at `x` and scales it to
reflect truncation to the interval (`lower`, `upper`). Values outside
the truncation bounds are assigned a density of 0 (or `-Inf` on the log
scale). Internally, [stats::dnorm](https://rdrr.io/r/stats/Normal.html)
and [stats::pnorm](https://rdrr.io/r/stats/Normal.html) are used.

If `x` is a matrix, the result retains the same dimensions. All other
arguments are recycled as needed. For example, if x has two rows and 5
columns, then mean might provide 2 values, so that the first/second row
is evaluated under the first/second mean value.
