# Numerical integration using the trapezoidal rule

These are internal helper functions to perform numerical integration via
the trapezoidal rule. The workhorse is `internal_trapz()`, which
computes the full integral or returns the cumulative integral.

## Usage

``` r
internal_trapz(x, y, return_cumsum = FALSE)

cumtrapz(x, y)

trapz(x, y)
```

## Arguments

- x:

  numeric vector of strictly increasing x-values.

- y:

  numeric vector of function values at `x`.

- return_cumsum:

  logical, if `TRUE` return the cumulative integral at each point in `x`
  (starting with 0), if `FALSE` return the total integral.

## Value

- `trapz()`: a single numeric value

- `cumtrapz()`: numeric vector of cumulative integrals (starting with 0)

- `internal_trapz()`: either of the above, depending on `return_cumsum`

## Details

- `internal_trapz(x, y, return_cumsum = FALSE)`: core implementation

- `trapz(x, y)` wrapper for `internal_trapz(x, y, FALSE)`, returns the
  total integral.

- `cumtrapz(x, y)` wrapper for `internal_trapz(x, y, TRUE)`, returns the
  cumulative integral.
