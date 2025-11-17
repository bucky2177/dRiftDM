# Check if an object is a valid numeric vector

This function verifies whether the input is a numeric vector with no
missing (`NA`, `NaN`) or infinite (`Inf` or `-Inf`) values.

## Usage

``` r
is_numeric(x)
```

## Arguments

- x:

  An object to check.

## Value

A logical value: `TRUE` if the input is a numeric vector without any
missing or infinite values, otherwise `FALSE`.
