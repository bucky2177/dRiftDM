# Convert Character Digits to Numeric Digits

This internal function casts a character vector to integer, if the
character vector only contains digits for entries. If the input vector
is not of type character or if any entry contains a non-digit, then the
vector is returned unmodified.

## Usage

``` r
try_cast_integer(values)
```

## Arguments

- values:

  a vector of values to attempt conversion to integer.

## Value

an integer vector if conversion succeeds; otherwise, the original
vector.
