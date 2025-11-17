# Maybe droplevels of ID column

This function takes a data frame with an ID colmumn, and drops the
unused levels from the ID column if it is factor; in this case a warning
is thrown

## Usage

``` r
drop_levels_ID_column(some_data)
```

## Arguments

- some_data:

  a data.frame with an ID column

## Value

if the ID column is not of type factor, then the unmodified object is
returned.

if the ID column is of type factor,
[droplevels](https://rdrr.io/r/base/droplevels.html) is applied, and if
levels were dropped, a warning is thrown
