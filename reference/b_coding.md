# The Coding of the Boundaries

Functions to get or set the "boundary coding" of an object.

## Usage

``` r
b_coding(object, ...) <- value

# S3 method for class 'drift_dm'
b_coding(object, ...) <- value

b_coding(object, ...)

# S3 method for class 'drift_dm'
b_coding(object, ...)

# S3 method for class 'fits_ids_dm'
b_coding(object, ...)

# S3 method for class 'fits_agg_dm'
b_coding(object, ...)
```

## Arguments

- object:

  an object of type
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md),
  `fits_ids_dm`, or `fits_agg_dm` (see
  [`estimate_dm()`](https://bucky2177.github.io/dRiftDM/reference/estimate_dm.md)).

- ...:

  additional arguments.

- value:

  a named list, specifying how boundaries are coded (see Details).

## Value

For `b_coding()` a list containing the boundary coding For
`b_coding<-()` the updated `drift_dm` or `fits_ids_dm` object

## Details

`b_coding()` is a generic accessor function, and `b_coding<-()` a
generic replacement function. The default methods get and set the
"boundary coding", which is an attribute of
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
model.

The boundary coding summarizes which response time belongs to which
boundary and how the boundaries shall be "labeled". The list specifies
three entries:

- `column`, contains a single character string, indicating which column
  in an observed data set codes the boundaries.

- `u_name_value`, contains a numeric or character vector of length 1.
  The name of this vector gives a label for the upper boundary, and the
  entry gives the value stored in `obs_data[[column]]` coding the upper
  boundary.

- `l_name_value`, contains a numeric or character vector of length 1.
  The name of this vector gives a label for the lower boundary, and the
  entry gives the value stored in `obs_data[[column]]` coding the lower
  boundary.

The package `dRiftDM` has a default boundary coding:

- `column` = "Error"

- `u_name_value` = c("corr" = 0)

- `l_name_value` = c("err" = 1)

Thus, per default, dRiftDM assumes that any observed data set has a
column "Error", providing the values 0 and 1 for the upper and lower
boundary, respectively. The upper and lower boundaries are labeled
"corr" and "err", respectively. These labels are used, for example, when
calculating statistics (see
[calc_stats](https://bucky2177.github.io/dRiftDM/reference/calc_stats.md)).

When calling `b_coding<-()` with `value = NULL`, the default "accuracy"
coding is evoked

## See also

[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)

## Examples

``` r
# show the default accuracy coding of dRiftDM
my_model <- ratcliff_dm() # get a pre-built model
b_coding(my_model)
#> $column
#> [1] "Error"
#> 
#> $u_name_value
#> corr 
#>    0 
#> 
#> $l_name_value
#> err 
#>   1 
#> 

# can be modified/replaced
b_coding(my_model)[["column"]] <- "Response"

# accessor method also available for fits_ids_dm objects
# get an exemplary fits_ids_dm object (see estimate_model_ids)
fits <- get_example_fits("fits_ids_dm")
names(b_coding(fits))
#> [1] "column"       "u_name_value" "l_name_value"
```
