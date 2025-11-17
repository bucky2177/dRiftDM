# Summarizing Flex Parameters

summary method for class "flex_prms".

## Usage

``` r
# S3 method for class 'flex_prms'
summary(object, ...)

# S3 method for class 'summary.flex_prms'
print(
  x,
  ...,
  round_digits = drift_dm_default_rounding(),
  dependencies = TRUE,
  cust_parameters = TRUE
)
```

## Arguments

- object:

  an object of class `flex_prms`, resulting from a call to
  [flex_prms](https://bucky2177.github.io/dRiftDM/reference/flex_prms.md).

- ...:

  additional arguments passed forward to the respective method

- x:

  an object of class `summary.flex_prms`; a result of a call to
  `summary.flex_prms()`.

- round_digits:

  integer, indicating the number of decimal places (round) to be used
  (default is 3).

- dependencies:

  logical, controlling if a summary of the special dependencies shall be
  printed (see the "special dependency instruction" in the details of
  [flex_prms](https://bucky2177.github.io/dRiftDM/reference/flex_prms.md))

- cust_parameters:

  logical, controlling if a summary of the custom parameters shall be
  printed (see the "additional/custom parameter instruction" in the
  details of
  [flex_prms](https://bucky2177.github.io/dRiftDM/reference/flex_prms.md))

## Value

`summary.flex_prms()` returns a list of class `summary.flex_prms` (see
the Details section summarizing each entry of this list).

`print.summary.flex_prms()` returns invisibly the `summary.flex_prms`
object.

## Details

The `summary.flex_prms()` function creates a summary object containing:

- **prms_matrix**: All parameter values across all conditions.

- **unique_matrix**: A character matrix, showing how parameters relate
  across conditions.

- **depend_strings**: Special Dependencies, formatted as a string.

- **cust_prms_matrix**: (if they exist), a matrix containing all custom
  parameters.

The `print.summary.flex_prms()` function displays the summary object in
a formatted manner.

## Examples

``` r
# create a flex_prms object
flex_obj <- flex_prms(c(a = 1, b = 2), conds = c("foo", "bar"))

sum_obj <- summary(flex_obj)
print(sum_obj)
#> Parameter Values:
#>     a b
#> foo 1 2
#> bar 1 2
#> 
#> Parameter Settings:
#>     a b
#> foo 1 2
#> bar 1 2
#> 

# the print function for the summary object is identical to the print
# function of the flex_prms object
print(flex_obj)
#> Parameter Values:
#>     a b
#> foo 1 2
#> bar 1 2
#> 
#> Parameter Settings:
#>     a b
#> foo 1 2
#> bar 1 2
#> 
```
