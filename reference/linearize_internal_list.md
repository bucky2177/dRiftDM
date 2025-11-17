# Relabel the internal list

The entries of the internal list are either digits (0-x) or expressions.
To ensure a valid mapping of these values to an input vector (as done
when an optimizer provides input parameters), we have to linearize the
list (done whenever modifying the list, see the different flex\_\*
functions )

## Usage

``` r
linearize_internal_list(internal_list)
```

## Arguments

- internal_list:

  the internal list, with entries for each parameter x condition
  combination

## Value

another list, but with remapped digits in increasing order, while
leaving expressions or digits of 0 untouched.

## See also

[`flex_vary_prms()`](https://bucky2177.github.io/dRiftDM/reference/flex_vary_prms.md),
[`flex_restrain_prms()`](https://bucky2177.github.io/dRiftDM/reference/flex_restrain_prms.md),
[`flex_fix_prms()`](https://bucky2177.github.io/dRiftDM/reference/flex_fix_prms.md),
[`flex_special_dependency()`](https://bucky2177.github.io/dRiftDM/reference/flex_special_dependency.md)
