# Unique Conditions-Parameter Combinations

This is a helper function. It searches through the
`linear_internal_list` of the stored
[flex_prms](https://bucky2177.github.io/dRiftDM/reference/flex_prms.md)
object, and keeps the first unique appearance of parameters. For
example, when the parameter muc is equal for comp, neutral, and incomp,
the function will provide the info "muc" and "comp", thus dropping
incomp and neutral, where the parameter is the same.

## Usage

``` r
prms_cond_combo(drift_dm_obj)
```

## Arguments

- drift_dm_obj:

  an object of type
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)

## Value

a matrix with two rows. Each column contains a combination of the
parameter name and the condition that can be considered unique.
Parameter names are stored in the first row, condition labels in the
second.
