# Specify custom parameters

This function takes a flex_prms_obj and adds or builds the entry
`cust_prms` to allow for custom parameters. An examplary instruction is
"peak_l = (a-1)\*tau"

## Usage

``` r
flex_cust_prm(flex_prms_obj, formula_instr)
```

## Arguments

- flex_prms_obj:

  a flex_prms object

- formula_instr:

  a string referring to "custom parameter combination" (see
  [modify_flex_prms](https://bucky2177.github.io/dRiftDM/reference/modify_flex_prms.md))

## Value

a modified flex_prms object with respect to the `cust_prms` entry

## Details

The entry `cust_prms` is a list with entries `expressions` and `values`.
Each of these is again a named list, that either contains the expression
with instructions on how to calculate the custom parameter (e.g.,
"peak_l") or the respective values. Values are getting
updated/calculated in
[`update_special_values()`](https://bucky2177.github.io/dRiftDM/reference/update_special_values.md)

The `cust_prms` exists of two entries "expressions" and values".
"expressions" contains a named list, with expressions referring to
`prms_matrix` (see
[flex_prms](https://bucky2177.github.io/dRiftDM/reference/flex_prms.md))
on how to calculate the custom parameter (across all conditions). The
"values" contain a named list, with named numeric vectors (names are
conditions, values the calculated custom parameter values)
