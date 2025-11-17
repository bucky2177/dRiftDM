# Set a specific value to the parameter matrix (internal docu)

This function takes a flex_prms_obj and sets certain values to the
parameter matrix, based on the given instruction string (i.e., ' prm ~
conda =\> 0.3)

## Usage

``` r
flex_specific_value(flex_prms_obj, formula_instr)
```

## Arguments

- flex_prms_obj:

  flex_prms object

- formula_instr:

  a string referring to "set" (see
  [modify_flex_prms](https://bucky2177.github.io/dRiftDM/reference/modify_flex_prms.md))

## Value

an updated flex_prms object with a modified prms_matrix object, and (if
applicable) a modified cust_prms matrix
