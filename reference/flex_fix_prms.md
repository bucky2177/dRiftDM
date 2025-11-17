# Exclude parameters from being modified (i.e., fix it; internal docu)

This function modifies the (linear) internal list and sets the desired
parameters (based on the instruction string) to 0. This indicates that
this parameter is not altered within the function x2prms_vals (i.e., '
prm \<!\> conda')

## Usage

``` r
flex_fix_prms(flex_prms_obj, formula_instr)
```

## Arguments

- flex_prms_obj:

  a flex_prms object

- formula_instr:

  a string referring to "fix" (see
  [modify_flex_prms](https://bucky2177.github.io/dRiftDM/reference/modify_flex_prms.md))

## Value

a modified flex_prms_obj with respect to the (linear) internal list
