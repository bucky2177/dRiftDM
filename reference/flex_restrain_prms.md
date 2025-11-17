# Set parameters as equal across conditions

This function takes a flex_prms object and modifies the (linear)
internal list so that a parameter is set as equal across multiple
conditions, according to the instruction formula (i.e., ' prm ~! conda +
condb)

## Usage

``` r
flex_restrain_prms(flex_prms_obj, formula_instr)
```

## Arguments

- flex_prms_obj:

  flex_prms object

- formula_instr:

  a string referring to "restrain" (see
  [modify_flex_prms](https://bucky2177.github.io/dRiftDM/reference/modify_flex_prms.md))

## Value

a modified flex_prms object with an updated (linear) internal list
