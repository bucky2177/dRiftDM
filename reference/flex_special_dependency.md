# Set special dependencies (internal docu)

Sets special dependencies so that parameters depend on other parameters.
(i.e., 'prmX ~ conda == -(prmY ~ condb)')

## Usage

``` r
flex_special_dependency(flex_prms_obj, formula_instr)
```

## Arguments

- flex_prms_obj:

  a flex_prms object

- formula_instr:

  a string referring to "special dependency" (see
  [modify_flex_prms](https://bucky2177.github.io/dRiftDM/reference/modify_flex_prms.md))

## Value

a modified flex_prms_object with a modified (linear) internal list and
modified parameter and custom parameter matrices
