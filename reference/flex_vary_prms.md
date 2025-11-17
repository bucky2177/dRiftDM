# Allow parameters to vary

This function takes an object of type flex_prms and a instruction string
that refers to a "vary" instruction (i.e., ab ~ cd + ds). This string is
broken down and unique parameters are introduced for the condition x
parameter combinations

## Usage

``` r
flex_vary_prms(flex_prms_obj, formula_instr)
```

## Arguments

- flex_prms_obj:

  an object of type flex_prms

- formula_instr:

  a string referring to "vary" (see
  [modify_flex_prms](https://bucky2177.github.io/dRiftDM/reference/modify_flex_prms.md))

## Value

an updated flex_prms_obj with an updated (linear) internal list
