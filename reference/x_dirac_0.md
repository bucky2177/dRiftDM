# Constant Starting Point at Zero

A dirac delta on zero, to provide no bias and a constant starting point

## Usage

``` r
x_dirac_0(prms_model, prms_solve, x_vec, one_cond, ddm_opts)
```

## Arguments

- prms_model:

  the model parameters; no prm name required

- prms_solve:

  solver settings

- x_vec:

  evidence space

- one_cond:

  one condition

- ddm_opts:

  optional arguments attached to an object

## Value

a vector of the same length as x_vec with zeros, except for the element
in the middle of the vector
