# Constant Non-Decision time

A dirac delta on "non_dec", to provide a constant non-decision time.

## Usage

``` r
nt_constant(prms_model, prms_solve, t_vec, one_cond, ddm_opts)
```

## Arguments

- prms_model:

  the model parameters; containing "non_dec"

- prms_solve:

  solver settings

- t_vec:

  time space

- one_cond:

  one condition

- ddm_opts:

  optional arguments attached to an object

## Value

a vector of the same length as t_vec with zeros, except for the element
matching with "non_dec" with respect to "t_vec"
