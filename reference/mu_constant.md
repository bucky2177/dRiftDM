# Constant Drift Rate

Constant Drift Rate

## Usage

``` r
mu_constant(prms_model, prms_solve, t_vec, one_cond, ddm_opts)
```

## Arguments

- prms_model:

  the model parameters, containing muc

- prms_solve:

  solver settings

- t_vec:

  time space

- one_cond:

  one condition

- ddm_opts:

  optional arguments attached to an object

## Value

a vector of the same length as t_vec with the drift rate for each
element of the vector.
