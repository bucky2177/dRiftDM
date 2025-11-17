# Collapsing Boundary - Hyperbolic Ratio Function

Provides the boundary, collapsing as a hyperbolic ratio function.

## Usage

``` r
b_hyperbol(prms_model, prms_solve, t_vec, one_cond, ddm_opts)

dt_b_hyperbol(prms_model, prms_solve, t_vec, one_cond, ddm_opts)
```

## Arguments

- prms_model:

  the model parameters, containing b0, kappa, t05

- prms_solve:

  solver settings

- t_vec:

  time space

- one_cond:

  one condition

- ddm_opts:

  optional arguments attached to an object

## Value

a vector of the same length as t_vec with the boundary values (or the
deriviative) for each element of the vector.

## Details

`b_hyperbol` and `dt_b_hyperbol` provide the plain boundary values and
the respective derivative, respectively.
