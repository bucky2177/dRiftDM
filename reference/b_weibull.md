# Collapsing Boundary - Weibull Function

Provides the boundary, collapsing in accordance with a Weibull function

## Usage

``` r
b_weibull(prms_model, prms_solve, t_vec, one_cond, ddm_opts)

dt_b_weibull(prms_model, prms_solve, t_vec, one_cond, ddm_opts)
```

## Arguments

- prms_model:

  the model parameters, containing b0, lambda, k, kappa

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

`b_weibull` and `dt_b_weibull` provide the plain boundary values and the
respective derivative, respectively.
