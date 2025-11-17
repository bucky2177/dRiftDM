# Integral of DMC's Drift Rate

Provides the integral of the drift rate of the superimposed decision
process. This is the sum of the rescaled gamma function and the linear
function.

## Usage

``` r
mu_int_dmc(prms_model, prms_solve, t_vec, one_cond, ddm_opts)
```

## Arguments

- prms_model:

  the model parameters, containing muc, tau, a, A

- prms_solve:

  solver settings

- t_vec:

  time space

- one_cond:

  one condition

- ddm_opts:

  optional arguments attached to an object

## Value

provides the scaled gamma distribution function of the superimposed
process for each time step in t_vec.
