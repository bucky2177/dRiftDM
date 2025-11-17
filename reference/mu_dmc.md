# Drift Rate for DMC

Provides the drift rate of the superimposed decision process. That is
the derivative of the rescaled gamma function plus a constant drift rate
for the controlled process.

## Usage

``` r
mu_dmc(prms_model, prms_solve, t_vec, one_cond, ddm_opts)
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

provides the first derivative of the superimposed process with respect
to t_vec.
