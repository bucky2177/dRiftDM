# Drift Rate for SSP

Provides the drift rate for the SSP model. That is, the sum of attention
attributed to the flankers and central target, scaled by the perceptual
input.

## Usage

``` r
mu_ssp(prms_model, prms_solve, t_vec, one_cond, ddm_opts)
```

## Arguments

- prms_model:

  the model parameters, containing p, sd_0, r, sign

- prms_solve:

  solver settings

- t_vec:

  time space

- one_cond:

  one condition

- ddm_opts:

  optional arguments attached to an object

## Value

provides the drift rate for SSP with respect to t_vec
