# Compute EZ Diffusion parameters

Internal helper that computes EZ diffusion model parameters for each
condition in a `drift_dm_obj`. The computation is based on the equations
from Wagenmakers et al. (2007) and estimates drift rate (`muc`),
boundary separation (`b`), and non-decision time (`non_dec`).

## Usage

``` r
get_ez_diffusion(drift_dm_obj)
```

## Arguments

- drift_dm_obj:

  a drift diffusion model object containing observed data in `obs_data`,
  including upper (`rts_u`) and lower (`rts_l`) response times per
  condition

## Value

a matrix with rows `muc`, `b`, and `non_dec`

## Details

If `Pc` equals 0, 0.5, or 1, small adjustments are applied to prevent
numerical issues in the logit transformation.
