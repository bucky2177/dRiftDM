# Calculate Quantiles

Function that calls the underlying quantile calculation functions
[`calc_quantiles_obs()`](https://bucky2177.github.io/dRiftDM/reference/calc_quantiles_obs.md)
and
[`calc_quantiles_pred()`](https://bucky2177.github.io/dRiftDM/reference/calc_quantiles_obs.md).
Does input checks and the data wrangling

## Usage

``` r
calc_quantiles(
  pdf_u = NULL,
  pdf_l = NULL,
  t_vec = NULL,
  dt = NULL,
  rts_u = NULL,
  rts_l = NULL,
  one_cond,
  probs = NULL,
  b_coding,
  skip_if_contr_low = NULL
)
```

## Arguments

- pdf_u, pdf_l:

  density values for the upper and lower boundary

- t_vec:

  the time space (required for the pdfs)

- dt:

  the step size corresponding to the time space

- rts_u, rts_l:

  vectors of RTs for the upper and lower boundary

- one_cond:

  character label

- probs:

  numeric vector with values between 0 and 1 for the probability levels

- b_coding:

  used for accessing the upper/lower boundary labels, determines the
  corresponding columns of the returned data.frame (e.g.,
  Quant\_`corr`).

- skip_if_contr_low:

  numeric. If the contribution of the upper or lower PDF to the overall
  PDF is too low, return NAs for this PDF (see also
  [`drift_dm_skip_if_contr_low()`](https://bucky2177.github.io/dRiftDM/reference/defaults.md)).

## Value

a data.frame with "Source", "Cond", "Prob"s, "Quant\_\<u_label\>",
"Quant\_\<l_label\>" of type c("quantiles", "sum_dist", "stats_dm",
"data.frame")

## Details

if pdf_u and pdf_l are not NULL, returns quantiles for the densities

if rts_u and rts_l are not NULL, returns quantiles for the response
times

if all are not NULL, returns both.

## See also

[`new_stats_dm()`](https://bucky2177.github.io/dRiftDM/reference/new_stats_dm.md)
