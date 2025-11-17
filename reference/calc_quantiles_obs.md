# Calculate Quantiles

Backend functions to calculate quantiles for RT vectors or pdfs

## Usage

``` r
calc_quantiles_obs(rts_u, rts_l, one_cond, probs)

calc_quantiles_pred(
  pdf_u,
  pdf_l,
  t_vec,
  one_cond,
  probs,
  dt,
  skip_if_contr_low = NULL
)
```

## Arguments

- rts_u, rts_l:

  vectors of RTs for the upper and lower boundary

- one_cond:

  character label

- probs:

  numeric vector with values between 0 and 1 for the probability levels

- pdf_u, pdf_l:

  density values for the upper and lower boundary

- t_vec:

  the time space (required for the pdfs)

- dt:

  the step size corresponding to the time space

- skip_if_contr_low:

  numeric. If the contribution of the upper or lower PDF to the overall
  PDF is too low, return NAs for this PDF (see also
  [`drift_dm_skip_if_contr_low()`](https://bucky2177.github.io/dRiftDM/reference/defaults.md)).

## Value

a data.frame with the "Cond" label, the "Prob"s and "Quant_U" and
"Quant_L" for the quantiles

## Details

for RTs: straightforward via
[stats::quantile](https://rdrr.io/r/stats/quantile.html).

for Densities: Calculate CDF (for each pdf separately here), and then
map the desired probability level via the CDF (y-axis) to the time space
(x-axis)
