# Calculate Basic Mean, Standard Deviations, and Percentages for Observed and Predicted Data

Backend functions to compute the mean response times, standard
deviations of response times, and response proportions; for both
observed RTs or the predicted probability density functions.

## Usage

``` r
calc_basic_stats_obs(rts_u, rts_l, one_cond)

calc_basic_stats_pred(
  pdf_u,
  pdf_l,
  one_cond,
  t_vec,
  dt,
  skip_if_contr_low = NULL
)
```

## Arguments

- rts_u, rts_l:

  numeric, vectors of response times for the upper and lower boundaries.

- one_cond:

  character, a label for the condition.

- pdf_u, pdf_l:

  numeric, vectors of probability density values for the upper and lower
  boundaries.

- t_vec:

  numeric vector, containing the time points corresponding to the
  probability density values.

- dt:

  a single numeric, providing the step size in `t_vec`.

- skip_if_contr_low:

  a single numeric, threshold below which probability densities are
  ignored (default is obtained from
  [`drift_dm_skip_if_contr_low()`](https://bucky2177.github.io/dRiftDM/reference/defaults.md)).

## Value

A [data.frame](https://rdrr.io/r/base/data.frame.html) with columns:

- `Cond`: Condition label.

- `Mean_U`: Mean response time for the upper boundary

- `Mean_L`: Mean response time for the lower boundary

- `SD_U`: Mean response time for the upper boundary

- `SD_L`: Mean response time for the lower boundary

- `P_U`: Proportion of upper-boundary responses.

## Details

- For observed data, calculates mean RTs, standard deviations of RTs,
  and the proportion of upper responses.

- The same statistics are calculated for the probability density values
  (via simple numerical integration)
