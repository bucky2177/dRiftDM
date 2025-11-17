# Calculate Basic Statistics for Response Times or Probability Densities

Function that calls the underlying functions
[calc_basic_stats_obs](https://bucky2177.github.io/dRiftDM/reference/calc_basic_stats_obs.md)
and
[calc_basic_stats_pred](https://bucky2177.github.io/dRiftDM/reference/calc_basic_stats_obs.md).
Handles input checks and data wrangling.

## Usage

``` r
calc_basic_stats(
  pdf_u = NULL,
  pdf_l = NULL,
  rts_u = NULL,
  rts_l = NULL,
  one_cond,
  b_coding,
  t_vec = NULL,
  dt = NULL,
  skip_if_contr_low = NULL
)
```

## Arguments

- pdf_u, pdf_l:

  numeric, vectors of probability density values for the upper and lower
  boundaries.

- rts_u, rts_l:

  numeric, vectors of response times for the upper and lower boundaries.

- one_cond:

  character, a label for the condition.

- b_coding:

  list, used for accessing the upper boundary label, determines the
  corresponding column of the returned data.frame (e.g., P\_`corr`).

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

- `Source`: Indicates whether the statistics refer to observed (`obs`)
  or predicted (`pred`) data.

- `Cond`: A condition label.

- `Mean_<u_label>`: Mean response time for the upper boundary.

- `Mean_<l_label>`: Mean response time for the lower boundary.

- `P_<u_label>`: Proportion of upper-boundary responses.

## Details

- If `pdf_u` and `pdf_l` are provided, the function computes statistics
  for the probability densities.

- If `rts_u` and `rts_l` are provided, the function computes statistics
  for the observed RTs.

- If both sets of inputs are provided, both types of statistics are
  computed and returned.

## See also

[calc_basic_stats_obs](https://bucky2177.github.io/dRiftDM/reference/calc_basic_stats_obs.md),
[calc_basic_stats_pred](https://bucky2177.github.io/dRiftDM/reference/calc_basic_stats_obs.md),
[new_stats_dm](https://bucky2177.github.io/dRiftDM/reference/new_stats_dm.md)
