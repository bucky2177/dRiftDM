# Calculate CAFs

Function that calls the underlying CAF calculation functions
[calc_cafs_obs](https://bucky2177.github.io/dRiftDM/reference/calc_cafs_obs.md)
and
[calc_cafs_pred](https://bucky2177.github.io/dRiftDM/reference/calc_cafs_obs.md).
Does input checks and the data wrangling

## Usage

``` r
calc_cafs(
  pdf_u = NULL,
  pdf_l = NULL,
  t_vec = NULL,
  rts_u = NULL,
  rts_l = NULL,
  one_cond,
  n_bins = NULL,
  b_coding
)
```

## Arguments

- pdf_u, pdf_l:

  density values for the upper and lower boundary

- t_vec:

  the time space

- rts_u, rts_l:

  vectors of RTs for the upper and lower boundary

- one_cond:

  character label

- n_bins:

  number of bins to use for the CAFs

- b_coding:

  used for accessing the upper boundary label, determines the
  corresponding column of the returned data.frame (e.g., P\_`corr`).

## Value

a data.frame with "Source", "Cond", "Bin"s, "P\_\<u_label\>" for the
CAFs of type c("cafs", "sum_dist", "stats_dm", "data.frame")

## Details

if pdf_u and pdf_l are not NULL, returns CAFs of the densities

if rts_u and rts_l are not NULL, returns CAFs of the response times

if all are not NULL, returns both.

## See also

[`new_stats_dm()`](https://bucky2177.github.io/dRiftDM/reference/new_stats_dm.md)
