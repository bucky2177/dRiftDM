# RMSE Calculation and Helpers

Internal helpers for computing the root mean squared error (RMSE)
between predicted and observed quantiles and conditional accuracy
functions.

## Usage

``` r
calc_rmse_eval(pdfs, t_vec, dt, stats_agg, stats_agg_info, weight_err = 1.5)

calc_rmse(quants_pred, cafs_pred, quants_obs, cafs_obs, weight_err = 1.5)
```

## Arguments

- pdfs:

  list of PDFs per condition (named).

- t_vec:

  numeric time vector.

- dt:

  numeric time step.

- stats_agg:

  list of observed summary statistics.

- stats_agg_info:

  list with info on quantile probabilities and CAF bins.

- weight_err:

  non-negative numeric scalar; weight factor for CAF error relative to
  quantile error. Default is 1.5

- quants_pred:

  numeric vector of predicted quantiles (already flattened).

- cafs_pred:

  numeric vector of predicted CAFs (already flattened).

- quants_obs:

  numeric vector of observed quantiles (already flattened).

- cafs_obs:

  numeric vector of observed CAFs (already flattened).

## Value

A single numeric RMSE value, or `NULL` if no observed stats were
provided, or `Inf` if predictions failed (contain `NA`).

## Details

- `calc_rmse_eval()` prepares observed and predicted quantiles/CAFs from
  PDFs and aggregated info, then calls `calc_rmse()`.

- `calc_rmse()` computes the weighted RMSE given predicted and observed
  quantiles/CAFs.

## See also

[`stats_from_pdfs_agg_info()`](https://bucky2177.github.io/dRiftDM/reference/stats_from_pdfs_agg_info.md)
