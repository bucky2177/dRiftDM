# Get Quantiles/CAFs from PDFs and stats_agg_info

Internal helper to compute predicted summary statistics from
condition-wise PDFs, following the condition order. Calculates summary
statistics according to the information in `stats_agg_info`. Currently
supports either quantiles or conditional accuracy functions (CAFs).

## Usage

``` r
stats_from_pdfs_agg_info(pdfs, t_vec, dt, stats_agg_info = NULL, what)
```

## Arguments

- pdfs:

  named list of PDFs per condition, each containing elements `pdf_u` and
  `pdf_l`.

- t_vec:

  numeric time vector.

- dt:

  numeric time step.

- stats_agg_info:

  list with information needed to compute summaries (e.g., quantile
  probabilities or CAF bin counts; optional).

- what:

  character, one of `"quantiles"` or `"cafs"`, selecting which statistic
  to compute.

## Value

A list of numeric vectors, one per condition, containing the predicted
quantiles or CAFs. In case of failure, `NA_real_` values are returned.
