# Calculate the Log-Likelihood

Wrapper function around `log_like_heart`

## Usage

``` r
calc_log_like(pdfs, t_vec, obs_data)

log_like_heart(pdf_u, pdf_l, t_vec, rts_u, rts_l)
```

## Arguments

- pdfs:

  a list of pdfs (see details)

- t_vec:

  time space

- obs_data:

  a list of obs_data

- pdf_u, pdf_l:

  numeric vectors of the pdfs (unpacked)

- rts_u, rts_l:

  numeric vectors of the observed RTs (unpacked)

## Value

a single value of the log-likelihood. If no data is provided, `NULL` is
returned. If the calculation fails, `-Inf` is returned.

## Details

### calc_log_like

Iterates over all conditions, and passes forward the (unpacked)
arguments to `log_like_heart`, adding each log-likelihood of a
condition.

`pdfs` must be a list with entries named as the conditions, and then
each condition being a list of the two PDFs (named pdf_u and pdf_l)

`obs_data` must be a list with entries "rts_u" and "rts_l", and then
each rts\_\* entry being a named list with the RT values for each
condition

### log_like_heart

Gets the density values for RTs in rts_u/rts_l via
[`stats::approx()`](https://rdrr.io/r/stats/approxfun.html), takes the
log of that, and then sums across both. Wraps up the calculation in a
tryCatch statement, throwing warnings when log_like_values can not be
calculated
