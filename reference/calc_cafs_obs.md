# Calculate CAFs

Backend functions to calculate conditional accuracy functions for RT
vectors or pdfs

## Usage

``` r
calc_cafs_obs(rts_u, rts_l, one_cond, n_bins)

calc_cafs_pred(pdf_u, pdf_l, t_vec, one_cond, n_bins)
```

## Arguments

- rts_u, rts_l:

  vectors of RTs for the upper and lower boundary

- one_cond:

  character label

- n_bins:

  number of bins to use for the CAFs

- pdf_u, pdf_l:

  density values for the upper and lower boundary

- t_vec:

  the time space

## Value

a data.frame with the "Cond" label, the "Bin"s and "P_U" for the CAFs

## Details

for RTs: first elements are attributed to a bin (with bins calculated
across all RTs using equally spaced quantiles), then accuracy per bin is
calculated.

for Densities: Add density values, calculate a CDF and force it between
0 and

1.  Then determine the indices that cut the CDF into bins by considering
    equally spaced quantiles. Then calculate the ratio of probability
    mass per bin.
