# Calculate delta function(s)

Given a dataset providing the quantiles
([calc_quantiles](https://bucky2177.github.io/dRiftDM/reference/calc_quantiles.md)),
calculates delta function(s) for the character vectors minuends and
subtrahends

## Usage

``` r
calc_delta_funs(
  quantiles_dat,
  minuends = NULL,
  subtrahends = NULL,
  dvs = NULL,
  b_coding
)
```

## Arguments

- quantiles_dat:

  a data.frame of quantiles
  ([calc_quantiles](https://bucky2177.github.io/dRiftDM/reference/calc_quantiles.md))

- minuends, subtrahends:

  character vectors (with equal length), specifying the conditions to
  use for the delta function: minuend - subtrahend

- dvs:

  character, indicating which quantile columns to use. Default is
  "Quant\_\<u_label\>". If multiple dvs are provided, then minuends and
  subtrahends must have the same length, and matching occurs pairwise.
  In this case, if only one minuend/subtrahend is specified, minuend and
  subtrahend are recycled to the necessary length.

- b_coding:

  a
  [b_coding](https://bucky2177.github.io/dRiftDM/reference/b_coding.md)
  object, necessary to build default dvs

## Value

a data.frame with columns "Source", "Prob", the "Quant\_\<u_label\>",
"Quant\_\<l_label". May have the following additional columns:

- if only one dv: as many Delta\_\<minuend_subtrahend\> and
  Avg\_\<minuends_subtrahends\> as minuends and subtrahends.

- if more than one dv: as many
  Delta\_\<u/l-label\>*\<minuend_subtrahend\> and
  Avg*\<u/l-label\>\_\<minuends_subtrahends\> as minuends and
  subtrahends.

The data.frame is of type c("delta_funs", "sum_dist", "stats_dm",
"data.frame")

## Details

Takes the quantile data_frame,
[stats::reshape](https://rdrr.io/r/stats/reshape.html) it to wide, and
then access the relevant `dv` columns, together with minuends and
subtrahends to calculate the delta functions.

## See also

[`new_stats_dm()`](https://bucky2177.github.io/dRiftDM/reference/new_stats_dm.md)
