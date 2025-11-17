# Calculate Statistics for Model Prediction and/or Observed Data

This function derives statistics that can be calculated for model
predictions and/or observed data. However, it does not calculate it, but
rather calls the respective backend functions. Supported statistics
currently include:

- Basic Summary Statistics (i.e., means and response percentages
  [`calc_basic_stats()`](https://bucky2177.github.io/dRiftDM/reference/calc_basic_stats.md))

- Conditional Accuracy Functions (CAFs;
  [`calc_cafs()`](https://bucky2177.github.io/dRiftDM/reference/calc_cafs.md))

- Quantiles
  ([`calc_quantiles()`](https://bucky2177.github.io/dRiftDM/reference/calc_quantiles.md))

- Delta Functions
  ([`calc_delta_funs()`](https://bucky2177.github.io/dRiftDM/reference/calc_delta_funs.md)).

- Density Estimates
  ([`calc_dens()`](https://bucky2177.github.io/dRiftDM/reference/calc_dens_obs.md)).

## Usage

``` r
calc_stats_pred_obs(type, b_coding, conds, ..., scale_mass = FALSE)
```

## Arguments

- type:

  character string, specifying the type of statistic to calculate.
  Available options are `"basic_stats"`, `"cafs"`, `"quantiles"`,
  `"delta_funs"`, and `"densities"`.

- b_coding:

  list for the boundary coding (see
  [b_coding](https://bucky2177.github.io/dRiftDM/reference/b_coding.md)).

- conds:

  character vector, specifying the conditions to include in calculations
  (used for labeling and subsetting the model PDFs and the observed
  data).

- ...:

  Additional parameters passed on to the specific statistic calculation
  function (see Details).

- scale_mass:

  a single logical, only relevant for density estimation. If `TRUE`, PDF
  masses are scaled proportional to the number of trials per condition.

## Value

A data frame with the calculated statistic across `conds` (ordered
according to `Source`).

## Details

When calling this function the arguments `all_rts_u`/`all_rts_l` and/or
`all_pdfs` must always be specified (see
[re_evaluate_model](https://bucky2177.github.io/dRiftDM/reference/re_evaluate_model.md),
[obs_data](https://bucky2177.github.io/dRiftDM/reference/obs_data.md)).
Otherwise, the backend functions won't work properly. Further arguments
are:

- for CAFS: `n_bins` controls the number of bins, with a default of 5.

- for Quantiles and Delta Functions: `probs` controls the quantiles to
  calculate. Default is `seq(0.1, 0.9, 0.1)` (see
  [`drift_dm_default_probs()`](https://bucky2177.github.io/dRiftDM/reference/defaults.md)).

- for basic summary satistics, Quantiles, and Delta Function:
  `skip_if_contr_low` controls if quantiles and means are calculated for
  PDFs with very small contribution (see also
  [`drift_dm_skip_if_contr_low()`](https://bucky2177.github.io/dRiftDM/reference/defaults.md)).

- for densities: `discr` controls the bin width for the observed data.
  Default is 0.015 seconds

This function gets called by
[`calc_stats()`](https://bucky2177.github.io/dRiftDM/reference/calc_stats.md)
