# Calculate and Combine Density Estimates for Observed and Predicted Data

Internal helper functions that return a `data.frame` summarizing density
values for observed and predicted response times.

## Usage

``` r
calc_dens_obs(
  rts_u,
  rts_l,
  one_cond,
  t_max = NULL,
  discr = NULL,
  scaling_factor = 1
)

calc_dens(
  pdf_u = NULL,
  pdf_l = NULL,
  t_vec = NULL,
  t_max = NULL,
  discr = NULL,
  rts_u = NULL,
  rts_l = NULL,
  one_cond,
  b_coding,
  scaling_factor = 1
)
```

## Arguments

- rts_u, rts_l:

  vectors of RTs for the upper and lower boundary

- one_cond:

  character label

- t_max:

  a single numeric value specifying the maximum RT to consider. Defaults
  to the smallest multiple of `discr` above the maximum RT. If `t_vec`
  is provided, `t_max` defaults to the maximum value of `t_vec`.

- discr:

  a single numeric value defining the bin width for histogram and KDE
  estimation. Defaults to 0.015 (seconds).

- scaling_factor:

  a single numeric value, multiplied with the PDFs. It is used to scale
  the corresponding probability mass proportional to the number of
  trials per condition. Defaults to `1.0`.

- pdf_u, pdf_l:

  density values for the upper and lower boundary

- t_vec:

  the time space (required for the pdfs)

- b_coding:

  used for accessing the upper/lower boundary labels, determines the
  corresponding columns of the returned data.frame (e.g.,
  Quant\_`corr`).

## Value

A `stats_dm` object (via
[`new_stats_dm()`](https://bucky2177.github.io/dRiftDM/reference/new_stats_dm.md))
containing a `data.frame` with columns:

- `Source`: indicates whether the row is from observed (`"obs"`) or
  predicted (`"pred"`) data.

- `Cond`: the condition label.

- `Time`: the time point corresponding to the density value.

- `Stat`: type of density summary—`"hist"` or `"kde"` (for observed
  data), or `"pdf"` (for predicted data).

- `Dens_<U>`: density value for the upper response.

- `Dens_<L>`: density value for the lower response.

The `<U>` and `<L>` placeholders are determined by the `b_coding`
argument.

## Details

`calc_dens_obs()` computes empirical histograms and kernel density
estimates for a single condition based on observed RTs.

`calc_dens()` serves as a general interface that combines observed and
predicted data into a single `data.frame`. Observed data (`rts_u` and
`rts_l`) is passed to `calc_dens_obs()`. Predicted data (`pdf_u` and
`pdf_l`) is wrapped into a `data.frame` that matches the structure
returned by `calc_dens_obs()`. If both are provided, observed and
predicted data are row-bound into a single `data.frame`.

These functions are used internally to support `type = "density"` in
[`calc_stats()`](https://bucky2177.github.io/dRiftDM/reference/calc_stats.md),
providing a full distributional overview.
