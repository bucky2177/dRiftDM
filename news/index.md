# Changelog

## dRiftDM 0.3.0

### Important notes for users

- The workflow for estimating a model has changed with the introduction
  of
  [`estimate_dm()`](https://bucky2177.github.io/dRiftDM/reference/estimate_dm.md).
  From this version onward, fits are not automatically saved to the file
  system to be reloaded later. Instead,
  [`estimate_dm()`](https://bucky2177.github.io/dRiftDM/reference/estimate_dm.md)
  returns fitted objects directly, and you save the results yourselves.

### Major and new features

- `dRiftDM` now uses an adaptive time-stepping scheme for deriving PDFs
  — substantially increasing speed.

- We now support variability in the drift rate for the constant
  drift-rate component (not only for the Ratcliff DDM).

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods were
  redesigned to avoid argument clashes and to provide more customization
  options.

- [`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
  objects gain a new entry `cost_function`. This lets us use the
  `"rmse"` statistic or full-range maximum likelihood, and it enables
  fitting aggregated data via `"rmse"`.

- The `neg_log_like` entry of a `drift_dm` object has been replaced by
  the more general `cost_value`.

- [`cost_function()`](https://bucky2177.github.io/dRiftDM/reference/cost_function.md)
  accessor and replacement methods have been introduced.

- [`cost_value()`](https://bucky2177.github.io/dRiftDM/reference/cost_function.md)
  accessor and replacement methods have been introduced.

- [`estimate_dm()`](https://bucky2177.github.io/dRiftDM/reference/estimate_dm.md)
  has been introduced.

- If possible, `dRiftDM` now provides reasonable starting values for the
  Nelder-Mead and BFGS optimization routines (both bounded and
  unbounded). To this end, EZ Diffusion parameter estimates are used
  whenever possible, in combination with grid-search-like procedure.

- [`estimate_model()`](https://bucky2177.github.io/dRiftDM/reference/estimate_model.md)
  has been deprecated and superseded by
  [`estimate_dm()`](https://bucky2177.github.io/dRiftDM/reference/estimate_dm.md).

- [`estimate_model_ids()`](https://bucky2177.github.io/dRiftDM/reference/estimate_model_ids.md)
  has been deprecated. Use
  [`estimate_dm()`](https://bucky2177.github.io/dRiftDM/reference/estimate_dm.md),
  which does not save individual fits to the file system — ensuring more
  consistent behavior across fitting modes.

- [`get_lower_upper()`](https://bucky2177.github.io/dRiftDM/reference/get_lower_upper.md)
  has been introduced. It provides default upper and lower parameter
  ranges for pre-built models and their components.

- Hierarchical and non-hierarchical Bayesian parameter estimation is now
  possible via
  [`estimate_dm()`](https://bucky2177.github.io/dRiftDM/reference/estimate_dm.md)!
  This is still experimental, and the returned `mcmc_dm` type is not
  fully integrated yet (currently: diagnostic checks and parameter
  extraction is supported).

- [`calc_stats()`](https://bucky2177.github.io/dRiftDM/reference/calc_stats.md)
  gains `basic_stats` and `densities` options for `type`. `basic_stats`
  returns means, standard deviations, and choice proportions;
  `densities` returns density values.

- [`calc_stats()`](https://bucky2177.github.io/dRiftDM/reference/calc_stats.md)
  gains a `resample` option to quantify variability in model
  predictions. We can resample for a given model or a single individual,
  or bootstrap an entire sample.

- [`calc_stats()`](https://bucky2177.github.io/dRiftDM/reference/calc_stats.md)
  arguments `split_by_ID` and `average` have been superseded by the more
  general `level` argument.

- [`simulate_data()`](https://bucky2177.github.io/dRiftDM/reference/simulate_data.md)
  no longer returns RTs restricted to the time grid (step size `dt`).
  PDFs are linearly interpolated for inverse transform sampling. We can
  control RT decimal places via `round_to`.

- [`simulate_data()`](https://bucky2177.github.io/dRiftDM/reference/simulate_data.md)
  now supports the `conds` argument.

- [`ssp_dm()`](https://bucky2177.github.io/dRiftDM/reference/ssp_dm.md)
  gains `var_non_dec` and `var_start` to toggle variability in
  non-decision time and starting point.

- [`ssp_dm()`](https://bucky2177.github.io/dRiftDM/reference/ssp_dm.md)
  now uses uniform variability in non-decision time, aligning more
  closely with the original publication.

- [`ssp_dm()`](https://bucky2177.github.io/dRiftDM/reference/ssp_dm.md)
  default `dx` and `dt` increase computation speed while balancing
  numerical error for many parameter values.

- [`dmc_dm()`](https://bucky2177.github.io/dRiftDM/reference/dmc_dm.md)
  default `dx` and `dt` increase computation speed while balancing
  numerical error for many parameter values.

- [`ratcliff_dm()`](https://bucky2177.github.io/dRiftDM/reference/ratcliff_dm.md)
  default `dx` and `dt` increase computation speed while balancing
  numerical error for many parameter values.

- [`coef()`](https://rdrr.io/r/stats/coef.html) and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) now support
  the `mcmc_dm` object type.

- [`check_discretization()`](https://bucky2177.github.io/dRiftDM/reference/check_discretization.md)
  has been introduced. This function helps us assess the loss in
  precision when increasing `dt` and `dx`.

- `get_example_fits_ids()` was removed.

- [`get_example_fits()`](https://bucky2177.github.io/dRiftDM/reference/get_example_fits.md)
  has been introduced to obtain `fits_ids_dm`, `fits_agg_dm`, or
  `mcmc_dm` objects.

### Minor

- [`nt_constant()`](https://bucky2177.github.io/dRiftDM/reference/nt_constant.md)
  now uses [`round()`](https://rdrr.io/r/base/Round.html) instead of
  [`as.integer()`](https://rdrr.io/r/base/integer.html) to locate the
  Dirac delta index, reducing bias in non-decision time estimates.

- [`pdfs()`](https://bucky2177.github.io/dRiftDM/reference/pdfs.md) now
  also returns a vector of the time domain.

- The [`coef()`](https://rdrr.io/r/stats/coef.html) and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method now
  supports `mcmc_dm` objects.

- The `progress` argument replaces `verbose` in `calc_stats` (default:
  1).

- The `"fit_stats"` option for
  [`calc_stats()`](https://bucky2177.github.io/dRiftDM/reference/calc_stats.md)
  now returns multiple fit statistics, including log-likelihood, AIC,
  BIC, and root-mean-squared error.

- `simulate_traces` now properly considers trial-by-trial variability in
  the drift rate.

### Bug and code fixes

- [`calc_stats()`](https://bucky2177.github.io/dRiftDM/reference/calc_stats.md)
  is now more precise due to proper numerical integration.

## dRiftDM 0.2.2

CRAN release: 2025-03-04

### New features:

- [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) methods are now
  available for `traces_dm`, `traces_dm_list`, `stats_dm`,
  `stats_dm_list`, and `coefs_dm` objects.

- New
  [`unpack_obj()`](https://bucky2177.github.io/dRiftDM/reference/unpack_obj.md)
  makes it easy to strip away attributes and class labels of objects
  created by dRiftDM. The more specific predecessor function
  [`unpack_traces()`](https://bucky2177.github.io/dRiftDM/reference/unpack_traces.md)
  is now deprecated
  [`unpack_traces()`](https://bucky2177.github.io/dRiftDM/reference/unpack_traces.md).

- New [`pdfs()`](https://bucky2177.github.io/dRiftDM/reference/pdfs.md)
  provides access to a model’s predicted probability density function.

### Minor improvements and changes:

- [`coef.drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/coef.drift_dm.md)
  gains a `select_custom_prms` argument.

- `list_stats_dm` objects are now called `stats_dm_list` for name
  consistency.

- `traces_dm` objects now have additional attributes (which were
  required for appropriate print and summary methods).

- More consistent capitalization in `print.summary.*()` methods.

## dRiftDM 0.2.1

CRAN release: 2025-01-08

- Initial CRAN submission.
