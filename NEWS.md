# dRiftDM 0.3.0

## Important notes for users

- The workflow for estimating a model has changed with the introduction of
  `estimate_dm()`. From this version onward, fits are not automatically saved to
  the file system to be reloaded later. Instead, `estimate_dm()` returns fitted
  objects directly, and you save the results yourselves.

## Major and new features

- `dRiftDM` now uses an adaptive time-stepping scheme for deriving PDFs ---
  substantially increasing speed.

- We now support variability in the drift rate for the constant drift-rate
  component (not only for the Ratcliff DDM).

- `plot()` methods were redesigned to avoid argument clashes and to provide
  more customization options.

- `drift_dm()` objects gain a new entry `cost_function`. This lets us use the
  `"rmse"` statistic or full-range maximum likelihood, and it enables fitting
  aggregated data via `"rmse"`.

- The `neg_log_like` entry of a `drift_dm` object has been replaced by the more
  general `cost_value`.

- `cost_function()` accessor and replacement methods have been introduced.

- `cost_value()` accessor and replacement methods have been introduced.

- `estimate_dm()` has been introduced.

- `estimate_model()` has been deprecated and superseded by `estimate_dm()`.

- `estimate_model_ids()` has been deprecated. Use `estimate_dm()`, which does
  not save individual fits to the file system --- ensuring more consistent
  behavior across fitting modes.

- `get_lower_upper()` has been introduced. It provides default upper and lower
  parameter ranges for pre-built models and their components.

- Hierarchical and non-hierarchical Bayesian parameter estimation is now
  possible via `estimate_dm()`! This is still experimental, and the returned
  `mcmc_dm` type is not fully integrated yet (currently: diagnostic checks and
  parameter extraction is supported).

- `calc_stats()` gains `basic_stats` and `densities` options for `type`.
  `basic_stats` returns means, standard deviations, and choice proportions;
  `densities` returns density values.

- `calc_stats()` gains a `resample` option to quantify variability in model
  predictions. We can resample for a given model or a single individual, or
  bootstrap an entire sample.

- `calc_stats()` arguments `split_by_ID` and `average` have been superseded by
  the more general `level` argument.

- `simulate_data()` no longer returns RTs restricted to the time grid (step
  size `dt`). PDFs are linearly interpolated for inverse transform sampling. We
  can control RT decimal places via `round_to`.

- `simulate_data()` now supports the `conds` argument.

- `ssp_dm()` gains `var_non_dec` and `var_start` to toggle variability in
  non-decision time and starting point.

- `ssp_dm()` now uses uniform variability in non-decision time, aligning more
  closely with the original publication.
  
- `ssp_dm()` default `dx` and `dt` increase computation speed while balancing
  numerical error for many parameter values.

- `dmc_dm()` default `dx` and `dt` increase computation speed while balancing
  numerical error for many parameter values.

- `ratcliff_dm()` default `dx` and `dt` increase computation speed while
  balancing numerical error for many parameter values.

- `coef()` and `plot()` now support the `mcmc_dm` object type.

- `check_discretization()` has been introduced. This function helps us assess
  the loss in precision when increasing `dt` and `dx`.

- `get_example_fits_ids()` was removed.

- `get_example_fits()` has been introduced to obtain `fits_ids_dm`,
  `fits_agg_dm`, or `mcmc_dm` objects.

## Minor

- `nt_constant()` now uses `round()` instead of `as.integer()` to locate the
  Dirac delta index, reducing bias in non-decision time estimates.

- `pdfs()` now also returns a vector of the time domain.

- The `coef()` and `plot()` method now supports `mcmc_dm` objects.

- The `progress` argument replaces `verbose` in `calc_stats` (default: 1).

- The `"fit_stats"` option for `calc_stats()` now returns multiple fit
  statistics, including log-likelihood, AIC, BIC, and root-mean-squared error.

- `simulate_traces` now properly considers trial-by-trial variability in the
  drift rate.

## Bug and code fixes

- `calc_stats()` is now more precise due to proper numerical integration.



# dRiftDM 0.2.2


## New features: 

- `print()` and `summary()` methods are now available for `traces_dm`,
`traces_dm_list`, `stats_dm`, `stats_dm_list`, and `coefs_dm` objects.

- New `unpack_obj()` makes it easy to strip away attributes and class labels of
objects created by dRiftDM. The more specific predecessor function
`unpack_traces()` is now deprecated `unpack_traces()`.

- New `pdfs()` provides access to a model's predicted probability density
function.


## Minor improvements and changes: 

- `coef.drift_dm()` gains a `select_custom_prms` argument.

- `list_stats_dm` objects are now called `stats_dm_list` for name consistency.

- `traces_dm` objects now have additional attributes (which were required for
appropriate print and summary methods).

- More consistent capitalization in `print.summary.*()` methods.



# dRiftDM 0.2.1

* Initial CRAN submission.
