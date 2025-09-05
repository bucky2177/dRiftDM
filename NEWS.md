# dRiftDM (development version)

## Major and New features: 

- `dRiftDM` now uses an adaptive time-stepping scheme when deriving the PDFs.
This allows for a substantial increase in speed.

- `plot()` methods have been changed fundamentally, now avoiding clash of 
arguments and providing more customization options

- `drift_dm()` objects now gain the new feature `cost_function`. This allows
users to either use the `"rmse"` statistic or (full)-range maximum likelihood,
and further allows to fit aggregated data via the `"rmse"` statistic.

- `estimate_dm()` has been introduced.

- `estimate_model()` has been deprecated and superseded with the more general 
`estimate_dm()` function

- `estimate_model_ids()` has been deprecated. The function `estimate_dm()`
should now be used, which doesn't, however, save individual fits to the file 
system (to ensure a more consistent behavior across different ways of fitting
a model).

- `get_lower_upper()` has been introduced. The function provides default upper
and lower parameter ranges for pre-built models and their components.

- `calc_stats.drift_dm()` has gained an additional argument resample.

- Bayesian parameter estimation is now possible with 
`estimate_bayesian()`! However, this function will be already removed 
soon for a more general all-purpose estimation function.

- `calc_stats()` gains the options `basic_stats` and `densities` for the `type` 
argument. `basic_stats` allows you to request means, standard deviations, and a 
ratio of response choices. `densities` allows you to request density values.

 `calc_stats()` gains the `resample` option

- `simulate_data()` does no longer return RTs exactly matching with the
time domain (i.e., they are no longer limited by step size `dt`). Now, the model
PDFs are first linearly interpolated before conducting inverse transform 
sampling. Users can control the number of decimal places of the simulated RTs
using the argument `round_to`.

- `simulate_data()` now supports the `conds` argument.

- `ssp_dm()` gains the arguments `var_non_dec` and `var_start`. This allows 
users to toggle on and off variability in the non-decision time and starting
point.

- `ssp_dm()` uses a uniform variability in the non-decision time, aligning more 
closely with the original publication.

- 

## Minor: 

- `nt_constant()` now uses `round()` instead of `as.integer()` to get the 
index for the dirac delta. This reduces the bias in non-decision time estimates.

- `pdfs()` now also returns a vector of the time domain

- the `coef()` method now supports bayesian returns

- `progress` argument replaced `verbose` in `calc_stats`. Default is 1.

- `"fit_stats"` option for `calc_stats()` now returns multiple fit statistics,
including the log-likelihood, AIC, BIC, and the root-mean squared-error.

## Bug and Code Fixes

- `calc_stats()` is now more precise due to proper numerical integration




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
