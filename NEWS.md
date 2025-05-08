# dRiftDM (development version)

## New features: 

- `calc_stats()` gains the option `basic_stats` for the `type` argument. This 
allows you to request means, standard deviations, and a ratio of response 
choices.

- `simulate_data()` does no longer return RTs exactly matching with the
time domain (i.e., they are no longer limited by step size `dt`). Now, the model
PDFs are first linearly interpolated before conducting inverse transform 
sampling. Users can control the number of decimal places of the simulated RTs
using the argument `round_to`.


## Minor: 

- `nt_constant()` now uses `round()` instead of `as.integer()` to get the 
index for the dirac delta. This reduces the bias in non-decision time estimates.

- `pdfs()` now also returns a vector of the time domain

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
