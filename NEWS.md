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
