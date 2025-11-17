# Load Estimates of a Fit Procedure

**\[deprecated\]** This function was deprecated in dRiftDM version
0.3.0, because dRiftDM no longer saves model fits to disk when fitting
multiple participants. When estimating multiple individuals with the new
function
[`estimate_dm()`](https://bucky2177.github.io/dRiftDM/reference/estimate_dm.md),
an object of type `fits_ids_dm` is returned directly.

## Usage

``` r
load_fits_ids(
  path = "drift_dm_fits",
  fit_procedure_name = "",
  detailed_info = FALSE,
  check_data = TRUE,
  progress = 2
)
```

## Arguments

- path:

  character, a path pointing to a folder or directory containing the
  individual model fits.

- fit_procedure_name:

  character, an optional name that identifies the fit procedure that
  should be loaded

- detailed_info:

  logical, controls the amount of information displayed in case multiple
  fit procedures were found and the user is prompted to explicitly
  choose one

- check_data:

  logical, should the data be checked before passing them back? This
  checks the observed data and the properties of the model. Default is
  `TRUE`

- progress:

  numerical, indicating if and how progress shall be depicted. If 0, no
  progress is shown. If 1, basic infos about the checking progress is
  shown. If 2, multiple progressbars are shown. Default is 2.

## Value

For `load_fits_ids()`, an object of type `fits_ids_dm`, which
essentially is a list with two entries:

- `drift_dm_fit_info`, containing a list of the main arguments when
  [estimate_model_ids](https://bucky2177.github.io/dRiftDM/reference/estimate_model_ids.md)
  was originally called, including a time-stamp.

- `all_fits`, containing a list of all the modified/fitted `drift_dm`
  objects. The list's entry are named according to the individuals'
  identifier (i.e., `ID`).

For
[`print.fits_ids_dm()`](https://bucky2177.github.io/dRiftDM/reference/estimate_dm.md),
the supplied `fit_ids_dm` object `x` (invisible return).

## Details

Old documentation: This function loads the results of a fit procedure
where a model was fitted to multiple individuals (see
[estimate_model_ids](https://bucky2177.github.io/dRiftDM/reference/estimate_model_ids.md)).
It is also the function that creates an object of type `fits_ids_dm`.

with respect to the logic outlined in the details of
[estimate_model_ids](https://bucky2177.github.io/dRiftDM/reference/estimate_model_ids.md)
on the organization of fit procedures, `path` could either point to a
directory with (potentially) multiple fit routines or to a specific
folder with the individual fits. In either case the intended location is
recursively searched for files named `drift_dm_fit_info.rds`.

If the fit procedure was uniquely located, either because only one fit
routine was found in the intended location or because only one
`drift_dm_fit_info.rds` contains the optional identifier specified in
`fit_procedure_name`, then all individual model fits including the
information `fit_procedure_name` are loaded and returned.

In case multiple fit procedures are identified, the user is prompted
with a [utils::menu](https://rdrr.io/r/utils/menu.html), listing
information about the possible candidates. The intended fit procedure
can then interactively be chosen by the user. The amount of displayed
information is controlled via `detailed_info`.

The [`print()`](https://rdrr.io/r/base/print.html) method for objects of
type `fits_ids_dm` prints out basic information about the fit procedure
name, the fitted model, time of (last) call, and the number of
individual data sets.

## See also

[`estimate_model_ids()`](https://bucky2177.github.io/dRiftDM/reference/estimate_model_ids.md)
