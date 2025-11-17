# Fit Multiple Individuals and Save Results

**\[deprecated\]** This function was deprecated in dRiftDM version
0.3.0. Please use the more general
[`estimate_dm()`](https://bucky2177.github.io/dRiftDM/reference/estimate_dm.md)
instead. NOTE: dRiftDM now supports multiple ways of estimating a model.
To ensure a more consistent function interface, individual fits are no
longer saved to disk when fitting multiple participants. Instead,
[`estimate_dm()`](https://bucky2177.github.io/dRiftDM/reference/estimate_dm.md)
directly returns an object of type `fits_ids_dm`, which users can save
manually if desired.

Old documentation: Provides a wrapper around
[estimate_model](https://bucky2177.github.io/dRiftDM/reference/estimate_model.md)
to fit multiple individuals. Each individual will be stored in a folder.
This folder will also contain a file `drift_dm_fit_info.rds`, containing
the main arguments of the function call. One call to this function is
considered a "fit procedure". Fit procedures can be loaded via
[load_fits_ids](https://bucky2177.github.io/dRiftDM/reference/load_fits_ids.md).

## Usage

``` r
estimate_model_ids(
  drift_dm_obj,
  obs_data_ids,
  lower,
  upper,
  fit_procedure_name,
  fit_path,
  fit_dir = "drift_dm_fits",
  folder_name = fit_procedure_name,
  seed = NULL,
  force_refit = FALSE,
  progress = 2,
  start_vals = NULL,
  ...
)
```

## Arguments

- drift_dm_obj:

  an object inheriting from
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
  that will be estimated for each individual in `obs_data_ids`.

- obs_data_ids:

  data.frame, see
  [obs_data](https://bucky2177.github.io/dRiftDM/reference/obs_data.md).
  An additional column `ID` necessary, to identify a single individual.

- lower, upper:

  numeric vectors or lists, providing the parameter space, see
  [estimate_model](https://bucky2177.github.io/dRiftDM/reference/estimate_model.md).

- fit_procedure_name:

  character, providing a name of the fitting procedure. This name will
  be stored in `drift_dm_fit_info.rds` to identify the fitting
  procedure, see also
  [load_fits_ids](https://bucky2177.github.io/dRiftDM/reference/load_fits_ids.md).

- fit_path:

  character, a path, pointing to the location where all fits shall be
  stored (i.e., `fit_dir` will be created in this location). From the
  user perspective, the path will likely be identical to the current
  working directory.

- fit_dir:

  character, a directory where (multiple) fitting procedures can be
  stored. If the directory does not exist yet, it will be created via
  `base::create.dir(fit_dir, recursive = TRUE)` in the location provided
  by `fit_path`. Default is `"drift_dm_fits"`.

- folder_name:

  character, a folder name for storing all the individual model fits.
  This variable should just state the name, and should not be a path.
  Per default `folder_name` is identical to `fit_procedure_name`.

- seed:

  numeric, a seed to make the fitting procedure reproducable (only
  relevant for differential evolution, see
  [estimate_model](https://bucky2177.github.io/dRiftDM/reference/estimate_model.md)).
  Default is `NULL` which means no seed.

- force_refit:

  logical, if `TRUE` each individual of a fitting routine will be fitted
  once more. Default is `FALSE`.

- progress:

  numerical, indicating if and how progress shall be displayed. If 0, no
  progress is shown. If 1, the currently fitted individual is printed
  out. If 2, a progressbar is shown. Default is 2.

- start_vals:

  optional data.frame, providing values to be set before calling
  [estimate_model](https://bucky2177.github.io/dRiftDM/reference/estimate_model.md).
  Can be used to control the starting values for each individual when
  calling Nelder-Mead. Note that this will only have an effect if
  DEoptim is not used (i.e., when setting `use_de_optim = FALSE`; see
  [estimate_model](https://bucky2177.github.io/dRiftDM/reference/estimate_model.md)).
  The data.frame must provide a column `ID` whose entries match the `ID`
  column in `obs_data_ids`, as well as a column for each parameter of
  the model matching with `coef(drift_dm_obj, select_unique = TRUE)`.

- ...:

  additional arguments passed down to
  [estimate_model](https://bucky2177.github.io/dRiftDM/reference/estimate_model.md).

## Value

nothing (`NULL`; invisibly)

## Details

Examples and more information can also be found in
[`vignette("dRiftDM", "dRiftDM")`](https://bucky2177.github.io/dRiftDM/articles/dRiftDM.md).

When developing the fitting routine we had three levels of files/folders
in mind:

- In a directory/folder named `fit_dir` multiple fitting routines can be
  stored (default is "drift_dm_fits")

- Each fitting routine has its own folder with a name as given by
  `folder_name` (e.g., "ulrich_flanker", "ulrich_simon", ...)

- Within each folder, a file called `drift_dm_fit_info.rds` contains the
  main information about the function call. That is, the time when last
  modifying/calling a fitting routine, the `lower` and `upper` parameter
  boundaries, the `drift_dm_object` that was fitted to each individual,
  the original data set `obs_data_ids`, and the identifier
  `fit_procedure_name`. In the same folder each individual has its own
  `<individual>.rds` file containing the modified `drift_dm_object`.

## See also

[load_fits_ids](https://bucky2177.github.io/dRiftDM/reference/load_fits_ids.md)
