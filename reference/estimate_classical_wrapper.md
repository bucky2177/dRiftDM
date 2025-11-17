# Internal wrapper for classical estimation of individuals

This function wraps
[`estimate_classical()`](https://bucky2177.github.io/dRiftDM/reference/estimate_classical.md)
to handle fitting multiple individuals in a consistent way. It prepares
the data, distributes the estimation across individuals, and manages
parallelization strategies and progress reporting. Unlike the deprecated
[`estimate_model_ids()`](https://bucky2177.github.io/dRiftDM/reference/estimate_model_ids.md),
this function no longer saves results to disk — instead, it directly
returns an object of class `fits_ids_dm`.

## Usage

``` r
estimate_classical_wrapper(
  drift_dm_obj,
  obs_data_ids,
  parallelization_strategy = NULL,
  progress = NULL,
  start_vals = NULL,
  optimizer,
  n_cores = NULL,
  seed = NULL,
  ...
)
```

## Arguments

- drift_dm_obj:

  a
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
  object that will be estimated for each individual in `obs_data_ids`.

- obs_data_ids:

  a [data.frame](https://rdrr.io/r/base/data.frame.html) of observed
  data including an `ID` column that uniquely identifies each
  individual.

- parallelization_strategy:

  an integer, either `1` or `2`. Strategy `1` parallelizes across
  individuals, while strategy `2` parallelizes within individuals (only
  supported for `"DEoptim"`). Default is `1`

- progress:

  an integer, controlling progress output. `0` = no progress, `1` =
  minimal output, `2` = progress bar. Default is `1`

- start_vals:

  an optional `data.frame` with starting values for each individual.
  Must contain an `ID` column matching the IDs in `obs_data_ids`, and
  one column per parameter.

- seed:

  an optional seed to make the results reproducible

- ...:

  further arguments passed to
  [`estimate_classical()`](https://bucky2177.github.io/dRiftDM/reference/estimate_classical.md),
  including `lower`, `upper`, `verbose`, `control`, `round_digits`. Note
  that the argument `return_runs` is not supported.

## Value

an object of class `fits_ids_dm`, which is a list with two components:

- `drift_dm_fit_info` — a list containing the model object, observed
  data, optimizer information, and convergence messages

- `all_fits` — a list of individual estimation results

## Details

Convergence issues are checked automatically. If one or more individuals
fail to converge, a warning is issued with the corresponding IDs and
messages returned by the optimizer.

## See also

[`estimate_classical()`](https://bucky2177.github.io/dRiftDM/reference/estimate_classical.md),
[`estimate_dm()`](https://bucky2177.github.io/dRiftDM/reference/estimate_dm.md)
