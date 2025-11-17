# Create a DDM model — internal

This function assembles all components to create a `drift_dm` object.

## Usage

``` r
new_drift_dm(
  flex_prms_obj,
  sigma,
  t_max,
  dt,
  dx,
  solver,
  comp_funs,
  cost_function,
  subclass,
  b_coding = NULL,
  obs_data = NULL
)
```

## Arguments

- flex_prms_obj:

  a flex_prms object.

- sigma:

  the diffusion noise (`sigma`).

- t_max:

  the maximum trial duration (`t_max`).

- dt:

  the temporal step size (`dt`).

- dx:

  the evidence step size (`dx`).

- solver:

  a string identifying the solver (e.g., `"kfe"`).

- comp_funs:

  a list of component functions.

- cost_function:

  a string, defining how to compute the fit cost.

- subclass:

  a string with model info label set for the child class.

- b_coding:

  an optional list with boundary coding (e.g.,
  [`drift_dm_default_b_coding()`](https://bucky2177.github.io/dRiftDM/reference/defaults.md)).

- obs_data:

  an optional `data.frame` with observed data.

## Value

A list with elements `flex_prms_obj`, `prms_solve`, `solver`,
`comp_funs`, and `cost_function`. The object has class attributes
`c(subclass, "drift_dm")` and an attribute `"b_coding"` containing the
boundary coding. If `obs_data` is not `NULL`, the observed data are
attached via
[`obs_data()`](https://bucky2177.github.io/dRiftDM/reference/obs_data.md).

## Details

We do not perform input checks here; we just assemble the object. Any
pre-wrangling is done in
[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md).
Checks are performed by
[`validate_drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/validate_drift_dm.md),
which is called indirectly via the setters (e.g.,
[`prms_solve()`](https://bucky2177.github.io/dRiftDM/reference/prms_solve.md)
and
[`obs_data()`](https://bucky2177.github.io/dRiftDM/reference/obs_data.md)).

## See also

[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md),
[`validate_drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/validate_drift_dm.md),
[`obs_data()`](https://bucky2177.github.io/dRiftDM/reference/obs_data.md),
[`drift_dm_default_b_coding()`](https://bucky2177.github.io/dRiftDM/reference/defaults.md),
[`prms_solve()`](https://bucky2177.github.io/dRiftDM/reference/prms_solve.md).
