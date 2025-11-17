# Estimate the Parameters of a drift_dm Model

**\[deprecated\]** This function was deprecated in dRiftDM version
v.0.3.0, please use the more general
[`estimate_dm()`](https://bucky2177.github.io/dRiftDM/reference/estimate_dm.md)
function.

Old documentation: Find the 'best' parameter settings by fitting a
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
models' predicted probability density functions (PDFs) to the observed
data stored within the respective object. The fitting procedure is done
by minimizing the negative log-likelihood of the model.

Users have three options:

- Estimate the parameters via Differential Evolution (Default)

- Estimate the parameters via (bounded) Nelder-Mead

- Use Differential Evolution followed by Nelder-Mead.

See also
[`vignette("dRiftDM", "dRiftDM")`](https://bucky2177.github.io/dRiftDM/articles/dRiftDM.md)

## Usage

``` r
estimate_model(
  drift_dm_obj,
  lower,
  upper,
  verbose = 0,
  use_de_optim = TRUE,
  use_nmkb = FALSE,
  seed = NULL,
  de_n_cores = 1,
  de_control = list(reltol = 1e-08, steptol = 50, itermax = 200, trace = FALSE),
  nmkb_control = list(tol = 1e-06)
)
```

## Arguments

- drift_dm_obj:

  an object inheriting from
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)

- lower, upper:

  numeric vectors or lists, specifying the lower and upper bounds on
  each parameter to be optimized (see Details).

- verbose:

  numeric, indicating the amount of information displayed. If 0, no
  information is displayed (default). If 1, basic information about the
  start of Differential Evolution or Nelder-Mead and the final
  estimation result is given. If 2, each evaluation of the
  log-likelihood function is shown. Note that `verbose` is independent
  of the information displayed by
  [DEoptim::DEoptim](https://rdrr.io/pkg/DEoptim/man/DEoptim.html).

- use_de_optim:

  logical, indicating whether Differential Evolution via
  [DEoptim::DEoptim](https://rdrr.io/pkg/DEoptim/man/DEoptim.html)
  should be used. Default is `TRUE`

- use_nmkb:

  logical, indicating whether Nelder-Mead via
  [dfoptim::nmkb](https://rdrr.io/pkg/dfoptim/man/nmkb.html) should be
  used. Default is `FALSE`.

- seed:

  a single numeric, providing a seed for the Differential Evolution
  algorithm

- de_n_cores:

  a single numeric, indicating the number of cores to use. Run
  [`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html)
  to see how many cores are available on your machine. Note that it is
  generally not recommended to use all of your cores as this will
  drastically slow down your machine for any additional task.

- de_control, nmkb_control:

  lists of additional control parameters passed to
  [DEoptim::DEoptim](https://rdrr.io/pkg/DEoptim/man/DEoptim.html) and
  [dfoptim::nmkb](https://rdrr.io/pkg/dfoptim/man/nmkb.html).

## Value

the updated `drift_dm_obj` (with the estimated parameter values,
log-likelihood, and probability density functions of the first passage
time)

## Details

### Specifying lower/upper

the function `estimate_model` provides a flexible way of specifying the
search space; identical to specifying the parameter simulation space in
[simulate_data.drift_dm](https://bucky2177.github.io/dRiftDM/reference/simulate_data.md).

Users have three options to specify the simulation space:

- Plain numeric vectors (not very much recommended). In this case,
  `lower/upper` must be sorted in accordance with the parameters in the
  `flex_prms_obj` object that vary for at least one condition (call
  `print(drift_dm_obj)` and have a look at the `Parameter Settings`
  output)

- Named numeric vectors. In this case `lower/upper` have to provide
  labels in accordance with the parameters that are considered "free" at
  least once across conditions.

- The most flexible way is when `lower/upper` are lists. In this case,
  the list requires an entry called "default_values" which specifies the
  named or plain numeric vectors as above. If the list only contains
  this entry, then the behavior is as if `lower/upper` were already
  numeric vectors. However, the `lower/upper` lists can also provide
  entries labeled as specific conditions, which contain named (!)
  numeric vectors with parameter labels. This will modify the value for
  the upper/lower parameter space with respect to the specified
  parameters in the respective condition.

### Details on Nelder-Mead and Differential Evolution

If both `use_de_optim` and `use_nmkb` are `TRUE`, then Nelder-Mead
follows Differential Evolution. Note that Nelder-Mead requires a set of
starting parameters for which either the parameter values of
`drift_dm_obj` or the estimated parameter values by Differential
Evolution are used.

Default settings will lead
[DEoptim::DEoptim](https://rdrr.io/pkg/DEoptim/man/DEoptim.html) to stop
if the algorithm is unable to reduce the negative log-likelihood by a
factor of `reltol * (abs(val) + reltol)`after `steptol = 50` steps, with
`reltol = 1e-8` (or if the default itermax of 200 steps is reached).
Similarly, [dfoptim::nmkb](https://rdrr.io/pkg/dfoptim/man/nmkb.html)
will stop if the absolute difference of the log-likelihood between
successive iterations is below `tol = 1e-6`.See
[DEoptim::DEoptim.control](https://rdrr.io/pkg/DEoptim/man/DEoptim.control.html)
and the details of
[dfoptim::nmkb](https://rdrr.io/pkg/dfoptim/man/nmkb.html) for further
information.

## See also

[estimate_model_ids](https://bucky2177.github.io/dRiftDM/reference/estimate_model_ids.md)
