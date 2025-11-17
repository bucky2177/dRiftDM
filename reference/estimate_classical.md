# Estimate Parameters of a `drift_dm` Model via Classical Optimization

`estimate_classical()` estimates the parameters of a
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
model by minimizing the model's cost function (e.g., RMSE or negative
log-likelihood) using classical (non-Bayesian) optimization routines.

Available optimizers include:

- Nelder-Mead (bounded or unbounded): `"Nelder-Mead"`, `"nmkb"` (via
  [`stats::optim()`](https://rdrr.io/r/stats/optim.html) and
  [`dfoptim::nmkb()`](https://rdrr.io/pkg/dfoptim/man/nmkb.html),
  respectively)

- BFGS and L-BFGS-B (via
  [`stats::optim()`](https://rdrr.io/r/stats/optim.html))

- Differential Evolution (via
  [`DEoptim::DEoptim()`](https://rdrr.io/pkg/DEoptim/man/DEoptim.html))

## Usage

``` r
estimate_classical(
  drift_dm_obj,
  optimizer,
  start_vals = NULL,
  return_runs = NULL,
  lower = NULL,
  upper = NULL,
  verbose = NULL,
  de_n_cores = 1,
  control = list(),
  round_digits = NULL,
  seed = NULL,
  use_ez = NULL,
  n_lhs = NULL
)
```

## Arguments

- drift_dm_obj:

  an object inheriting from
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md).

- optimizer:

  a character string specifying the optimizer to use. Must be one of
  `"Nelder-Mead"`, `"nmkb"`, `"BFGS"`, `"L-BFGS-B"`, or `"DEoptim"`.

- start_vals:

  a set of starting values. Must be compatible with
  [`get_parameters_smart()`](https://bucky2177.github.io/dRiftDM/reference/get_parameters_smart.md).
  If `start_vals` is not `NULL`, the function tries to set the provided
  parameter values to the model, using those values as starting values
  for the optimization routine. Special case: If start_vals is a
  `data.frame`, the function is recursively called for each row of
  `start_vals`, providing a handy way to run an optimization routine
  with different starting values. Default is `NULL`, which implies that
  the current model parameters are used as starting values.

- return_runs:

  a single logical. Only relevant when `start_vals` is a `data.frame`
  and the optimization routine is called multiple times with different
  starting values. If `FALSE`, the best-fitting model is returned. If
  `TRUE`, a list is returned, containing the best-fitting model, all
  cost values across runs, and all estimated model parameters across
  runs.

- lower, upper:

  bounds on the parameters to be estimated. Can be numeric vectors,
  named vectors, or flexible lists (see Details).

- verbose:

  an integer (0, 1, or 2). Controls the amount of printed output.

  - 0 = silent

  - 1 = starting/exiting messages

  - 2 = all parameters and the cost value per iteration

- de_n_cores:

  an integer \> 0. Number of CPU cores to use for `DEoptim`.

- control:

  a named list of control parameters passed to the chosen optimizer.

- round_digits:

  an integer. Number of digits to round cost values in printed output.
  If `NULL`, defaults to
  [`drift_dm_default_rounding()`](https://bucky2177.github.io/dRiftDM/reference/defaults.md).

- seed:

  a seed, to make the results of DEoptim reproducible.

## Value

The updated `drift_dm_obj`, with optimized parameters.

## Details

### Search space specification

`lower` and `upper` can be specified flexibly:

- As unnamed numeric vectors (not recommended unless you're sure of the
  parameter order)

- As named numeric vectors matching the parameter names of the model

- As lists with a `default_values` entry (plus optional
  condition-specific entries)

This design mirrors the structure used in
[`simulate_data.drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/simulate_data.md).

### Optimization details

Some optimizers (i.e., `"nmkb"`, `"L-BFGS-B"`, `"DEoptim"`) require both
`lower` and `upper` bounds.

Differential Evolution (`DEoptim`) supports parallelization across cores
via `de_n_cores`. If `de_n_cores > 1`, a parallel cluster is created and
automatically closed after optimization.

The cost function being minimized depends on the
[`cost_function()`](https://bucky2177.github.io/dRiftDM/reference/cost_function.md)
of the model.

During optimization, failed model evaluations yield a very high cost
value (i.e., `.Machine$double.xmax`). In some cases, this ensures that
the optimization doesn't crash, though, this is not guaranteed.
