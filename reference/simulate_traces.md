# Simulate Trajectories/Traces of a Model

Simulates single trajectories/traces of a model (i.e., evidence
accumulation processes) using forward Euler.

Might come in handy when exploring the model's behavior or when creating
figures (see also
[plot.traces_dm_list](https://bucky2177.github.io/dRiftDM/reference/plot.traces_dm_list.md))

## Usage

``` r
simulate_traces(object, k, ...)

# S3 method for class 'drift_dm'
simulate_traces(
  object,
  k,
  ...,
  conds = NULL,
  add_x = FALSE,
  sigma = NULL,
  seed = NULL,
  unpack = FALSE
)

# S3 method for class 'fits_ids_dm'
simulate_traces(object, k, ...)

# S3 method for class 'fits_agg_dm'
simulate_traces(object, k, ...)

# S3 method for class 'traces_dm_list'
print(x, ..., round_digits = drift_dm_default_rounding(), print_steps = 5)

# S3 method for class 'traces_dm'
print(
  x,
  ...,
  round_digits = drift_dm_default_rounding(),
  print_steps = 5,
  print_k = 4
)
```

## Arguments

- object:

  an object of type
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md),
  `fits_ids_dm`, or `fits_agg_dm` (see
  [`estimate_dm()`](https://bucky2177.github.io/dRiftDM/reference/estimate_dm.md)).

- k:

  numeric, the number of traces to simulate per condition. Can be a
  named numeric vector, to specify different number of traces per
  condition.

- ...:

  additional arguments passed forward to the respective method.

- conds:

  optional character vector, conditions for which traces shall be
  simulated. If `NULL`, then traces for all conditions are simulated.

- add_x:

  logical, indicating whether traces should contain a variable starting
  point. If `TRUE`, samples from `x_fun` (see
  [comp_vals](https://bucky2177.github.io/dRiftDM/reference/comp_vals.md))
  are added to each trace. Default is `FALSE`.

- sigma:

  optional numeric, providing a value \>= 0 for the diffusion constant
  "sigma" to temporally override
  [prms_solve](https://bucky2177.github.io/dRiftDM/reference/prms_solve.md).
  Useful for exploring the model without noise.

- seed:

  optional numerical, a seed for reproducible sampling

- unpack:

  logical, indicating if the traces shall be "unpacked" (see also
  [unpack_obj](https://bucky2177.github.io/dRiftDM/reference/unpack_obj.md)
  and the return value below).

- x:

  an object of type `traces_dm_list` or `traces_dm`, resulting from a
  call to `simulate_traces`.

- round_digits:

  integer, indicating the number of decimal places (round) to be used
  when printing out the traces (default is 3).

- print_steps:

  integer, indicating the number of steps to show when printing out
  traces (default is 5).

- print_k:

  integer, indicating how many traces shall be shown when printing out
  traces (default is 4).

## Value

`simulate_traces()` returns either an object of type `traces_dm_list`,
or directly a list of matrices across conditions, containing the traces
(if `unpack = TRUE`). If the model has only one condition (and
`unpack = TRUE`), then the matrix of traces for this one condition is
directly returned.

The returned list has as many entries as conditions requested. For
example, if only one condition is requested via the `conds` argument,
then the list is of length 1 (if `unpack = FALSE`). If `conds` is set to
`NULL` (default), then the list will have as many entries as conditions
specified in the supplied `object` (see also
[conds](https://bucky2177.github.io/dRiftDM/reference/conds.md)). If
`unpack = FALSE`, the list contains an additional attribute with the
time space.

Each matrix of traces has `k` rows and `nt + 1` columns, stored as an
array of size (`k`, `nt + 1`). Note that `nt` is the number of steps in
the discretization of time; see
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md).
If `unpack = FALSE`, the array is of type `traces_dm`. It contains some
additional attributes about the time space, the drift rate, the
boundary, the added starting values, if starting values were added, the
original model class and parameters, the boundary coding, and the solver
settings.

The print methods `print.traces_dm_list()` and `print.traces_dm()` each
invisibly return the supplied object `x`.

## Details

`simulate_traces()` is a generic function, applicable to objects of type
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md) or
`fits_ids_dm` (see
[load_fits_ids](https://bucky2177.github.io/dRiftDM/reference/load_fits_ids.md)).

For
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
objects, `simulate_traces()` performs the simulation on the parameter
values currently set (see
[`coef.drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/coef.drift_dm.md)).

For `fits_ids_dm` objects, `simulate_traces()` first extracts the model
and all parameter values for all IDs (see
[`coef.fits_ids_dm()`](https://bucky2177.github.io/dRiftDM/reference/coef.drift_dm.md)).
Subsequently, simulations are based on the averaged parameter values.

The algorithm for simulating traces is forward euler. See Richter et al.
(2023) and Ulrich et al. (2015) (Appendix A) for more information.

## Note

Evidence values with traces beyond the boundary of the model are set to
NA before passing them back.

The reason why `simulate_traces` passes back an object of type
`traces_dm_list` (instead of simply a list of arrays) is to provide a
[plot.traces_dm_list](https://bucky2177.github.io/dRiftDM/reference/plot.traces_dm_list.md)
and print.traces_dm_list function.

Users can unpack the traces even after calling `simulate_traces()` using
[`unpack_obj()`](https://bucky2177.github.io/dRiftDM/reference/unpack_obj.md).

## See also

[`unpack_obj()`](https://bucky2177.github.io/dRiftDM/reference/unpack_obj.md),
[`plot.traces_dm_list()`](https://bucky2177.github.io/dRiftDM/reference/plot.traces_dm_list.md)

## Examples

``` r
# get a pre-built model to demonstrate the function
my_model <- dmc_dm()
some_traces <- simulate_traces(my_model, k = 1, seed = 1)
print(some_traces)
#> Class(es): traces_dm_list
#> 
#> Time space:
#> 0.000, 0.007, 0.015, 0.022 ... 3.000 
#> 
#> Condition: comp 
#> ~>  0.000,  0.027,  0.107,  0.086 ...  0.677 
#> 
#> Condition: incomp 
#> ~>  0.000,  0.143,  0.087,  0.061 ...  0.676 

# a method is also available for fits_ids_dm objects
# (see estimate_model_ids)
# get an exemplary fits_ids_dm object
fits <- get_example_fits("fits_ids_dm")
some_traces <- simulate_traces(fits, k = 1, seed = 1)
print(some_traces)
#> Class(es): traces_dm_list
#> 
#> Time space:
#> 0.000, 0.007, 0.015, 0.022 ... 3.000 
#> 
#> Condition: comp 
#> ~>  0.000,  0.018,  0.099,  0.085 ...  0.516 
#> 
#> Condition: incomp 
#> ~>  0.000,  0.164,  0.119,  0.098 ...  0.487 

# we can also print only the traces of one condition
print(some_traces$comp)
#> ~>  0.000,  0.018,  0.099,  0.085 ...  0.516 
```
