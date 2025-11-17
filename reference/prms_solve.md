# The Parameters for Deriving Model Predictions

Functions to get or set the "solver settings" of an object. This
includes the diffusion constant and the discretization of the time and
evidence space.

## Usage

``` r
prms_solve(object, ...) <- value

# S3 method for class 'drift_dm'
prms_solve(object, ..., eval_model = FALSE) <- value

prms_solve(object, ...)

# S3 method for class 'drift_dm'
prms_solve(object, ...)

# S3 method for class 'fits_ids_dm'
prms_solve(object, ...)

# S3 method for class 'fits_agg_dm'
prms_solve(object, ...)
```

## Arguments

- object:

  an object of type
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md),
  `fits_ids_dm`, or `fits_agg_dm` (see
  [`estimate_dm()`](https://bucky2177.github.io/dRiftDM/reference/estimate_dm.md)).

- ...:

  additional arguments (i.e., `eval_model`).

- value:

  a named numeric vector providing new values for the `prms_solve`
  vector (see
  [`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)).

- eval_model:

  logical, indicating if the model should be re-evaluated or not when
  updating the solver settings (see
  [re_evaluate_model](https://bucky2177.github.io/dRiftDM/reference/re_evaluate_model.md)).
  Default is `FALSE`.

## Value

For `prms_solve()` the vector `prms_solve` (see
[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)).

For `prms_solve<-()` the updated
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
object.

## Details

`prms_solve()` is a generic accessor function, and `prms_solve<-()` is a
generic replacement function. The default methods get and set the
"solver settings".

It is possible to update parts of the "solver setttings" (i.e., parts of
the underlying `prms_solve` vector). However, modifying `"nx"` or `"nt"`
is not allowed! Any attempts to modify the respective entries will
silently fail (no explicit error/warning etc. is thrown).

## Note

There is only a replacement function for
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
objects. This is because replacing the solver settings after the model
has been fitted (e.g., for a `fits_ids_dm` object) doesn't make sense.

## See also

[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)

## Examples

``` r
# get some default model to demonstrate the prms_solve() functions
my_model <- ratcliff_dm()
# show the discretization and scaling of the model
prms_solve(my_model)
#>   sigma   t_max      dt      dx      nt      nx 
#> 1.0e+00 3.0e+00 7.5e-03 2.0e-02 4.0e+02 1.0e+02 
# partially modify these settings
prms_solve(my_model)[c("dx", "dt")] <- c(0.005)
prms_solve(my_model)
#> sigma t_max    dt    dx    nt    nx 
#> 1e+00 3e+00 5e-03 5e-03 6e+02 4e+02 

# accessor method also available for fits_ids_dm objects
# (see estimate_model_ids)
# get an exemplary fits_ids_dm object
fits <- get_example_fits("fits_ids_dm")
prms_solve(fits)
#>   sigma   t_max      dt      dx      nt      nx 
#> 1.0e+00 3.0e+00 7.5e-03 2.0e-02 4.0e+02 1.0e+02 
```
