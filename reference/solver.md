# The Solver for Deriving Model Predictions

Functions to get or set the "solver" of an object. The "solver" controls
the method for deriving the model's first passage time (i.e., its
predicted PDFs).

## Usage

``` r
solver(object, ...) <- value

# S3 method for class 'drift_dm'
solver(object, ..., eval_model = FALSE) <- value

solver(object, ...)

# S3 method for class 'drift_dm'
solver(object, ...)

# S3 method for class 'fits_ids_dm'
solver(object, ...)

# S3 method for class 'fits_agg_dm'
solver(object, ...)
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

  a single character string, providing the new "solver" (i.e., approach
  to derive the first passage time; see
  [`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)).

- eval_model:

  logical, indicating if the model should be re-evaluated or not when
  updating the solver (see
  [re_evaluate_model](https://bucky2177.github.io/dRiftDM/reference/re_evaluate_model.md)).
  Default is `FALSE`.

## Value

For [`solve()`](https://rdrr.io/r/base/solve.html) the string `solver`
(see
[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)).

For `solver<-()` the updated
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
object.

## Details

`solver()` is a generic accessor function, and `solver<-()` is a generic
replacement function. The default methods get and set the "solver".

The "solver" indicates the approach with which the PDFs of a model are
calculated. Supported options are "kfe" and "im_zero" (method based on
the Kolmogorov-Forward-Equation or on integral equations, respectively).
Note that "im_zero" is only supported for models that assume a fixed
starting point from 0.

## Note

There is only a replacement function for
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
objects. This is because replacing the approach for deriving PDFs after
the model has been fitted (i.e., for a `fits_ids_dm` object) doesn't
make sense.

## See also

[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)

## Examples

``` r
# get some default model to demonstrate the solver() functions
my_model <- ratcliff_dm()
solver(my_model)
#> [1] "kfe"
# change to the integral approach
solver(my_model) <- "im_zero"
#> Warning: When solver = 'im_zero', use a small 'dt'; 'im_zero' does not yet support dynamic time stepping. It will usually run slower than the 'kfe' solver.
solver(my_model)
#> [1] "im_zero"

# accessor method also available for fits_ids_dm objects
# (see estimate_model_ids)
# get an exemplary fits_ids_dm object
fits <- get_example_fits("fits_ids_dm")
solver(fits)
#> [1] "kfe"
```
