# Optional Arguments for the Component Functions

Functions to get or set the optional, user-defined R objects attached to
a model object.

## Usage

``` r
ddm_opts(object, ...) <- value

# S3 method for class 'drift_dm'
ddm_opts(object, ..., eval_model = FALSE) <- value

ddm_opts(object, ...)

# S3 method for class 'drift_dm'
ddm_opts(object, ...)

# S3 method for class 'fits_agg_dm'
ddm_opts(object, ...)
```

## Arguments

- object:

  an object of type
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
  or `fits_agg_dm` (see
  [`estimate_dm()`](https://bucky2177.github.io/dRiftDM/reference/estimate_dm.md)).

- ...:

  additional arguments passed down to the specific method.

- value:

  an arbitrary R object.

- eval_model:

  logical, indicating if the model should be re-evaluated or not after
  attaching the arbitrary R object to the model (see
  [re_evaluate_model](https://bucky2177.github.io/dRiftDM/reference/re_evaluate_model.md)).
  Default is `FALSE`.

## Value

For `ddm_opts()` the optional R object that was once supplied by the
user, or `NULL`.

For `ddm_opts<-()` the updated
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
object.

## Details

When deriving model predictions, the model's component functions (see
[`comp_funs()`](https://bucky2177.github.io/dRiftDM/reference/comp_funs.md))
are evaluated and the returned values are passed forward to dedicated
numerical methods implemented in dRiftDM. To allow users to access
arbitrary R objects within their custom component functions, models may
contain a `ddm_opts` entry (see also
[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
and the end of
[`vignette("customize_ddms", "dRiftDM")`](https://bucky2177.github.io/dRiftDM/articles/customize_ddms.md)
for an example).

`ddm_opts()` is a generic accessor function, and `ddm_opts<-()` is a
generic replacement function. The default methods get and set the
optional R object.

## See also

[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md),
[`comp_funs()`](https://bucky2177.github.io/dRiftDM/reference/comp_funs.md)

## Examples

``` r
# get a pre-built model for demonstration
a_model <- ratcliff_dm()
ddm_opts(a_model) <- "Hello World"
ddm_opts(a_model)
#> [1] "Hello World"
```
