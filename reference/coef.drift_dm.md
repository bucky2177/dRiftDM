# Access Coefficients of a Model

Extract or set the coefficients/parameters objects supported by
[dRiftDM](https://bucky2177.github.io/dRiftDM/reference/dRiftDM-package.md).

## Usage

``` r
coef(object, ...) <- value

# S3 method for class 'drift_dm'
coef(object, ..., eval_model = FALSE) <- value

# S3 method for class 'drift_dm'
coef(object, ..., select_unique = TRUE, select_custom_prms = TRUE)

# S3 method for class 'fits_agg_dm'
coef(object, ...)

# S3 method for class 'fits_ids_dm'
coef(object, ...)

# S3 method for class 'mcmc_dm'
coef(object, ..., .f = mean, id = NULL)

# S3 method for class 'coefs_dm'
print(
  x,
  ...,
  round_digits = drift_dm_default_rounding(),
  print_rows = 10,
  some = FALSE,
  show_header = TRUE,
  show_note = TRUE
)
```

## Arguments

- object:

  an object of type
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md),
  `fits_agg_dm`, `fits_ids_dm` (see also
  [`estimate_dm()`](https://bucky2177.github.io/dRiftDM/reference/estimate_dm.md)),
  or `mcmc_dm`.

- ...:

  additional arguments passed forward (to `coef.drift_dm()` for objects
  of type `fits_agg_dm`; to `.f` for objects of type `mcmc_dm`.

- value:

  numerical, a vector with valid values to update the model's
  parameters. Must match with the number of (unique and free)
  parameters.

- eval_model:

  logical, indicating if the model should be re-evaluated or not when
  updating the parameters (see
  [re_evaluate_model](https://bucky2177.github.io/dRiftDM/reference/re_evaluate_model.md)).
  Default is `FALSE`.

- select_unique:

  logical, indicating if only those parameters shall be returned that
  are considered unique (e.g., when a parameter is set to be identical
  across three conditions, then the parameter is only returned once).
  Default is `TRUE`. This will also return only those parameters that
  are estimated. The argument is currently not supported for objects of
  type `mcmc_dm`.

- select_custom_prms:

  logical, indicating if custom parameters shall be returned as well.
  Only has an effect if `select_unique = FALSE`. The argument is
  currently not supported for objects of type `mcmc_dm`.

- .f:

  the function to be applied to each parameter of a chain. Must either
  return a single value or a vector (with always the same length).
  Default is `mean` (i.e., the mean function).

- id:

  an optional numeric or character vector specifying the IDs of
  participants from whom to summarize samples. Only applicable when the
  model was estimated hierarchically. Use `id = NA` as a shorthand to
  summarize samples for all individuals in the chain object.

- x:

  an object of type `coefs_dm`, as returned by the function
  [`coef()`](https://rdrr.io/r/stats/coef.html) when supplied with a
  `fits_ids_dm` object.

- round_digits:

  integer, controls the number of digits shown. Default is 3.

- print_rows:

  integer, controls the number of rows shown.

- some:

  logical. If `TRUE`, a subset of randomly sampled rows is shown.

- show_header:

  logical. If `TRUE`, a header specifying the type of statistic will be
  displayed.

- show_note:

  logical. If `TRUE`, a footnote is displayed indicating that the
  underlying [data.frame](https://rdrr.io/r/base/data.frame.html) can be
  accessed as usual.

## Value

For objects of type
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md),
`coefs()` returns either a named numeric vector if
`select_unique = TRUE`, or a matrix if `select_unique = FALSE`. If
`select_custom_prms = TRUE`, custom parameters are added to the matrix.

For objects of type `fits_ids_dm`, `coefs()` returns a
[data.frame](https://rdrr.io/r/base/data.frame.html). If
`select_unique = TRUE`, the columns will be the (unique, free)
parameters, together with a column coding `IDs`. If
`select_unique = FALSE`, the columns will be the parameters as listed in
the columns of `prms_matrix` (see
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)),
together with columns coding the conditions and `IDs`. If
`select_custom_prms = TRUE`, the
[data.frame](https://rdrr.io/r/base/data.frame.html) will also contain
columns for the custom parameters. The returned
[data.frame](https://rdrr.io/r/base/data.frame.html) has the class label
`coefs_dm` to easily plot histograms for each parameter (see
[hist.coefs_dm](https://bucky2177.github.io/dRiftDM/reference/hist.coefs_dm.md)).

For objects of type `fits_agg_dm`, returns the same as `coef.drift_dm()`
(i.e., as if calling [`coef()`](https://rdrr.io/r/stats/coef.html) with
an object of type `drift_dm`)

For objects of type `mcmc_dm`, the return type depends on the model
structure and the `.f` output:

- If the model is non-hierarchical or `id` is a single value (not `NA`),
  the function returns either a `vector` or a `matrix`, depending on
  whether `.f` returns a single value or a vector.

- In the hierarchical case, when `id` is a vector or `NA`, the function
  returns a `data.frame`. If `.f` returns a single value, the
  `data.frame` will contain one row per participant (with an `ID` column
  and one column per parameter). If `.f` returns a vector, the
  `data.frame` will include an additional column `.f_out`, coding the
  output of `.f` in long format.

## Details

`coef.*()` are methods for the generic
[`stats::coef()`](https://rdrr.io/r/stats/coef.html) function;
`coefs<-()` is a generic replacement function, currently supporting
objects of type
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md).

The argument `value` supplied to the `coefs<-()` function must match
with the vector returned from `coef(<object>)`. It is possible to update
just part of the (unique) parameters.

Whenever the argument `select_unique` is `TRUE`, `dRiftDM` tries to
provide unique parameter labels.

## See also

[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)

## Examples

``` r
# get a pre-built model and a data set for demonstration purpose
# (when creating the model, set the discretization to reasonable values)
a_model <- dmc_dm()
coef(a_model) # gives the free and unique parameters
#>        muc          b    non_dec sd_non_dec        tau          A      alpha 
#>       4.00       0.60       0.30       0.02       0.04       0.10       4.00 
coef(a_model, select_unique = FALSE) # gives the entire parameter matrix
#>        muc   b non_dec sd_non_dec  tau a    A alpha peak_l
#> comp     4 0.6     0.3       0.02 0.04 2  0.1     4   0.04
#> incomp   4 0.6     0.3       0.02 0.04 2 -0.1     4   0.04
```
