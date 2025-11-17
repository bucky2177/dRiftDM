# The Conditions of an Object

Extract the conditions from a (supported) object.

## Usage

``` r
conds(object, ...) <- value

# S3 method for class 'drift_dm'
conds(object, ..., eval_model = FALSE, messaging = TRUE) <- value

conds(object, ...)

# S3 method for class 'drift_dm'
conds(object, ...)

# S3 method for class 'fits_ids_dm'
conds(object, ...)

# S3 method for class 'fits_agg_dm'
conds(object, ...)

# S3 method for class 'data.frame'
conds(object, ...)

# S3 method for class 'traces_dm_list'
conds(object, ...)
```

## Arguments

- object:

  an `R` object, see details

- ...:

  additional arguments passed forward.

- value:

  a character vector, providing labels for the model's new conditions.

- eval_model:

  logical, indicating if the model should be re-evaluated or not when
  updating the conditions (see
  [re_evaluate_model](https://bucky2177.github.io/dRiftDM/reference/re_evaluate_model.md)).
  Default is `FALSE`.

- messaging:

  logical, indicating if messages shall be displayed or not.

## Value

For `conds()` `NULL` or a character vector with the conditions. `NULL`
is given if the object has no conditions (e.g., when a data.frame has no
`Cond` column).

For `conds<-()` the updated
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
object.

## Details

`conds()` is a generic accessor function and `conds<-()` is a generic
replacement function. The replacement method currently only supports
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
objects. The default methods get and set the conditions of an object.

When replacing the conditions of a
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
object, a new
[flex_prms](https://bucky2177.github.io/dRiftDM/reference/flex_prms.md)
object is created and then set to the model, resetting all parameter
specifications and setting all parameter values to those of the
previously first condition. In addition, if data was attached to the
model, the data is removed. This is because there is no meaningful way
for dRiftDM to know how the model should behave for the newly introduced
condition(s), and how these new conditions relate to the old ones.
Messages reminding the user of this behavior are displayed per default.

## See also

[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)

## Examples

``` r
# get a pre-built model to demonstrate the conds() function
my_model <- dmc_dm()
conds(my_model)
#> [1] "comp"   "incomp"

# accessor functions also work with other object types provided by dRiftDM
# (simulated traces; see the documentation of the respective function)
some_traces <- simulate_traces(my_model, k = 1)
conds(some_traces)
#> [1] "comp"   "incomp"

# get an exemplary fits_ids_dm object (see estimate_model_ids)
fits <- get_example_fits("fits_ids_dm")
conds(fits)
#> [1] "comp"   "incomp"

# also works with data.frames that have a "Cond" column
conds(dmc_synth_data)
#> [1] "comp"   "incomp"
```
