# Access/Replace the Cost Function Label and Access the Cost Function Value

Functions to access/replace the cost function label of a
`dRiftDM object` and to access the current cost function value. The cost
function label codes which cost function is used during estimation
(e.g., the negative log-likelihood). The cost function value indicates
the current value of the cost function given the current set of
parameters and the data.

## Usage

``` r
cost_function(object, ...) <- value

# S3 method for class 'drift_dm'
cost_function(object, ..., eval_model = FALSE) <- value

cost_function(object, ...)

# S3 method for class 'drift_dm'
cost_function(object, ...)

# S3 method for class 'fits_ids_dm'
cost_function(object, ...)

# S3 method for class 'fits_agg_dm'
cost_function(object, ...)

cost_value(object, ...)

# S3 method for class 'drift_dm'
cost_value(object, ...)

# S3 method for class 'fits_ids_dm'
cost_value(object, ...)

# S3 method for class 'fits_agg_dm'
cost_value(object, ...)
```

## Arguments

- object:

  an object of type
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md),
  `fits_ids_dm`, or `fits_agg_dm` (see
  [`estimate_dm()`](https://bucky2177.github.io/dRiftDM/reference/estimate_dm.md)).

- ...:

  additional arguments passed down to
  [`update_stats_agg()`](https://bucky2177.github.io/dRiftDM/reference/update_stats_agg.md)
  when setting the cost function label.

- value:

  a character string, providing the cost function label (options are
  `"neg_log_like"` or `"rmse"`)

- eval_model:

  logical, indicating if the model should be re-evaluated or not when
  updating the conditions (see
  [re_evaluate_model](https://bucky2177.github.io/dRiftDM/reference/re_evaluate_model.md)).
  Default is `FALSE`.

## Value

- `cost_function()` returns a single character string, specifying the
  used cost function

- `cost_function<-()` returns the model object with the updated cost
  function.

- `cost_value()` returns a single numeric if `object` is of type
  `drift_dm` or `fits_agg_dm`. If there is no data attached to an object
  of type `drift_dm`, the function returns `NULL`. If `object` is of
  type `fits_ids_dm`, the function returns a
  [data.frame](https://rdrr.io/r/base/data.frame.html) with all cost
  values across participants.

## See also

[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md),
[`re_evaluate_model()`](https://bucky2177.github.io/dRiftDM/reference/re_evaluate_model.md)

## Examples

``` r
# get a pre-built model for demonstration purpose
a_model <- ratcliff_dm(obs_data = ratcliff_synth_data)
cost_function(a_model)
#> [1] "neg_log_like"
cost_value(a_model)
#> [1] -193.7039

# switch the default cost function to rmse
cost_function(a_model) <- "rmse"
out <- estimate_dm(a_model, verbose = 0, messaging = FALSE)
# -> the model was estimated using the RMSE statistic

```
