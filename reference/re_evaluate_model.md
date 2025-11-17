# Re-evaluate the model

Updates the PDFs of a model. If observed data is available (e.g., via
the
[obs_data](https://bucky2177.github.io/dRiftDM/reference/obs_data.md)
entry or the `stats_agg` entry; depending on the
[cost_function](https://bucky2177.github.io/dRiftDM/reference/cost_function.md),
see also
[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)),
the
[cost_value](https://bucky2177.github.io/dRiftDM/reference/cost_function.md)
is also updated.

## Usage

``` r
re_evaluate_model(drift_dm_obj, eval_model = TRUE)
```

## Arguments

- drift_dm_obj:

  an object of type
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)

- eval_model:

  logical, indicating if the model should be evaluated or not. If
  `FALSE`, PDFs and the value of the cost function are deleted from the
  model. Default is `True`.

## Value

Returns the passed `drift_dm_obj` object, after (re-)calculating the
PDFs and (if observed data is set) the cost_value.

- the PDFs an be addressed via `drift_dm_obj$pdfs`

- the cost_value can be addressed via `drift_dm_obj$cost_value`

Note that if `re_evaluate` model is called before observed data was set,
the function silently updates the `pdfs`, but not `cost_value`.

## Details

More in-depth information about the mathematical details for deriving
the PDFs can be found in Richter et al. (2023)

## See also

[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)

## Examples

``` r
# choose a pre-built model (e.g., the Ratcliff model)
# and set the discretization as needed
my_model <- ratcliff_dm()

# then calculate the model's predicted PDF
my_model <- re_evaluate_model(my_model)
str(my_model$pdfs) # show the structure of the attached pdfs
#> List of 1
#>  $ null:List of 2
#>   ..$ pdf_u: num [1:401] 1e-08 1e-08 1e-08 1e-08 1e-08 ...
#>   ..$ pdf_l: num [1:401] 1e-08 1e-08 1e-08 1e-08 1e-08 ...

# if you want the cost_function, make sure some data is attached to the
# model (see also the documentation of obs_data())
obs_data(my_model) <- ratcliff_synth_data # this data set comes with dRiftDM
my_model <- re_evaluate_model(my_model)
str(my_model$pdfs)
#> List of 1
#>  $ null:List of 2
#>   ..$ pdf_u: num [1:401] 1e-08 1e-08 1e-08 1e-08 1e-08 ...
#>   ..$ pdf_l: num [1:401] 1e-08 1e-08 1e-08 1e-08 1e-08 ...
print(my_model$cost_value)
#> [1] -193.7039
```
