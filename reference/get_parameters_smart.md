# Turn default/special parameter specifications to vectors

The function is used in the depths to map parameter inputs to the
parameters of a model. One application is to get the search space as a
vector, matching with the free parameters of a model. Other applications
map, for example, mean values to the free parameters of a model.
Relevant when users use the "default parameters" approach where they
only specify the parameter labels and assume the package figures out how
each parameter relates across conditions (see
[simulate_data](https://bucky2177.github.io/dRiftDM/reference/simulate_data.md)).
This comes in handy, when freeing a parameter across conditions, while
the search space remains the same (otherwise, a user would always have
to adapt the vectors for lower/upper to match with
[x2prms_vals](https://bucky2177.github.io/dRiftDM/reference/x2prms_vals.md))

## Usage

``` r
get_parameters_smart(
  drift_dm_obj,
  input_a,
  input_b = NULL,
  labels = TRUE,
  is_l_u = TRUE,
  fill_up_with = NULL
)
```

## Arguments

- drift_dm_obj:

  an object of type drift_dm

- input_a, input_b:

  either a atomic vector or list (see
  [create_matrix_smart](https://bucky2177.github.io/dRiftDM/reference/create_matrix_smart.md))

- labels:

  optional logical, if `TRUE`, then the returned vectors have the unique
  parameter labels according to
  [prm_cond_combo_2_labels](https://bucky2177.github.io/dRiftDM/reference/prm_cond_combo_2_labels.md).

- is_l_u:

  optional logical, if `TRUE`, a warning is thrown when `input_a` leads
  to larger values than `input_b`. Useful when `input_a` and `input_b`
  span a (search) space.

- fill_up_with:

  optional values used to fill up the returned vectors for all
  parameters that are not specified in `input_a` or `input_b` (requires
  at least one parameter to specified).

## Value

a list with two entries named `vec_a/vec_b`. The length and names (if
requested) matches with coef(model, select_unique = TRUE). When
`input_a` and/or `input_b` is `NULL`, the respective entry for
`vec_a`/`vec_b` will be `NULL` as well.

## Details

The function first gets all unique parameters across conditions using
[prms_cond_combo](https://bucky2177.github.io/dRiftDM/reference/prms_cond_combo.md).
The unique parameter labels are then forwarded to
[create_matrix_smart](https://bucky2177.github.io/dRiftDM/reference/create_matrix_smart.md),
together with all (!) the conditions in the model and the
`input_a`/`input_b` arguments. Subsequently, the created matrices are
wrangled into vectors in accordance with
[prms_cond_combo](https://bucky2177.github.io/dRiftDM/reference/prms_cond_combo.md).
The vectors are then passed back.
