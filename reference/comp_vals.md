# Evaluate all Component Functions

Gets/calculates all values provided by the component functions of a
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
object

## Usage

``` r
comp_vals(
  drift_dm_obj,
  x_vec = NULL,
  t_vec = NULL,
  nt = NULL,
  dt = NULL,
  nx = NULL,
  dx = NULL,
  prms_solve = NULL,
  solver = NULL,
  prms_matrix = NULL
)
```

## Arguments

- drift_dm_obj:

  an object of type
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)

- x_vec:

  optional, the discretized evidence space

- t_vec:

  optional, the discretized time space

- nx, nt, dx, dt:

  optional, the steps and step sizes of each space

- prms_solve:

  optional, vector of solver settings

- solver:

  optional, string controlling which component values are evaluated

- prms_matrix:

  optional, matrix of parameters

## Value

If solver "kfe", a named list with entries "mu_vals", "x_vals",
"b_vals", "dt_b_vals", "nt_vals".

If solver "im_zero", the returned list will also contain "mu_int_vals".

## Details

arguments are optional, because they can be extracted from the model.
However, supplying these are faster than creating them.
