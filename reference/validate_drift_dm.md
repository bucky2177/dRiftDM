# Validate a DDM object

Performs basic checks to ensure everything is as expected with the
model. This function should be called whenever modifying a ddm object!

## Usage

``` r
validate_drift_dm(drift_dm_obj)
```

## Arguments

- drift_dm_obj:

  the ddm object

## Value

the ddm object, after it passed all checks. Usually, it will be
unmodified. The only exception is when the observed RTs are larger than
`t_max`. Then, the returned ddm object has a new `t_max`that covers the
largest RTs.

## Details

Checks:

- The flex_prms_object via
  [`validate_flex_prms()`](https://bucky2177.github.io/dRiftDM/reference/validate_flex_prms.md)

- The prms_solve (that it is a named numeric vector
  [`check_if_named_numeric_vector()`](https://bucky2177.github.io/dRiftDM/reference/check_if_named_numeric_vector.md)
  with the expected entries) and that nt, nx make sense. This may adjust
  t_max if t_max is smaller than max(RT) of the observed data

- The solver string (only a single string and that it refers to
  something that is actually implemented). If im_zero, then check if
  dirac delta on 0.

- The cost_function string (only a single string and that it refers to
  something that is actually implemented).

- If cost_function is a summary statistic, that the objects stats_agg
  exists and has the correct structure.

- That the list comp_funs only contains functions and that each function
  provides the expected arguments

- If PDFs exist, the names, lengths and data type

- If cost_val exists, if it is a single numeric

- If obs_data exists, the data type, names, and structure

- The b_coding (column, u_name_value and l_name_value).
