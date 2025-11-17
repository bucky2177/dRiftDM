# Update all prms

This function takes a flex_prms object and updates the prms_matrix
according to the special instructions in `internal_list` and the custom
parameters `cust_prms`

## Usage

``` r
update_special_values(flex_prms_obj)
```

## Arguments

- flex_prms_obj:

  a flex_prms object

## Value

the modified flex_prms_obj (i.e,. with the updated prms_matrix and the
updated cust_prms\$values)
