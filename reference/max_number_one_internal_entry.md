# Get the maximum number from an internal entry or flex_prms_obj (internal docu)

The entries of the internal list are either digits (0-x) or expressions.

## Usage

``` r
max_number_one_internal_entry(one_internal_entry)

get_number_prms(flex_prms_obj)
```

## Arguments

- one_internal_entry:

  one entry of multiple conditions

- flex_prms_obj:

  a list stored as a flex_prms object

## Value

the largest digit in the entry or of the linear_list in the supplied
flex_prms_obj (0 if there are only expressions). The largest number of
the linear_list corresponds to the number of model parameters.
