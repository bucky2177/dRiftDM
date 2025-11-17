# Update the parameter matrix for vector inputs (internal docu)

This function takes a numeric vector and maps the values to the
parameter matrix using the linearized internal list. This will also lead
to an update of the values for which special dependencies were set

## Usage

``` r
x2prms_vals(x, flex_prms_obj)
```

## Arguments

- x:

  a numeric vector with new values to set

- flex_prms_obj:

  a
  [flex_prms](https://bucky2177.github.io/dRiftDM/reference/flex_prms.md)
  with the (linearized) internal list and the parameter matrix

## Value

a flex_prms_obj with updated parameter matrix

## Details

Does not perform input checks!
