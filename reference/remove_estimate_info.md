# Remove flags added when calling estimate_classical

This is a small internal function that ensures that the flags added
after estimating a model via
[`estimate_classical()`](https://bucky2177.github.io/dRiftDM/reference/estimate_classical.md)
are removed. It is used when replacing settings of a model after it has
been estimated.

## Usage

``` r
remove_estimate_info(drift_dm_obj)
```

## Arguments

- drift_dm_obj:

  a
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
  object

## Value

the model without the list entry `estimate_info`
