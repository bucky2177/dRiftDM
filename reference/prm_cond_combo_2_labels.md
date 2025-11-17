# Generate Parameter-Condition Labels

Creates a vector of labels from a parameter-condition combination
matrix, resulting from a call to
[prms_cond_combo](https://bucky2177.github.io/dRiftDM/reference/prms_cond_combo.md).
Used, for instance, in
[coef.drift_dm](https://bucky2177.github.io/dRiftDM/reference/coef.drift_dm.md).

## Usage

``` r
prm_cond_combo_2_labels(prms_cond_combo, sep = ".")
```

## Arguments

- prms_cond_combo:

  a 2-row character matrix where each column represents a unique
  parameter-condition combination.

- sep:

  Separator for parameter and condition labels (default: "~").

## Value

A vector of labels with as many entries as the columns of
`prms_cond_combo` had, combining parameter and condition (if necessary).

If the parameter labels are already unique (because all parameters do
not vary across conditions or are selectively used for one condition),
then only these parameter labels are returned
