# Format Parameters as String

Converts parameter values into a formatted string.

## Usage

``` r
prms_to_str(x, prms = NULL, round_digits = NULL, sep = "=>", collapse = "\n")
```

## Arguments

- x:

  a
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)object
  or character vector for labels.

- prms:

  Numeric vector of values (used if `x` is character).

- round_digits:

  Rounding precision (default set by
  [`drift_dm_default_rounding()`](https://bucky2177.github.io/dRiftDM/reference/defaults.md)).

- sep:

  Separator between names and values (default: "=\>").

- collapse:

  String to separate each name-value pair (default: "\n").

## Value

A single formatted string with parameter names and values. (e.g., "a =\>
0 \n b =\> 1")

## See also

[`coef.drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/coef.drift_dm.md),
as the numeric vector provided by this call is used when `x` is of type
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
