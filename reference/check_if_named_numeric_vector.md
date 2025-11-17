# Check if Object is a Named Numeric Vector

Validates that an object is a named numeric vector with specified
attributes. Optionally checks specific names, length, and restrictions
on label characters.

## Usage

``` r
check_if_named_numeric_vector(
  x,
  var_name,
  labels = NULL,
  length = NULL,
  allow_non_word_chars = FALSE
)
```

## Arguments

- x:

  numeric vector, expected to be named.

- var_name:

  character, the name of the variable to display in error messages.

- labels:

  character vector, optional, specifying valid names for `x`. If
  provided, all names in `x` must match these labels.

- length:

  integer, optional, specifying the exact required length of `x`.

- allow_non_word_chars:

  logical, whether to permit non-word characters in names (default is
  `FALSE`).

## Value

Throws an error if the conditions are not met. If all checks pass, no
output is returned.

## Details

Checks for:

- Numeric type of `x` with non-zero length

- Required length, if specified

- Unique, non-empty names for each entry in `x`

- Match of all names in `x` to `labels`, if `labels` is specified

- Absence of `NA` of `Inf` values in `x`

- Optional absence of non-word names if `allow_non_word_chars` is FALSE
