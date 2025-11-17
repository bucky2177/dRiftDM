# Create a matrix for lower and upper

Outsourced, deep inside the package function to avoid large nesting

## Usage

``` r
create_matrix_smart(input, conds, prm_labels = NULL)
```

## Arguments

- input:

  either a list or a vector of numeric values

- conds:

  a character string, conceptually representing the conditions of a
  model

- prm_labels:

  a character string with parameter labels. Used as a fall back when the
  default_values are not labeled (see details)

## Value

a matrix indicating either the upper or lower end of a parameter space.
There will be as many rows as `conds` implies. The number of columns
depend on `input` (matching its length if it is a vector, or matching
the length of the entry "default_values" if it is a list). If `input` is
`NULL`, then `NULL` is returned.

## Details

The goal of this function is to build up a matrix, serving as the upper
or lower end of a parameter space (relevant when simulating data). The
function gets called by
[`get_parameters_smart()`](https://bucky2177.github.io/dRiftDM/reference/get_parameters_smart.md).

It assumes the following: `input` is either a list or a numeric vector.

- The easiest case is when it is a numeric vector. In this case, the
  function builds a matrix with as many rows as entries in `conds`. The
  rows will also be labeled according to `conds`. The column names are
  either the names specified with the numeric vector, or the labels
  specified in `prm_labels`

- The less intuitive case is when `input` is a list. In this case, the
  list requires an entry called "default_values" which specifies the
  named or plain numeric vector as above. If the list only contains this
  entry, then the behavior is as if `input` was already a numeric
  vector. However, the `input` list can also have entries labeled as
  specific conditions, which contain named (!) numeric vectors with
  parameter labels. This will modify the value for the upper/lower
  parameter space with respect to the specified parameters in the
  respective condition.#'

## See also

[`simulate_data()`](https://bucky2177.github.io/dRiftDM/reference/simulate_data.md),
[`simulate_values()`](https://bucky2177.github.io/dRiftDM/reference/simulate_values.md)
