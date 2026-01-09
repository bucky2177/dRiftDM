# Get Starting Values for Model Parameters

I return a named numeric vector of starting values, when possible. If
`use_ez` is `TRUE` and `obs_data` is available, I try to obtain
EZ-diffusion-based guesses for compatible component functions. Remaining
parameters (if any) can be initialized via a simple latin hypercube
search within `lower`/`upper`.

## Usage

``` r
get_starting_values(object, ...)

# S3 method for class 'drift_dm'
get_starting_values(
  object,
  ...,
  lower = NULL,
  upper = NULL,
  verbose = 0,
  use_ez = NULL,
  n_lhs = NULL
)
```

## Arguments

- object:

  supported object, currently only of type
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md).

- ...:

  additional arguments passed to the respective method.

- lower, upper:

  optional named numeric vectors of search bounds. Required for the
  latin hypercube search.

- verbose:

  numeric scalar controlling messages (0 = silent).

- use_ez:

  logical; try EZ-diffusion starting values when possible.

- n_lhs:

  integer; base size factor for latin hypercube sampling (total samples
  are `n_lhs * d`, where `d` is the number of remaining parameters).

## Value

a named numeric vector of starting values, or `NULL` if no reasonable
starting values can be determined.
