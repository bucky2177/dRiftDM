# Generate Prior Functions for Model Parameters

This function creates prior distribution functions for each model
parameter in a drift diffusion model (DDM), depending on the specified
hierarchical level. It returns both log-density functions and, where
applicable, random-sample generators based on the user-defined prior
settings.

## Usage

``` r
get_default_prior_settings(
  drift_dm_obj,
  level,
  means = NULL,
  sds = NULL,
  lower = NULL,
  upper = NULL,
  shapes = NULL,
  rates = NULL
)
```

## Arguments

- drift_dm_obj:

  a
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
  model object.

- level:

  a character string, specifying the modeling level. Must be one of:
  `"hyper"` (group-level priors), `"lower"` (individual-level priors
  given group-level parameters), or `"none"` (non-hierarchical setting).

- means:

  a named numeric vector or list, specifying the prior means for each
  parameter. Missing values will be filled up from the first matching
  parameter in `drift_dm_obj`

- sds:

  a named numeric vector or list of standard deviations. Missing or
  `NULL` values will be replaced by corresponding values from `mean`.

- lower, upper:

  optional numeric vectors or lists specifying the lower and upper
  truncation bounds for each prior distribution. Defaults to `-Inf` and
  `Inf`, respectively.

- shapes, rates:

  optional numeric vectors or lists specifying the shape and rate
  parameter for group-level standard deviations (used at the
  hyper-level). Defaults to `1`.

## Value

A named list with two elements:

- `log_dens_priors`: A named list of functions. Each function returns
  the log-density for a parameter value, based on the chosen prior
  settings.

- `r_priors`: A named list of functions for sampling from the specified
  prior distributions.

## Details

Each prior is parameter-specific and wrapped using
[`purrr::partial()`](https://purrr.tidyverse.org/reference/partial.html)
so that downstream sampling or density evaluation can be performed
easily. At the hyper-level, the functions
[`d_default_prior_hyper()`](https://bucky2177.github.io/dRiftDM/reference/d_default_prior_hyper.md)
and
[`r_default_prior_hyper()`](https://bucky2177.github.io/dRiftDM/reference/d_default_prior_hyper.md)
are used. At the lower-level, the functions
[`dtnorm()`](https://bucky2177.github.io/dRiftDM/reference/dtnorm.md)
and
[`rtnorm()`](https://bucky2177.github.io/dRiftDM/reference/dtnorm.md)
are used.

The input arguments `means`, `sds`, `lowers`, `uppers`, `shapes`, and
`rates` are handled by the function
[`get_parameters_smart()`](https://bucky2177.github.io/dRiftDM/reference/get_parameters_smart.md).

## See also

[`get_parameters_smart()`](https://bucky2177.github.io/dRiftDM/reference/get_parameters_smart.md),
[`dtnorm()`](https://bucky2177.github.io/dRiftDM/reference/dtnorm.md),
[`rtnorm()`](https://bucky2177.github.io/dRiftDM/reference/dtnorm.md),
[`d_default_prior_hyper()`](https://bucky2177.github.io/dRiftDM/reference/d_default_prior_hyper.md),
[`r_default_prior_hyper()`](https://bucky2177.github.io/dRiftDM/reference/d_default_prior_hyper.md)
