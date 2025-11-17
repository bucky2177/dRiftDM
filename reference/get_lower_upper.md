# Get Default Parameter Ranges for a Model

`get_lower_upper()` returns suggested default values for parameter
bounds of a `drift_dm` model. The function inspects the model's
component functions (e.g., drift, boundary, non-decision time, start)
and provides heuristic defaults for some of the pre-built components.
Only parameters that are currently considered *free* in the model are
returned.

## Usage

``` r
get_lower_upper(object, ...)

# S3 method for class 'drift_dm'
get_lower_upper(object, ..., warn = TRUE)
```

## Arguments

- object:

  a
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
  model.

- ...:

  additional arguments passed forward to the respective method.

- warn:

  a single logical, if `TRUE` issue a warning listing components and
  parameters where no defaults could be provided.

## Value

a list with two named numeric vectors:

- `lower` — suggested lower bounds for free parameters

- `upper` — suggested upper bounds for free parameters

## Details

Supported components include: `mu_constant`, `mu_dmc`, `mu_ssp`,
`b_constant`, `x_uniform`, `x_beta`, `nt_constant`, `nt_uniform`,
`nt_truncated_normal`. For some defaults we use the model's
discretization (`dt`, `dx`) to ensure sensible minima.

If a component is not recognized (or refers to currently unsupported
components), no defaults are provided for that component. When
`warn = TRUE`, a single warning lists components without defaults and
any free parameters that remain unmatched. In this case, the user has to
add the missing parameter ranges before attempting to fit the model.

The default ranges are **heuristics** intended to provide a reasonable
starting point for new users. They are not guaranteed to be appropriate
for every model or data set. Always review and, if needed, adjust the
returned values as needed.

## Examples

``` r
# get a model for the example
model <- dmc_dm(obs_data = dmc_synth_data)

# get the parameter ranges
lu <- get_lower_upper(model)
lu$lower
#>        muc          b    non_dec sd_non_dec        tau          A      alpha 
#>     0.5000     0.1500     0.1500     0.0075     0.0150     0.0050     2.0000 
lu$upper
#>        muc          b    non_dec sd_non_dec        tau          A      alpha 
#>       9.00       1.20       0.60       0.10       0.25       0.30       8.00 

# then continue to estimate
# estimate_dm(model, lower = lu$lower, upper = lu$upper, optimizer = "nmkb")
```
