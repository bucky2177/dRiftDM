# Create a Basic Diffusion Model

This function creates a
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
model that corresponds to the basic Ratcliff Diffusion Model

## Usage

``` r
ratcliff_dm(
  var_non_dec = FALSE,
  var_start = FALSE,
  var_drift = FALSE,
  instr = NULL,
  obs_data = NULL,
  sigma = 1,
  t_max = 3,
  dt = 0.0075,
  dx = 0.02,
  solver = "kfe",
  b_coding = NULL
)
```

## Arguments

- var_non_dec, var_start, var_drift:

  logical, indicating whether the model should have a variable
  non-decision time , starting point (uniform), or drift rate
  (normally-distributed). (see also `nt_uniform` and `x_uniform` in
  [component_shelf](https://bucky2177.github.io/dRiftDM/reference/component_shelf.md))

- instr:

  optional string with "instructions", see
  [`modify_flex_prms()`](https://bucky2177.github.io/dRiftDM/reference/modify_flex_prms.md).

- obs_data:

  data.frame, an optional data.frame with the observed data. See
  [obs_data](https://bucky2177.github.io/dRiftDM/reference/obs_data.md).

- sigma, t_max, dt, dx:

  numeric, providing the settings for the diffusion constant and
  discretization (see
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md))

- solver:

  character, specifying the
  [solver](https://bucky2177.github.io/dRiftDM/reference/solver.md).

- b_coding:

  list, an optional list with the boundary encoding (see
  [b_coding](https://bucky2177.github.io/dRiftDM/reference/b_coding.md))

## Value

An object of type `drift_dm` (parent class) and `ratcliff_dm` (child
class), created by the function
[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md).

## Details

The classical Ratcliff Diffusion Model is a diffusion model with a
constant drift rate `muc` and a constant boundary `b`. If
`var_non_dec = FALSE`, a constant non-decision time `non_dec` is
assumed, otherwise a uniform non-decision time with mean `non_dec` and
range `range_non_dec`. If `var_start = FALSE`, a constant starting point
centered between the boundaries is assumed (i.e., a dirac delta over 0),
otherwise a uniform starting point with mean 0 and range `range_start`.
If `var_drift = FALSE`, a constant drift rate is assumed, otherwise a
normally distributed drift rate with mean `mu_c` and standard deviation
`sd_muc` (can be computationally intensive). Important: Variable drift
rate is only possible with dRiftDM's `mu_constant` function. No custom
drift rate is yet possible in this case.

## References

Ratcliff R (1978). “A theory of memory retrieval.” *Psychological
Review*, **85**(2), 59–108.
[doi:10.1037/0033-295X.85.2.59](https://doi.org/10.1037/0033-295X.85.2.59)
.

## See also

[`component_shelf()`](https://bucky2177.github.io/dRiftDM/reference/component_shelf.md),
[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)

## Examples

``` r
# the model with default settings
my_model <- ratcliff_dm()

# the model with a variable non-decision time and with finer space
# discretization
my_model <- ratcliff_dm(var_non_dec = TRUE, dx = .01)
```
