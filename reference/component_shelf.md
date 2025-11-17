# Diffusion Model Components

This function is meant as a convenient way to access pre-built model
component functions.

## Usage

``` r
component_shelf()
```

## Value

A list of the respective functions; each entry/function can be accessed
by "name" (see the Example and Details).

## Details

The function provides the following functions:

- `mu_constant`, provides the component function for a constant drift
  rate with parameter `muc`.

- `mu_dmc`, provides the drift rate of the superimposed diffusion
  process of DMC. Necessary parameters are `muc` (drift rate of the
  controlled process), `a` (shape..), `A` (amplitude...), `tau` (scale
  of the automatic process).

- `mu_ssp`, provides the drift rate for SSP. Necessary parameters are
  `p` (perceptual input of flankers and target), `sd_0` (initial
  spotlight width), `r` (shrinking rate of the spotlight) and 'sign' (an
  auxiliary parameter for controlling the contribution of the flanker
  stimuli). Note that no `mu_int_ssp` exists.

- `mu_int_constant`, provides the complementary integral to
  `mu_constant`.

- `mu_int_dmc`, provides the complementary integral to `mu_dmc`.

- `x_dirac_0`, provides a dirac delta for a starting point centered
  between the boundaries (no parameter required).

- `x_uniform`, provides a uniform distribution for a start point
  centered between the boundaries. Requires a parameter `range_start`
  (between 0 and 2).

- `x_beta`, provides the function component for a symmetric beta-shaped
  starting point distribution with parameter `alpha`.

- `b_constant`, provides a constant boundary with parameter `b`.

- `b_hyperbol`, provides a collapsing boundary in terms of a hyperbolic
  ratio function with parameters `b0` as the initial value of the
  (upper) boundary, `kappa` the size of the collapse, and `t05` the
  point in time where the boundary has collapsed by half.

- `b_weibull`, provides a collapsing boundary in terms of a Weibull
  distribution with parameters `b0` as the initial value of the (upper)
  boundary, `lambda` controlling the time of the collapse, `k` the shape
  of the collapse, and `kappa` the size of the collapse.

- `dt_b_constant`, the first derivative of `b_constant`.

- `dt_b_hyperbol`, the first derivative of `b_hyperbol`.

- `nt_constant`, provides a constant non-decision time with parameter
  `non_dec`.

- `nt_uniform`, provides a uniform distribution for the non-decision
  time. Requires the parameters `non_dec` and `range_non_dec`.

- `nt_truncated_normal`, provides the component function for a normally
  distributed non-decision time with parameters `non_dec`, `sd_non_dec`.
  The Distribution is truncated to \\\[0, t\_{max}\]\\.

- `dummy_t` a function that accepts all required arguments for `mu_fun`
  or `mu_int_fun` but which throws an error. Might come in handy when a
  user doesn't require the integral of the drift rate.

See
[`vignette("customize_ddms", "dRiftDM")`](https://bucky2177.github.io/dRiftDM/articles/customize_ddms.md)
for more information on how to set/modify/customize the components of a
diffusion model.

## Examples

``` r
pre_built_functions <- component_shelf()
names(pre_built_functions)
#>  [1] "mu_constant"         "mu_dmc"              "mu_ssp"             
#>  [4] "mu_int_constant"     "mu_int_dmc"          "x_dirac_0"          
#>  [7] "x_beta"              "x_uniform"           "b_constant"         
#> [10] "b_hyperbol"          "b_weibull"           "dt_b_constant"      
#> [13] "dt_b_hyperbol"       "dt_b_weibull"        "nt_constant"        
#> [16] "nt_uniform"          "nt_truncated_normal" "dummy_t"            
```
