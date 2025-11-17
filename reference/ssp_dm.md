# Create the Shrinking Spotlight Model

This function creates a
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
object that corresponds to a simple version of the shrinking spotlight
model by White et al. (2011) .

## Usage

``` r
ssp_dm(
  var_non_dec = TRUE,
  var_start = FALSE,
  instr = NULL,
  obs_data = NULL,
  sigma = 1,
  t_max = 3,
  dt = 0.005,
  dx = 0.02,
  b_coding = NULL
)
```

## Arguments

- var_non_dec, var_start:

  logical, indicating whether the model should have a variable
  non-decision time or starting point (see also `nt_uniform` and
  `x_uniform` in
  [component_shelf](https://bucky2177.github.io/dRiftDM/reference/component_shelf.md)

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

- b_coding:

  list, an optional list with the boundary encoding (see
  [b_coding](https://bucky2177.github.io/dRiftDM/reference/b_coding.md))

## Value

An object of type `drift_dm` (parent class) and `ssp_dm` (child class),
created by the function
[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md).

## Details

The shrinking spotlight model is a model developed for the flanker task.

It has the following properties (see
[component_shelf](https://bucky2177.github.io/dRiftDM/reference/component_shelf.md)):

- a constant boundary (parameter `b`)

- a constant starting point in between the decision boundaries

- an evidence accumulation process that is driven by an attention
  spotlight that covers both the flankers and the target. The area that
  covers the flankers and target is modeled by normal distribution with
  mean 0:

  - At the beginning of the trial attention is wide-spread, and the
    width at t=0 is the standard deviation `sd_0`

  - As the trial progresses in time, the attention spotlight narrows,
    reflected by a linear decline of the standard deviation with rate
    `r` (to a minimum of 0.001).

  - the attention attributed to both the flankers and the target is
    scaled by `p` which controls the strength of evidence accumulation

- A non-decision time that follows a truncated normal distribution with
  mean `non_dec` and standard deviation `sd_non_dec`.

- The model also contains the auxiliary parameter `sign`, which is used
  to control the influence of the flankers across conditions. It is not
  really a parameter and should not be estimated!

Per default, the parameter `r` is assumed to be fixed (i.e., is not
estimated freely). The model also contains the custom parameter
`interf_t`, quantifying the interference time (`sd_0 / r`).

## Note

The parameters of SSP in `dRiftDM` differ in their size from the
original publication of White et al. (2011) . `dRiftDM` uses symmetrical
boundaries around zero and a diffusion constant of 1. In the original
publication, SSP was parameterized with boundaries ranging from zero to
`a` and a diffusion constant of 0.1.

Thus, in `dRiftDM`, the boundary `b` corresponds to \\b = a/2 \cdot
10\\. Additionally, `p` in `dRiftDM` is 10 times larger than `p` in the
original publication. Finally, `r` is expressed in seconds, and thus `r`
is 1000 times larger in `dRiftDM` than in the original publication.

## References

White CN, Ratcliff R, Starns JJ (2011). “Diffusion models of the flanker
task: Discrete versus gradual attentional selection.” *Cognitive
psychology*, **63**(4), 210–238.
[doi:10.1016/j.cogpsych.2011.08.001](https://doi.org/10.1016/j.cogpsych.2011.08.001)
.

## Examples

``` r
# the model with default settings
my_model <- ssp_dm()

# the model with a finer discretization
my_model <- ssp_dm(dt = .0025, dx = .01)
```
