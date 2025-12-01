# Create the Diffusion Model for Conflict Tasks

This function creates a
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
object that corresponds to the Diffusion Model for Conflict Tasks by
Ulrich et al. (2015) .

## Usage

``` r
dmc_dm(
  var_non_dec = TRUE,
  var_start = TRUE,
  instr = NULL,
  obs_data = NULL,
  sigma = 1,
  t_max = 3,
  dt = 0.0075,
  dx = 0.02,
  b_coding = NULL
)
```

## Arguments

- var_non_dec, var_start:

  logical, indicating whether the model should have a
  normally-distributed non-decision time or beta-shaped starting point
  distribution, respectively. (see `nt_truncated_normal` and `x_beta` in
  [component_shelf](https://bucky2177.github.io/dRiftDM/reference/component_shelf.md)).
  Defaults are `TRUE`. If `FALSE`, a constant non-decision time and
  starting point is set (see `nt_constant` and `x_dirac_0` in
  [component_shelf](https://bucky2177.github.io/dRiftDM/reference/component_shelf.md)).

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

An object of type `drift_dm` (parent class) and `dmc_dm` (child class),
created by the function
[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md).

## Details

The Diffusion Model for Conflict Tasks is a model for describing
conflict tasks like the Stroop, Simon, or flanker task.

It has the following properties (see
[component_shelf](https://bucky2177.github.io/dRiftDM/reference/component_shelf.md)):

- a constant boundary (parameter `b`)

- an evidence accumulation process that results from the sum of two
  subprocesses:

  - a controlled process with drift rate `muc`

  - a gamma-shaped process with a scale parameter `tau`, a shape
    parameter `a`, and an amplitude `A`.

If `var_non_dec = TRUE`, a (truncated) normally distributed non-decision
with mean `non_dec` and standard deviation `sd_non_dec` is assumed. If
`var_start = TRUE`, a beta-shaped starting point distribution is assumed
with shape and scale parameter `alpha`.

If `var_non_dec = TRUE`, a constant non-decision time at `non_dec` is
set. If `var_start = FALSE`, a starting point centered between the
boundaries is assumed (i.e., a dirac delta over 0).

Per default the shape parameter `a` is set to 2 and not allowed to vary.
This is because the derivative of the scaled gamma-distribution function
does not exist at `t = 0` for `a < 2`. Currently, we recommend keeping
`a` fixed to 2. If users decide to set `a != 2`, then a small value of
`tol = 0.001` (default) is added to the time vector `t_vec` before
calculating the derivative of the scaled gamma-distribution as
originally introduced by Ulrich et al. (2015) . Users can control this
value by passing a value via
[`ddm_opts()`](https://bucky2177.github.io/dRiftDM/reference/ddm_opts.md)
(see the example below). Note, however, that varying `a` can lead to
large numerical inaccuracies if `a` gets smaller.

The model assumes the amplitude `A` to be negative for incompatible
trials. Also, the model contains the custom parameter `peak_l`,
containing the peak latency (`(a-2)*tau`).

## Note

The scaling of the parameters in `dRiftDM` is different to Ulrich et al.
(2015) . This is because `dRiftDM` works in seconds and with a diffusion
constant of 1, while the original DMC parameterization is in
milliseconds and with a diffusion constant of 4. We describe how to
convert the parameters on our
[website](https://bucky2177.github.io/dRiftDM/articles/convert_dmc_parameters.html).

## References

Ulrich R, Schröter H, Leuthold H, Birngruber T (2015). “Automatic and
controlled stimulus processing in conflict tasks: Superimposed diffusion
processes and delta functions.” *Cognitive Psychology*, **78**, 148–174.
[doi:10.1016/j.cogpsych.2015.02.005](https://doi.org/10.1016/j.cogpsych.2015.02.005)
.

## Examples

``` r
# the model with default settings
my_model <- dmc_dm()

# the model with no variability in the starting point and a finer
# discretization
my_model <- dmc_dm(var_start = FALSE, dt = .005, dx = .01)

# we don't recommend this, but if you really want a != 2, just do...
# (see the Details for more warnings/information about this)
my_model <- dmc_dm(instr = "a ~!")
coef(my_model)["a"] <- 1.9
# -> if you want to control the small value that is added to t_vec when
# calculating the drift rate for a != 2, just use ...
ddm_opts(my_model) <- 0.0001 # ==> t_vec + 0.0001
ddm_opts(my_model) <- NULL # default ==> t_vec + 0.001

```
