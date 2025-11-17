# How to Convert DMC's parameters

When working with Drift-Diffusion Models (DDMs), like the Diffusion
Model for Conflict Tasks (DMC, Ulrich et al. 2015), a recurring nuisance
is that the size of the model’s parameters depends on the time scale
(seconds vs. milliseconds) and the diffusion constant. For example, in
the original article by Ulrich et al. (2015), DMC was introduced on the
time scale of milliseconds and with a diffusion constant of
$\sigma = 4$. However, it is not uncommon for DDMs to be realized in the
time scale of seconds and/or with a diffusion constant of $\sigma = 1$.

The motivation for this vignette is to show how to convert **DMC’s
parameters** for different time scales and diffusion constants, thereby
answering a frequently asked question (see also Appendix B in Koob et
al. 2023). While the following functions and equations only hold for
DMC, the problem addressed here is a general one and partially applies
to other DDMs as well. Therefore, we hope that this vignette may even be
interesting to researchers that are are not specifically concerned with
DMC.

## The Mathematical Equations

DMC has 8 parameters in its most general form:

- `muc` the drift rate of the controlled process
- `b` the (constant) boundary
- `non_dec` the mean of the (normally-distributed) non-decision time
- `sd_non_dec` the standard deviation of the (normally-distributed)
  non-decision time
- `tau` the scale of the automatic process
- `a` the shape of the automatic process
- `A` the amplitude of the automatic process
- `alpha` the shape parameter of the starting point distribution (a
  symmetric beta-distribution)

### Scaling for the Diffusion Constant

We have to consider the diffusion constant whenever a parameter depends
on the scale of the evidence space (i.e., if it is somehow related to
the scale of the y-axis of the evidence accumulation process). This is
the case for `muc`, `b`, and `A` of DMC. We can transform these
parameters with:

$$\theta_{new} = \theta_{\sigma_{old}} \cdot \frac{\sigma_{new}}{\sigma_{old}}\,.$$
Here, $\theta_{\sigma_{old}}$ is a parameter (e.g., `muc`, `b`, and `A`)
in the scale of the previous diffusion constant, $\sigma_{old}$, and
$\theta_{\sigma_{new}}$ is the transformed parameter after scaling it to
the target diffusion constant, $\sigma_{new}$.

Example: To transform `muc` $= 0.5$ from a diffusion constant of
$\sigma_{old} = 4$ to match a DDM with a diffusion constant of
$\sigma_{new} = 1$, we compute $$0.5 \cdot \frac{1}{4} = 0.125\,.$$

### Scaling the Time Space

Time scaling is a bit more difficult, because we have to consider if and
how a parameter is affected by the time and/or evidence-space scale.

- `non_dec`, `sd_non_dec`, and `tau` depend primarily on the time space.
  The transformation from seconds to milliseconds, or vice versa, is
  straightforward:

$$\theta_{s} = \theta_{ms}/1000\quad\text{and}\quad\theta_{ms} = \theta_{s} \cdot 1000\,.$$

- `b` and `A` depend primarily on the evidence space scale. Yet, we
  still transform them slightly.[¹](#fn1) For these parameters we can
  use

$$\theta_{s} = \theta_{ms}/\sqrt{1000}\quad\text{and}\quad\theta_{ms} = \theta_{s} \cdot \sqrt{1000}\,.$$

- Finally, the drift rate `muc` describes the rate of *evidence
  increase* per *time step*. For it we have to use

$$\theta_{s} = \theta_{ms} \cdot \frac{1000}{\sqrt{1000}}\quad\text{and}\quad\theta_{ms} = \theta_{s} \cdot \frac{\sqrt{1000}}{1000}\,.$$

Example: To transform `muc` $= 0.5$ from milliseconds to seconds, we
calculate $$0.5 \cdot \frac{1000}{\sqrt{1000}} = 15.8\,.$$

## An R Helper Function

It would be tedious to do all the transformations “by hand,” so we
present here a small helper function that implements these
transformations. The function takes a named numeric vector of parameter
values and returns the transformed values.[²](#fn2)

``` r
# Input documentation:
# named_values: a named numeric vector
# sigma_old, sigma_new: the previous and target diffusion constants
# t_from_to: scaling of time (options: ms->s, s->ms, or none)
convert_prms <- function(
  named_values,
  sigma_old = 4,
  sigma_new = 1,
  t_from_to = "ms->s"
) {
  # Some rough input checks
  stopifnot(is.numeric(named_values), is.character(names(named_values)))
  stopifnot(is.numeric(sigma_old), is.numeric(sigma_new))
  t_from_to <- match.arg(t_from_to, choices = c("ms->s", "s->ms", "none"))

  # Internal conversion function (takes a name and value pair, and transforms it)
  internal <- function(name, value) {
    name <- match.arg(
      name,
      choices = c("muc", "b", "non_dec", "sd_non_dec", "tau", "a", "A", "alpha")
    )

    # 1. scale for the diffusion constant
    if (name %in% c("muc", "b", "A")) {
      value <- value * (sigma_new / sigma_old)
    }

    # 2. scale for the time
    # determine the scaling per parameter (assuming s->ms)
    scale <- 1
    if (name %in% c("non_dec", "sd_non_dec", "tau")) {
      scale <- 1000
    }
    if (name %in% c("b", "A")) {
      scale <- sqrt(1000)
    }
    if (name %in% c("muc")) {
      scale <- sqrt(1000) / 1000
    }

    # adapt, depending on the t_from_to argument
    if (t_from_to == "ms->s") {
      scale <- 1 / scale
    }
    if (t_from_to == "none") {
      scale <- 1
    }

    value <- value * scale
  }

  # Apply the internal function to each element
  converted_values <- mapply(FUN = internal, names(named_values), named_values)

  return(converted_values)
}
```

Let’s see if the conversion works by checking if model predictions are
the same before and after scaling:

``` r
dmc_s <- dmc_dm()
prms_solve(dmc_s) # current parameter settings for sigma = 1 and seconds
#>   sigma   t_max      dt      dx      nt      nx 
#> 1.0e+00 3.0e+00 7.5e-03 2.0e-02 4.0e+02 1.0e+02
quants_s <- calc_stats(dmc_s, "quantiles") # calculate predicted quantiles
print(quants_s) # show quantiles
#> Type of Statistic: quantiles
#> 
#>    Source   Cond Prob Quant_corr Quant_err
#> 1    pred   comp  0.1      0.325     0.321
#> 2    pred   comp  0.2      0.346     0.343
#> 3    pred   comp  0.3      0.364     0.362
#> 4    pred   comp  0.4      0.384     0.380
#> 5    pred   comp  0.5      0.406     0.398
#> 6    pred   comp  0.6      0.433     0.419
#> 7    pred   comp  0.7      0.465     0.443
#> 8    pred   comp  0.8      0.507     0.476
#> 9    pred   comp  0.9      0.574     0.531
#> 10   pred incomp  0.1      0.352     0.301
#> ...
#> 
#> (access the data.frame's columns/rows as usual)

# now the same with new diffusion constant of 4 and time scale in milliseconds
dmc_ms <- dmc_dm()
prms_solve(dmc_ms)["sigma"] <- 4 # new diffusion constant
prms_solve(dmc_ms)["t_max"] <- 3000 # 3000 ms is new max time
prms_solve(dmc_ms)["dt"] <- 1 # 1 ms steps
coef(dmc_ms) <- convert_prms(
  named_values = coef(dmc_ms), # the previous parameters
  sigma_old = 1, # diffusion constants
  sigma_new = 4,
  t_from_to = "s->ms" # how shall the time be scaled?
)
quants_ms <- calc_stats(dmc_ms, "quantiles") # calculate predicted quantiles
print(quants_ms) # show quantiles
#> Type of Statistic: quantiles
#> 
#>    Source   Cond Prob Quant_corr Quant_err
#> 1    pred   comp  0.1    325.168   321.132
#> 2    pred   comp  0.2    345.277   342.566
#> 3    pred   comp  0.3    363.914   361.111
#> 4    pred   comp  0.4    383.934   378.931
#> 5    pred   comp  0.5    406.706   397.280
#> 6    pred   comp  0.6    433.356   417.546
#> 7    pred   comp  0.7    465.710   441.869
#> 8    pred   comp  0.8    508.140   474.759
#> 9    pred   comp  0.9    575.097   530.647
#> 10   pred incomp  0.1    352.047   300.528
#> ...
#> 
#> (access the data.frame's columns/rows as usual)
```

Note: The transformed values in the unit of milliseconds and with a
diffusion constant of 4 are similar in size to those obtained by Ulrich
et al. (2015).

``` r
coef(dmc_s)
#>        muc          b    non_dec sd_non_dec        tau          A      alpha 
#>       4.00       0.60       0.30       0.02       0.04       0.10       4.00
coef(dmc_ms)
#>         muc           b     non_dec  sd_non_dec         tau           A 
#>   0.5059644  75.8946638 300.0000000  20.0000000  40.0000000  12.6491106 
#>       alpha 
#>   4.0000000
```

### References

Koob, Valentin, Ian Mackenzie, Rolf Ulrich, Hartmut Leuthold, and Markus
Janczyk. 2023. “The Role of Task-Relevant and Task-Irrelevant
Information in Congruency Sequence Effects: Applying the Diffusion Model
for Conflict Tasks.” *Cognitive Psychology* 140: 101528.

Ulrich, Rolf, Hannes Schröter, Hartmut Leuthold, and Teresa Birngruber.
2015. “Automatic and Controlled Stimulus Processing in Conflict Tasks:
Superimposed Diffusion Processes and Delta Functions.” *Cognitive
Psychology* 78: 148–74.
<https://doi.org/10.1016/j.cogpsych.2015.02.005>.

------------------------------------------------------------------------

1.  They depend indirectly on the time scale through the variance of the
    diffusion process.

2.  We did not export it with dRiftDM because it does not apply to all
    DDMs, only to DMC
