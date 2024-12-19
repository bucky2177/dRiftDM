
<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/bucky2177/dRiftDM/graph/badge.svg)](https://app.codecov.io/gh/bucky2177/dRiftDM)
[![R-CMD-check](https://github.com/bucky2177/dRiftDM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bucky2177/dRiftDM/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# dRiftDM <img src="man/figures/logo.png" align="right" height="138" alt="" />

The package dRiftDM was developed to assist psychological researchers in
applying and fitting diffusion models to empirical data within the R
environment. Its most important feature is the ability to handle
non-stationary problems, specifically diffusion models with
time-dependent parameters. The package includes essential tools for
standard analyses, such as building models, estimating parameters for
multiple participants (individually for each participant), and creating
summary statistics. The pre-built models available in the package
include:

- The Standard (Ratcliff) Diffusion Model (Ratcliff, 1978, Psychological
  Review)
- The Diffusion Model for Conflict Tasks (Ulrich et al., 2015, Cognitive
  Psychology)
- The Shrinking Spotlight Model (White et al., 2011, Cognitive
  Psychology)

Users can flexibly create custom models and utilize the dRiftDM
machinery for estimating them.

Starting with version 0.2.0, model predictions (i.e., first-passage
times) are derived by numerically solving the Kolmogorov-Forward
Equation or a coupled set of integral equations, based on code provided
by [Richter et al.](https://doi.org/10.1016/j.jmp.2023.102756) (2023,
Journal of Mathematical Psychology).

## Notes

The current version is 0.2.0. Compared to the previous version (0.1.1),
version 0.2.0 makes greater use of the S3 object system. Additionally,
it introduces a new method for handling parameters across conditions
using “flex_prms” objects.

If you wish to install the older version (0.1.1), you can use:

``` r
devtools::install_github("bucky2177/dRiftDM", ref = "0.1.1")
```

## Installation

You can install the development version of dRiftDM from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bucky2177/dRiftDM")
```

The [CRAN](https://cran.r-project.org/) version can be installed with:

``` r
install.packages("dRiftDM")
```

## Example

We are in the process of publishing a tutorial with more detailed
instructions (see the respective [OSF
pre-print](https://osf.io/preprints/osf/3t2vf)). For now, here is an
example of how to fit the Diffusion Model for Conflict Tasks to some
synthetic data using bounded Nelder-Mead:

``` r
library(dRiftDM)
#> 
#>  ---------------------------------------------------- 
#> / Welcome to dRiftDM 0.2.0 Please report any bugs or \
#> \ unexpected behavior                                /
#>  ---------------------------------------------------- 
#>       \
#>        \
#> 
#>         ^__^ 
#>         (oo)\ ________ 
#>         (__)\         )\ /\ 
#>              ||------w|
#>              ||      ||
dmc_model <- dmc_dm(dx = .002, dt = .002, t_max = 1.2)
obs_data(dmc_model) <- dmc_synth_data
print(dmc_model)
#> Class(es): dmc_dm, drift_dm
#> 
#> Current Parameter Matrix:
#>        muc   b non_dec sd_non_dec  tau a    A alpha
#> comp     4 0.6     0.3       0.02 0.04 2  0.1     4
#> incomp   4 0.6     0.3       0.02 0.04 2 -0.1     4
#> 
#> Unique Parameters:
#>        muc b non_dec sd_non_dec tau a A alpha
#> comp   1   2 3       4          5   0 6 7    
#> incomp 1   2 3       4          5   0 d 7    
#> 
#> Special Dependencies:
#> A ~ incomp == -(A ~ comp)
#> 
#> Custom Parameters:
#>        peak_l
#> comp     0.04
#> incomp   0.04
#> 
#> Deriving PDFs:
#>   solver: kfe
#>   values: sigma=1, t_max=1.2, dt=0.002, dx=0.002, nt=600, nx=1000
#> 
#> Observed Data: 300 trials comp; 300 trials incomp
```

``` r
est_model <- estimate_model(
  drift_dm_obj = dmc_model, # a model
  lower = c(2, 0.4, 0.2, 0.005, 0.02, 0.02, 2), # lower boundary - search space
  upper = c(6, 0.8, 0.5, 0.050, 0.12, 0.30, 7), # upper boundary - search space
  verbose = 2, # print out each evaluation
  use_de_optim = F, use_nmkb = T # use Nelder-Mead for demonstration
)
```
