
<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/bucky2177/dRiftDM/graph/badge.svg)](https://app.codecov.io/gh/bucky2177/dRiftDM)
[![R-CMD-check](https://github.com/bucky2177/dRiftDM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bucky2177/dRiftDM/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# dRiftDM <img src="man/figures/logo.png" align="right" height="138" alt="" />

The package dRiftDM was developed to assist psychology researchers in
applying and fitting diffusion models to empirical data within the R
environment. Its most important feature is the ability to handle
non-stationary problems, specifically diffusion models with
time-dependent parameters. The package includes essential tools for
standard analyses, such as building models, estimating parameters for
multiple participants (individually for each participant), and creating
summary statistics. The pre-built models available in the package are:

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

Compared to the previous version 0.1.1, versions \>0.2.0 make greater
use of the S3 object system. Additionally, beginning with version 0.2.0,
models use “flex_prms” objects to handle parameters across conditions.

To install the older version (0.1.1), you can use:

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

## How to use dRiftDM

If you are interested in getting started with dRiftDM, we recommend
reading the [OSF pre-print](https://osf.io/preprints/osf/3t2vf). More
information on functions and model customization can be found in
dRiftDM’s vignettes. These vignettes are also available from the
“Getting started” and “Articles” tabs on our [Github.io
page](https://bucky2177.github.io/dRiftDM/).

If you have any questions, feel free to contact us!
