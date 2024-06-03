
# dRiftDM

The package dRiftDM was developed to help psychology researchers apply
time-dependent diffusion models in R. It comes with necessary tools for
standard analyses, such as building a model, estimating parameters
across multiple participants (separately per participant), or creating
summary statistics. It also ships with pre-built models. Currently,
these are:

- The Diffusion Model for Conflict Tasks
- The Shrinking Spotlight Model
- The Standard (Ratcliff) Diffusion Model

With version 0.1.1 model predicitons (i.e., their first-passage time)
are derived by numerically solving the Kolmogorov-Forward Equation based
on code provided by Richter et al. (2023, JMP).

## Installation

You can install the development version of dRiftDM from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bucky2177/dRiftDM")
```

## Example

We are in the progress of publishing a tutorial with more detailed
instructions. For now, here is an example on how to fit the Diffusion
Model for Conflict Tasks to some synthetic data using bounded
Nelder-Mead:

``` r
library(dRiftDM)
#> 
#>  ----- 
#> Welcome to dRiftDM 0.1.1 
#> This is a first version... 
#> Please report any bugs/unexpected  
#> behavior to koob@uni-bremen.de 
#>  ------ 
#>     \   ^__^ 
#>      \  (oo)\ ________ 
#>         (__)\         )\ /\ 
#>              ||------w|
#>              ||      ||
dmc_model = dmc_dm(dx = .002, dt = .002, t_max = 1.2)
dmc_model = set_obs_data(drift_dm_obj = dmc_model,
                         obs_data = dmc_synth_data)
print(dmc_model)
#> Class(es): dmc_dm, drift_dm
#> 
#> Model Parameters:
#>   values: muc=4, b=0.6, non_dec=0.3, sd_non_dec=0.02, tau=0.04, a=2, A=0.1, alpha=4
#>   free: muc, b, non_dec, sd_non_dec, tau, A, alpha
#> 
#> Conditions: comp, incomp
#> 
#> Deriving PDFs:
#>   solver: kfe
#>   values: sigma=1, t_max=1.2, dt=0.002, dx=0.002, nt=600, nx=1000
#> 
#> Observed Data: 600 trials
```

``` r
est_model = estimate_model(
  drift_dm_obj = dmc_model,                       # a model
  lower = c(2, 0.4, 0.2, 0.005, 0.02, 0.02, 2),   # lower boundary - search space
  upper = c(6, 0.8, 0.5, 0.050, 0.12, 0.30, 7),   # upper boundary - search space
  verbose = 2,                                    # print out each evaluation
  use_de_optim = F, use_nmkb = T # use Nelder-Mead for demonstration
)
```
