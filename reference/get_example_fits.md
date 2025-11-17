# Auxiliary Function to load a `fits_ids_dm`, `fits_agg_dm`, or `mcmc_dm` object

The function is merely helper functions to create an object of type
`fits_ids_dm`, `fits_agg_dm`, or `mcmc_dm`. It is used for example code.

## Usage

``` r
get_example_fits(class, hierarchical = FALSE)
```

## Arguments

- class:

  a string of either `"fits_ids_dm"`, `"fits_agg_dm"`, or `"mcmc_dm"`
  (can be abbreviated)

- hierarchical:

  a logical, relevant when `class = "mcmc_dm"`. If `TRUE`, an object
  from a hierarchical fit is returned. If `FALSE`, an object from an
  individual fit is returned.

## Value

An object of type `fits_ids_dm`, `fits_agg_dm`, or `mcmc_dm`, mimicking
a result from calling
[`estimate_dm()`](https://bucky2177.github.io/dRiftDM/reference/estimate_dm.md).

## Details

For `"fits_ids_dm"`, the returned object comprises DMC (see
[`dmc_dm()`](https://bucky2177.github.io/dRiftDM/reference/dmc_dm.md))
fitted to three participants of the `ulrich_flanker_data`.

For `"fits_agg_dm"`, the returned object comprises the Ratcliff model
(see
[`ratcliff_dm()`](https://bucky2177.github.io/dRiftDM/reference/ratcliff_dm.md))
fitted to synthetic data of three participants.

For `"mcmc_dm"` and `hierarchical = FALSE`, the returned object
comprises the Ratcliff model (see
[`ratcliff_dm()`](https://bucky2177.github.io/dRiftDM/reference/ratcliff_dm.md))
fitted to synthetic data of one participant.

For `"mcmc_dm"` and `hierarchical = TRUE`, the returned object comprises
the Ratcliff model (see
[`ratcliff_dm()`](https://bucky2177.github.io/dRiftDM/reference/ratcliff_dm.md))
fitted to synthetic data of ten participants.

## Examples

``` r
get_example_fits(class = "fits_agg")
#> Fit approach: aggregated - classical
#> Fitted model type: ratcliff_dm, drift_dm
#> Optimizer: nmkb
#> Convergence: TRUE
#> N Individuals: 3 
#> Average Trial Numbers:
#>  100 trials null
```
