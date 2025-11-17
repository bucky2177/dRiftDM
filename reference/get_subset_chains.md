# Extract a Subset of MCMC Chains

When calling
[`estimate_bayesian()`](https://bucky2177.github.io/dRiftDM/reference/estimate_bayesian.md),
the MCMC results are packed up as an `mcmc_dm` object. This function is
used in the depths of `dRiftDM` to extract the relevant array of MCMC
samples, depending on whether the model is hierarchical and whether a
participant ID is provided.

## Usage

``` r
get_subset_chains(chains_obj, id = NULL)
```

## Arguments

- chains_obj:

  an object of class`mcmc_dm`.

- id:

  an optional single numeric or character, specifying the `ID` of a
  participant to extract individual-level samples from a hierarchical
  model. Ignored for non-hierarchical models.

## Value

A 3D array of MCMC samples. The first dimension indicates parameters,
the second dimension chains, and the third dimension iterations
