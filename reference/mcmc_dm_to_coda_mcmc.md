# Convert MCMC Chain Array to a `coda::mcmc.list` Object

Converts a 3D MCMC chain array (parameters × chains × iterations) into a
[`coda::mcmc.list`](https://rdrr.io/pkg/coda/man/mcmc.list.html) object
for compatibility with diagnostic and summary functions from the `coda`
package.

## Usage

``` r
mcmc_dm_to_coda_mcmc(chains)
```

## Arguments

- chains:

  a 3D numeric array with dimensions corresponding to parameters ×
  chains × iterations. `chains` are typically obtained from a call to
  [`get_subset_chains()`](https://bucky2177.github.io/dRiftDM/reference/get_subset_chains.md).

## Value

An object of class `mcmc.list` containing one `mcmc` object per chain.

## See also

[`coda::mcmc()`](https://rdrr.io/pkg/coda/man/mcmc.html),
[`coda::mcmc.list()`](https://rdrr.io/pkg/coda/man/mcmc.list.html)
