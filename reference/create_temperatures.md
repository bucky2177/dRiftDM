# Create "Temperatures" for TIDE

Create "Temperatures" for TIDE

## Usage

``` r
create_temperatures(n_chains, sampler)
```

## Arguments

- n_chains:

  numeric

- sampler:

  "TIDE" or anything else

## Value

a numeric vector of length equal to `n_chains`. The returned values
correspond to quantiles of a Beta(0.3, 1) distribution for
`sampler == "TIDE"`. Otherwise a numeric vector of `1`s is returned.
