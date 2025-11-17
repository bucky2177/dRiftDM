# A synthetic data set with two conditions

This dataset was simulated by using the Diffusion Model for Conflict
tasks (see
[`dmc_dm()`](https://bucky2177.github.io/dRiftDM/reference/dmc_dm.md))
with parameter settings that are typical for a Simon task.

## Usage

``` r
dmc_synth_data
```

## Format

A data frame with 600 rows and 3 columns:

- RT:

  Response Times

- Error:

  Error Coding (Error Response = 1; Correct Response = 0)

- Cond:

  Condition ('comp' and 'incomp')
