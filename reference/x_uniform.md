# Uniform Starting Point Distribution Centered Around Zero

Uniform Starting Point Distribution Centered Around Zero

## Usage

``` r
x_uniform(prms_model, prms_solve, x_vec, one_cond, ddm_opts)
```

## Arguments

- prms_model:

  the model parameters; prm name "range_start" required

- prms_solve:

  solver settings

- x_vec:

  evidence space

- one_cond:

  one condition

- ddm_opts:

  optional arguments attached to an object

## Value

returns the PDF of a uniform distribution for x_vec, centered around
zero and with a range of "range_start".
