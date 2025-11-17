# Uniform Non-Decision Time

Uniform Non-Decision Time

## Usage

``` r
nt_uniform(prms_model, prms_solve, t_vec, one_cond, ddm_opts)
```

## Arguments

- prms_model:

  the model parameters; including "non_dec" and "range_non_dec"

- prms_solve:

  solver settings

- t_vec:

  time space

- one_cond:

  one condition

- ddm_opts:

  optional arguments attached to an object

## Value

returns the PDF of a uniform distribution for t_vec, centered around
"non_dec" and with a range of "range_non_dec".
