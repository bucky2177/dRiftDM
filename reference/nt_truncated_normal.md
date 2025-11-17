# Truncated Normally-Distributed Non-Decision Time

Truncated Normally-Distributed Non-Decision Time

## Usage

``` r
nt_truncated_normal(prms_model, prms_solve, t_vec, one_cond, ddm_opts)
```

## Arguments

- prms_model:

  the model parameters; including "non_dec" and "sd_non_dec"

- prms_solve:

  solver settings

- t_vec:

  time space

- one_cond:

  one condition

- ddm_opts:

  optional arguments attached to an object

## Value

returns the PDF of a truncated normal distribution for t_vec, with mean
"non_dec" and standard deviation "sd_non_dec". Lower truncation is 0.
Upper truncation is max(t_vec)
