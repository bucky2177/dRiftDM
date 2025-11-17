# Default Prior for Group-Level (Hyper) Parameters

These functions define and evaluate a default prior distribution for
hyperparameters at the group level.

## Usage

``` r
d_default_prior_hyper(x, mean, sd, lower, upper, shape, rate, log)

r_default_prior_hyper(n, mean, sd, lower, upper, shape, rate)
```

## Arguments

- x:

  a numeric vector of length 2 or a matrix with 2 rows and N column.
  Here, `x[1]` or `x[1,]` are interpreted as the group mean(s) and
  `x[2]` or `x[2,]` as the group standard deviation(s).

- mean, sd:

  mean and standard deviation of the truncated normal distribution for
  the group-level mean. (recycled if necessary)

- lower, upper:

  lower and upper bounds for the truncated normal distribution.
  (recycled if necessary)

- shape, rate:

  shape and rate parameters of the gamma distribution for the
  group-level standard deviation. (recycled if necessary)

- log:

  logical; if `TRUE`, the log-density is returned.

- n:

  number of samples to generate.

## Value

For `d_default_prior_hyper`, a numeric vector representing the (log)
prior density value(s), with the simplifying assumption of independence
of the mean and standard deviation.

For `r_default_prior_hyper`, a 2-row matrix with `n` columns. The first
row contains sampled group means; the second row contains sampled
standard deviations. Samples are drawn independently. If `n` is 1, then
a named numeric vector is returned.

## Details

`d_default_prior_hyper` computes the (log) density of a prior for a
two-element vector or a 2xN matrix, containing the mean and standard
deviation (i.e., `phi_j`). The mean is modeled with a truncated normal
distribution, and the standard deviation with a gamma distribution.

`r_default_prior_hyper` samples hyperparameter values from this prior.

the arguments `mean`, `sd`, `lower`, `upper`, `shape`, and `rate` are
recycled if necessary with respect to the columns of `x`. For example,
if `x` has two columns, then `mean` might provide two values.
