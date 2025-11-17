# Draw Samples Using Inverse Transform Sampling

`draw_from_pdf` generates samples from a given probability density
function (PDF) using inverse transform sampling. This function takes in
a numeric PDF vector and a corresponding domain vector, then returns a
specified number of samples.

## Usage

``` r
draw_from_pdf(a_pdf, x_def, k, seed = NULL, round_to = NULL, method = "discr")
```

## Arguments

- a_pdf:

  a numeric vector representing the PDF values.

- x_def:

  a numeric vector defining the domain (or x-values) corresponding to
  the values in `a_pdf`. The vector `x_def` must be sorted in increasing
  order.

- k:

  a single integer specifying the number of samples to generate.

- seed:

  an optional single integer value used to set the seed for random
  number generation, allowing for reproducibility of results.

- round_to:

  an optional integer, indicating the number of digits to which the
  result should be rounded.

- method:

  a single character string. If "discr", then simulated values match
  `x_def`. If "linear", `x_def` and `a_pdf` are linearly interpolated,
  so that the simulated values can lay in between the discrete values of
  `x_def`.

## Value

A numeric vector of length `k` containing the sampled values from the
specified PDF. If `k` is 0, an empty numeric vector is returned.

## Details

This function implements inverse transform sampling by first
constructing a cumulative distribution function (CDF) from the given
PDF. Then `k` values between zero and one are sampled from a uniform
distribution, and the corresponding values are mapped to `x_def` using
linear interpolation.
