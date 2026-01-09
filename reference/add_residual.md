# Convolute the First Passage Times with the Non-Decision Time Distribution

Calls [stats::convolve](https://rdrr.io/r/stats/convolve.html) for the
first passage times and the non-decision time distribution to derive the
full distribution of response times. After convolution, I add the
robustness parameter.

## Usage

``` r
add_residual(pdf_nt, pdf_u, pdf_l, dt, nt)
```

## Arguments

- pdf_nt:

  the non-decision time density values

- pdf_u, pdf_l:

  the first passage times

- dt, nt:

  step size and number of steps for the time space (for input checks and
  scaling)

## Value

a list of PDFs for one condition "pdf_u" and "pdf_l"
