# Check time/space discretization via reference comparison

`check_discretization()` helps you choose or check time (`dt`) and space
(`dx`) discretization settings. It computes a high-precision *reference*
solution of the model's PDFs with `dt_ref`/`dx_ref`, and then compares
the reference PDFs to the discretization settings of the supplied
object, using the Hellinger distance per condition. Smaller distances
indicate closer agreement with the reference — i.e., a sufficiently fine
grid.

There are not yet overall and officially published recommendations on
how large the Hellinger distance can be without affecting model
precision, and this might even depend on the model itself. Based on some
preliminary simulations we would recommend trying to keep the Hellinger
Distance at best below 10 percent. However, it is best to iterate
between plotting model predictions and calculating the Hellinger
Distance, to ensure that you can best interpret this quantity for your
model at hand.

## Usage

``` r
check_discretization(object, ...)

# S3 method for class 'drift_dm'
check_discretization(
  object,
  ...,
  dt_ref = 0.001,
  dx_ref = 0.001,
  round_digits = 5
)

# S3 method for class 'fits_ids_dm'
check_discretization(object, ...)

# S3 method for class 'fits_agg_dm'
check_discretization(object, ...)
```

## Arguments

- object:

  a
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md),
  `fits_agg_dm`, or `fits_ids_dm` object. (the latter two are returned
  by
  [`estimate_dm()`](https://bucky2177.github.io/dRiftDM/reference/estimate_dm.md))

- ...:

  further arguments passed forward to the respective method.

- dt_ref, dx_ref:

  numeric scalars, providing a fine time or space step size for the
  reference solution. Defaults to `0.001`.

- round_digits:

  number of decimal places to which the final Hellinger distances are
  rounded (default: `5`).

## Value

a named numeric vector of Hellinger distances (one per condition) if
`object` is of type
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md) or
`fits_agg_dm`. A [data.frame](https://rdrr.io/r/base/data.frame.html) of
Hellinger distances across IDs and conditions if `object` is of type
`fits_ids_dm`. Hellinger distances are in `[0, 1]`, where `0` means
identical to the reference.

## Details

Under the hood, for each condition, we concatenate the lower- and upper-
boundary PDFs (`pdf_l`, `pdf_u`), interpolate the model PDFs to a time
space matching with the reference PDFs, and then compute the Hellinger
distance: \\H(p,q) = \sqrt{1 - \int \sqrt{p(t)\\q(t)}\\dt}\\

There are not yet overall, officially published recommendations on how
large the Hellinger distance can be without affecting model precision,
and this may even depend on the specific model. Based on preliminary
simulations, we recommend trying to keep the average Hellinger distance
below 5\\

The reference discretizations (`dt_ref/dx_ref`) must be at least as fine
as the object's current discretization settings (`dt_model/dx_model`).
If `dt_model < dt_ref` or `dx_model < dx_ref`, an error is raised
because the “reference” would not be the finest solution.

## See also

[`estimate_dm()`](https://bucky2177.github.io/dRiftDM/reference/estimate_dm.md),
[`trapz()`](https://bucky2177.github.io/dRiftDM/reference/trapz.md)

## Examples

``` r
# Example:
my_model <- ratcliff_dm()

# Assess current (dt=0.0075, dx=0.02) against a fine reference:
check_discretization(my_model)
#>   null 
#> 0.0165 

# If distances are near zero across conditions, the current grid is adequate.
```
