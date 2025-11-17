# Access the Probability Density Functions of a Model

Functions to obtain the probability density functions (PDFs) of a model.
These PDFs represent the convolution of the first-passage-time (decision
time) with the non-decision time.

## Usage

``` r
pdfs(object, ...)

# S3 method for class 'drift_dm'
pdfs(object, ...)

# S3 method for class 'fits_agg_dm'
pdfs(object, ...)
```

## Arguments

- object:

  an object of type
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
  or `fits_agg_dm` (see
  [`estimate_dm()`](https://bucky2177.github.io/dRiftDM/reference/estimate_dm.md)).

- ...:

  additional arguments passed down to the specific method.

## Value

A list with the entries:

- `pdfs`, contains another named list with entries corresponding to the
  conditions of the model (see
  [`conds()`](https://bucky2177.github.io/dRiftDM/reference/conds.md)).
  Each of these elements is another named list, containing the entries
  `pdf_u` and `pdf_l`, which are numeric vectors for the PDFs of the
  upper and lower boundary, respectively.

- `t_vec`, containing a numeric vector of the time domain.

## Details

If the model has not been evaluated,
[`re_evaluate_model()`](https://bucky2177.github.io/dRiftDM/reference/re_evaluate_model.md)
is called before returning the PDFs.

## See also

[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md),
[`re_evaluate_model()`](https://bucky2177.github.io/dRiftDM/reference/re_evaluate_model.md),
[`conds()`](https://bucky2177.github.io/dRiftDM/reference/conds.md)

## Examples

``` r
# get a pre-built model for demonstration purpose
a_model <- dmc_dm()
str(pdfs(a_model))
#> List of 2
#>  $ pdfs :List of 2
#>   ..$ comp  :List of 2
#>   .. ..$ pdf_u: num [1:401] 1e-08 1e-08 1e-08 1e-08 1e-08 ...
#>   .. ..$ pdf_l: num [1:401] 1e-08 1e-08 1e-08 1e-08 1e-08 ...
#>   ..$ incomp:List of 2
#>   .. ..$ pdf_u: num [1:401] 1e-08 1e-08 1e-08 1e-08 1e-08 ...
#>   .. ..$ pdf_l: num [1:401] 1e-08 1e-08 1e-08 1e-08 1e-08 ...
#>  $ t_vec: num [1:401] 0 0.0075 0.015 0.0225 0.03 0.0375 0.045 0.0525 0.06 0.0675 ...
```
