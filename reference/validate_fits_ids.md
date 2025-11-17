# Validate a an Object of Type fits_ids_dm

checks if all the information are in the fits_ids_dm (see
[estimate_model_ids](https://bucky2177.github.io/dRiftDM/reference/estimate_model_ids.md))
object and ensures that nothing obviously fishy is going on with the
individual model fits.

## Usage

``` r
validate_fits_ids(fits_ids, progress)
```

## Arguments

- fits_ids:

  an object of type fits_ids_dm

- progress:

  numeric, 0, no progress, 1 basic output, 2 progress bars

## Value

the unmodified fits_ids objects after passing all tests.

## Details

Checks:

- if all names are in the info file

- and if the respective entries make sense

- if the flex_prms object of the all saved models and the overall model
  is the same (except for the differences in the prm values).

- if the class, prms_solve and solver of the saved models and the
  overall model is the same

- if the estimated parameters are in the parameter space

- for same b_coding and same functions

- if the number of fits matches with the number of individuals in the
  info file

- if the data in each fitted model matches with the observed data in the
  info file
