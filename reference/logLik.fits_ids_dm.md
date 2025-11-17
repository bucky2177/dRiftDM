# Extract Model Statistics for fits_ids_dm Object

These methods are wrappers to extract specific model fit statistics
(log-likelihood, AIC, BIC) for each model in a `fits_ids_dm` object.

## Usage

``` r
# S3 method for class 'fits_ids_dm'
logLik(object, ...)

# S3 method for class 'fits_ids_dm'
AIC(object, ..., k = 2)

# S3 method for class 'fits_ids_dm'
BIC(object, ...)
```

## Arguments

- object:

  a `fits_ids_dm` object (see
  [estimate_model_ids](https://bucky2177.github.io/dRiftDM/reference/estimate_model_ids.md))

- ...:

  additional arguments (currently not used)

- k:

  numeric; penalty parameter for the AIC calculation. Defaults to 2
  (standard AIC).

## Value

An object of type `fit_stats` containing the respective statistic in one
column (named `Log_Like`, `AIC`, or `BIC`) and a corresponding `ID`
column. If any of the statistics can't be calculated, the function
returns `NULL`.

## Details

Each function retrieves the relevant statistics by calling
[calc_stats](https://bucky2177.github.io/dRiftDM/reference/calc_stats.md)
with `type = "fit_stats"` and selects the columns for `ID` and the
required statistic.

## See also

[`stats::AIC()`](https://rdrr.io/r/stats/AIC.html),
[`stats::BIC()`](https://rdrr.io/r/stats/AIC.html),
[logLik.drift_dm](https://bucky2177.github.io/dRiftDM/reference/logLik.drift_dm.md)

## Examples

``` r
# get an auxiliary fits_ids object for demonstration purpose;
# such an object results from calling load_fits_ids
all_fits <- get_example_fits("fits_ids_dm")

# AICs
AIC(all_fits)
#> Type of Statistic: fit_stats
#> 
#>   ID      AIC
#> 1  1 -784.163
#> 2  2 -737.576
#> 3  3 -931.294
#> 
#> (access the data.frame's columns/rows as usual)

# BICs
BIC(all_fits)
#> Type of Statistic: fit_stats
#> 
#>   ID      BIC
#> 1  1 -757.443
#> 2  2 -710.898
#> 3  3 -904.574
#> 
#> (access the data.frame's columns/rows as usual)

# Log-Likelihoods
logLik(all_fits)
#> Type of Statistic: fit_stats
#> 
#>   ID Log_Like
#> 1  1  399.081
#> 2  2  375.788
#> 3  3  472.647
#> 
#> (access the data.frame's columns/rows as usual)

# All unique and free parameters
coef(all_fits)
#> Object Type: coefs_dm
#> 
#>   ID   muc     b non_dec sd_non_dec   tau     A alpha
#> 1  1 4.551 0.446   0.341      0.032 0.035 0.103 7.386
#> 2  2 4.174 0.387   0.292      0.040 0.067 0.079 7.736
#> 3  3 5.652 0.585   0.319      0.014 0.101 0.180 3.840
#> 
#> (access the data.frame's columns/rows as usual)

# Or all parameters across all conditions
coef(all_fits, select_unique = FALSE)
#> Object Type: coefs_dm
#> 
#>   ID   Cond   muc     b non_dec sd_non_dec   tau a      A alpha peak_l
#> 1  1   comp 4.551 0.446   0.341      0.032 0.035 2  0.103 7.386  0.035
#> 2  1 incomp 4.551 0.446   0.341      0.032 0.035 2 -0.103 7.386  0.035
#> 3  2   comp 4.174 0.387   0.292      0.040 0.067 2  0.079 7.736  0.067
#> 4  2 incomp 4.174 0.387   0.292      0.040 0.067 2 -0.079 7.736  0.067
#> 5  3   comp 5.652 0.585   0.319      0.014 0.101 2  0.180 3.840  0.101
#> 6  3 incomp 5.652 0.585   0.319      0.014 0.101 2 -0.180 3.840  0.101
#> 
#> (access the data.frame's columns/rows as usual)
```
