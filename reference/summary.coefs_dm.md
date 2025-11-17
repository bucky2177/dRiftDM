# Summary for `coefs_dm` Objects

Summary and corresponding printing methods for `coefs_dm` objects. These
objects result from a call to
[`coef.fits_ids_dm()`](https://bucky2177.github.io/dRiftDM/reference/coef.drift_dm.md)
(i.e., when calling [`coef()`](https://rdrr.io/r/stats/coef.html) with
an object of type `fits_ids_dm`).

## Usage

``` r
# S3 method for class 'coefs_dm'
summary(object, ..., round_digits = drift_dm_default_rounding())

# S3 method for class 'summary.coefs_dm'
print(x, ..., show_header = TRUE)
```

## Arguments

- object:

  an object of type `coefs_dm`.

- ...:

  additional arguments passed forward.

- round_digits:

  integer, specifying the number of decimal places for rounding the
  summary of the underlying
  [data.frame](https://rdrr.io/r/base/data.frame.html). Default is 3.

- x:

  an object of class `summary.coefs_dm`.

- show_header:

  logical. If `TRUE`, a header specifying the type of statistic will be
  displayed.

## Value

For `summary.coefs_dm()` a summary object of class `summary.coefs_dm`.

For `print.summary.coefs_dm()`, the supplied object is returned
invisibly.

## Details

`summary.coefs_dm()` summarizes `coefs_dm` objects, returning the type,
a summary of the underlying
[data.frame](https://rdrr.io/r/base/data.frame.html)
(`summary_dataframe`), and the number of unique IDs (`n_ids`).

## Examples

``` r
# get a fits_ids object for demonstration purpose
fits_ids <- get_example_fits("fits_ids_dm")
coefs <- coef(fits_ids)
summary(coefs)
#> Object Type: coefs_dm
#> 
#> Parameters:
#>       muc             b            non_dec        sd_non_dec    
#>  Min.   :4.17   Min.   :0.387   Min.   :0.292   Min.   :0.0142  
#>  1st Qu.:4.36   1st Qu.:0.416   1st Qu.:0.306   1st Qu.:0.0232  
#>  Median :4.55   Median :0.446   Median :0.319   Median :0.0322  
#>  Mean   :4.79   Mean   :0.472   Mean   :0.317   Mean   :0.0289  
#>  3rd Qu.:5.10   3rd Qu.:0.515   3rd Qu.:0.330   3rd Qu.:0.0363  
#>  Max.   :5.65   Max.   :0.585   Max.   :0.341   Max.   :0.0403  
#>       tau               A              alpha     
#>  Min.   :0.0350   Min.   :0.0785   Min.   :3.84  
#>  1st Qu.:0.0509   1st Qu.:0.0906   1st Qu.:5.61  
#>  Median :0.0668   Median :0.1026   Median :7.39  
#>  Mean   :0.0676   Mean   :0.1202   Mean   :6.32  
#>  3rd Qu.:0.0839   3rd Qu.:0.1411   3rd Qu.:7.56  
#>  Max.   :0.1010   Max.   :0.1795   Max.   :7.74  
#> 
#> N IDs: 3 
```
