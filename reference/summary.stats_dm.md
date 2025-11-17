# Summary for `stats_dm` Objects

Summary and corresponding printing methods for objects of the classes
`stats_dm`, `basic_stats`, `cafs`, `quantiles`, `delta_funs`,
`fit_stats`, `sum_dist`, and `stats_dm_list`. These object types result
from a call to
[`calc_stats()`](https://bucky2177.github.io/dRiftDM/reference/calc_stats.md).

## Usage

``` r
# S3 method for class 'stats_dm'
summary(object, ..., round_digits = drift_dm_default_rounding())

# S3 method for class 'basic_stats'
summary(object, ...)

# S3 method for class 'cafs'
summary(object, ...)

# S3 method for class 'quantiles'
summary(object, ...)

# S3 method for class 'delta_funs'
summary(object, ...)

# S3 method for class 'fit_stats'
summary(object, ...)

# S3 method for class 'sum_dist'
summary(object, ...)

# S3 method for class 'stats_dm_list'
summary(object, ...)

# S3 method for class 'summary.stats_dm'
print(x, ..., show_header = TRUE, drop_cols = NULL)

# S3 method for class 'summary.basic_stats'
print(x, ...)

# S3 method for class 'summary.cafs'
print(x, ...)

# S3 method for class 'summary.quantiles'
print(x, ...)

# S3 method for class 'summary.delta_funs'
print(x, ...)

# S3 method for class 'summary.fit_stats'
print(x, ...)

# S3 method for class 'summary.sum_dist'
print(x, ...)

# S3 method for class 'summary.stats_dm_list'
print(x, ...)
```

## Arguments

- object:

  an object of the respective class

- ...:

  additional arguments passed forward.

- round_digits:

  integer, specifying the number of decimal places for rounding the
  summary of the underlying
  [data.frame](https://rdrr.io/r/base/data.frame.html). Default is 3.

- x:

  an object of the respective class.

- show_header:

  logical. If `TRUE`, a header specifying the type of statistic will be
  displayed.

- drop_cols:

  character vector, specifying which columns of the table summarizing
  the underlying [data.frame](https://rdrr.io/r/base/data.frame.html)
  should not be displayed.

## Value

For `summary.*()` methods, a summary object of class corresponding to
the input class.

For `print.*()` methods, the respective object is returned invisibly

## Details

- `summary.stats_dm()`: Summarizes `stats_dm` objects, returning the
  type, a summary of the underlying
  [data.frame](https://rdrr.io/r/base/data.frame.html)
  (`summary_dataframe`), and, if possible, the number of unique IDs
  (`n_ids`).

- `summary.sum_dist()`: Extends `summary.stats_dm()` with additional
  information about the source (`source`).

- `summary.basic_stats()`: Extends `summary.sum_dist()` with additional
  information about the conditions (`conds`).

- `summary.cafs()`: Extends `summary.sum_dist()` with additional
  information about the bins (`bins`) and conditions (`conds`).

- `summary.quantiles()`: Extends `summary.sum_dist()` with additional
  information about the quantile levels (`probs`) and conditions
  (`conds`).

- `summary.delta_funs()`: Extends `summary.sum_dist()` with additional
  information about the quantile levels (`probs`).

- `summary.fit_stats()`: Identical to `summary.stats_dm`.

- `summary.stats_dm_list()`: Applies the summary function to each
  element of the list and returns a list of the respective summary
  objects.

Note the following class relationships and properties:

- `basic_stats`, `cafs`, `quantiles`, and `delta_funs` are all
  inheriting from `sum_dist`.

- All `sum_dist` and `fit_stats` objects are inheriting from `stats_dm`.

- Each `stats_dm_list` object is just a list containing instances of
  `stats_dm`.

## Examples

``` r
# get a model with data for demonstration purpose
a_model <- dmc_dm()
obs_data(a_model) <- dmc_synth_data

# now get some statistics and call the summary functions
some_stats <- calc_stats(a_model, type = c("quantiles", "fit_stats"))
summary(some_stats) # summary.stats_dm_list
#> Summary of Element 1: quantiles
#> 
#> Dependent Variables:
#>    Quant_corr      Quant_err    
#>  Min.   :0.325   Min.   :0.301  
#>  1st Qu.:0.411   1st Qu.:0.361  
#>  Median :0.465   Median :0.431  
#>  Mean   :0.475   Mean   :0.438  
#>  3rd Qu.:0.530   3rd Qu.:0.491  
#>  Max.   :0.672   Max.   :0.698  
#> 
#> Sources: obs, pred 
#> Conditions: comp, incomp 
#> Probs: 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 
#> -------
#> 
#> Summary of Element 2: fit_stats
#> 
#> Dependent Variables:
#>     Log_Like    Neg_Log_Like       AIC            BIC           RMSE_s     
#>  Min.   :125   Min.   :-125   Min.   :-235   Min.   :-205   Min.   :0.083  
#>  1st Qu.:125   1st Qu.:-125   1st Qu.:-235   1st Qu.:-205   1st Qu.:0.083  
#>  Median :125   Median :-125   Median :-235   Median :-205   Median :0.083  
#>  Mean   :125   Mean   :-125   Mean   :-235   Mean   :-205   Mean   :0.083  
#>  3rd Qu.:125   3rd Qu.:-125   3rd Qu.:-235   3rd Qu.:-205   3rd Qu.:0.083  
#>  Max.   :125   Max.   :-125   Max.   :-235   Max.   :-205   Max.   :0.083  
#>     RMSE_ms  
#>  Min.   :83  
#>  1st Qu.:83  
#>  Median :83  
#>  Mean   :83  
#>  3rd Qu.:83  
#>  Max.   :83  
#> 
#> -------
summary(some_stats$quantiles) # summary.quantiles
#> Type of Statistic: quantiles
#> 
#> Dependent Variables:
#>    Quant_corr      Quant_err    
#>  Min.   :0.325   Min.   :0.301  
#>  1st Qu.:0.411   1st Qu.:0.361  
#>  Median :0.465   Median :0.431  
#>  Mean   :0.475   Mean   :0.438  
#>  3rd Qu.:0.530   3rd Qu.:0.491  
#>  Max.   :0.672   Max.   :0.698  
#> 
#> Sources: obs, pred 
#> Conditions: comp, incomp 
#> Probs: 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 
```
