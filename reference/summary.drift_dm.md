# Summary for `drift_dm` objects

summary and corresponding printing methods for objects of class
`drift_dm`, created by a call to
[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md).

## Usage

``` r
# S3 method for class 'drift_dm'
summary(object, ...)

# S3 method for class 'summary.drift_dm'
print(x, ..., round_digits = drift_dm_default_rounding())
```

## Arguments

- object:

  an object of class `drift_dm`.

- ...:

  additional arguments passed forward (currently not used).

- x:

  an object of class `summary.drift_dm`.

- round_digits:

  integer, specifying the number of decimal places for rounding in the
  printed summary. Default is 3.

## Value

`summary.drift_dm()` returns a list of class `summary.drift_dm` (see
details for the entries).

`print.summary.drift_dm()` returns invisibly the `summary.drift_dm`
object.

## Details

`summary.drift_dm()` constructs a summary list with information about
the `drift_dm` object. The returned list has class `summary.drift_dm`
and can include the following entries:

- **class**: Class vector of the `drift_dm` object.

- **summary_flex_prms**: Summary of the
  [flex_prms](https://bucky2177.github.io/dRiftDM/reference/flex_prms.md)
  object in the model (see
  [summary.flex_prms](https://bucky2177.github.io/dRiftDM/reference/summary.flex_prms.md)).

- **prms_solve**: Parameters used for solving the model (see
  [prms_solve](https://bucky2177.github.io/dRiftDM/reference/prms_solve.md)).

- **solver**: Solver used for generating model predictions.

- **b_coding**: Boundary coding for the model (see
  [b_coding](https://bucky2177.github.io/dRiftDM/reference/b_coding.md)).

- **obs_data**: Summary table of observed response time data, if
  available, by response type (upper/lower boundary). rows correspond to
  upper first then lower responses; row names are prefixed by the
  boundary names from `b_coding`. columns (all lower-case) are: `min`,
  `1st qu.`, `median`, `mean`, `3rd qu.`, `max`, and `n`.

- **cost_function**: Name (or descriptor) of the cost function used
  during estimation.

- **fit_stats**: Fit statistics, if available. we return a named atomic
  vector created via
  `unlist(unpack_obj(calc_stats(..., type = "fit_stats")))`.

- **estimate_info**: Additional information about the estimation
  procedure.

`print.summary.drift_dm()` displays this summary in a formatted way.

## Examples

``` r
# get a pre-built model for demonstration
a_model <- dmc_dm()
sum_obj <- summary(a_model)
print(sum_obj, round_digits = 2)
#> Class(es) dmc_dm, drift_dm
#> 
#> Parameter Values:
#>        muc   b non_dec sd_non_dec  tau a    A alpha
#> comp     4 0.6     0.3       0.02 0.04 2  0.1     4
#> incomp   4 0.6     0.3       0.02 0.04 2 -0.1     4
#> 
#> Parameter Settings:
#>        muc b non_dec sd_non_dec tau a A alpha
#> comp   1   2 3       4          5   0 6 7    
#> incomp 1   2 3       4          5   0 d 7    
#> 
#> Special Dependencies:
#> A ~ incomp == -(A ~ comp)
#> 
#> Custom Parameters:
#>        peak_l
#> comp     0.04
#> incomp   0.04
#> 
#> Observed Data:
#> NULL
#> 
#> Fit Indices:
#>     Log_Like Neg_Log_Like          AIC          BIC       RMSE_s      RMSE_ms 
#>           NA           NA           NA           NA           NA           NA 
#> 
#> -------
#> Deriving PDFS:
#>   solver: kfe
#>   values: sigma=1, t_max=3, dt=0.0075, dx=0.02, nt=400, nx=100
#> 
#> Boundary Coding:
#>   upper: corr 
#>   lower: err 
#>   expected data column: Error (corr = 0; err = 1) 

# more information is provided when we add data to the model
obs_data(a_model) <- dmc_synth_data # (data set comes with dRiftDM)
summary(a_model)
#> Class(es) dmc_dm, drift_dm
#> 
#> Parameter Values:
#>        muc   b non_dec sd_non_dec  tau a    A alpha
#> comp     4 0.6     0.3       0.02 0.04 2  0.1     4
#> incomp   4 0.6     0.3       0.02 0.04 2 -0.1     4
#> 
#> Parameter Settings:
#>        muc b non_dec sd_non_dec tau a A alpha
#> comp   1   2 3       4          5   0 6 7    
#> incomp 1   2 3       4          5   0 d 7    
#> 
#> Special Dependencies:
#> A ~ incomp == -(A ~ comp)
#> 
#> Custom Parameters:
#>        peak_l
#> comp     0.04
#> incomp   0.04
#> 
#> Observed Data:
#>              min. 1st qu. median  mean 3rd qu.  max.   n
#> corr comp   0.331   0.436  0.479 0.507   0.549 1.075 292
#> corr incomp 0.313   0.474  0.528 0.543   0.592 0.879 268
#> err comp    0.428   0.458  0.526 0.564   0.621 0.871   8
#> err incomp  0.302   0.398  0.452 0.458   0.498 0.771  32
#> 
#> Fit Indices:
#>     Log_Like Neg_Log_Like          AIC          BIC       RMSE_s      RMSE_ms 
#>      124.641     -124.641     -235.283     -204.504        0.083       83.002 
#> 
#> -------
#> Deriving PDFS:
#>   solver: kfe
#>   values: sigma=1, t_max=3, dt=0.0075, dx=0.02, nt=400, nx=100
#> 
#> Boundary Coding:
#>   upper: corr 
#>   lower: err 
#>   expected data column: Error (corr = 0; err = 1) 

# fit indices are added once we evaluate the model
a_model <- re_evaluate_model(a_model)
summary(a_model)
#> Class(es) dmc_dm, drift_dm
#> 
#> Parameter Values:
#>        muc   b non_dec sd_non_dec  tau a    A alpha
#> comp     4 0.6     0.3       0.02 0.04 2  0.1     4
#> incomp   4 0.6     0.3       0.02 0.04 2 -0.1     4
#> 
#> Parameter Settings:
#>        muc b non_dec sd_non_dec tau a A alpha
#> comp   1   2 3       4          5   0 6 7    
#> incomp 1   2 3       4          5   0 d 7    
#> 
#> Special Dependencies:
#> A ~ incomp == -(A ~ comp)
#> 
#> Custom Parameters:
#>        peak_l
#> comp     0.04
#> incomp   0.04
#> 
#> Observed Data:
#>              min. 1st qu. median  mean 3rd qu.  max.   n
#> corr comp   0.331   0.436  0.479 0.507   0.549 1.075 292
#> corr incomp 0.313   0.474  0.528 0.543   0.592 0.879 268
#> err comp    0.428   0.458  0.526 0.564   0.621 0.871   8
#> err incomp  0.302   0.398  0.452 0.458   0.498 0.771  32
#> 
#> Fit Indices:
#>     Log_Like Neg_Log_Like          AIC          BIC       RMSE_s      RMSE_ms 
#>      124.641     -124.641     -235.283     -204.504        0.083       83.002 
#> 
#> -------
#> Deriving PDFS:
#>   solver: kfe
#>   values: sigma=1, t_max=3, dt=0.0075, dx=0.02, nt=400, nx=100
#> 
#> Boundary Coding:
#>   upper: corr 
#>   lower: err 
#>   expected data column: Error (corr = 0; err = 1) 
```
