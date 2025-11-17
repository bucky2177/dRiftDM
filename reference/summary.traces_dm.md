# Summary for traces_dm and traces_dm_list Objects

Summary and corresponding printing methods for `traces_dm` and
`traces_dm_list` objects, resulting from a call to
[`simulate_traces()`](https://bucky2177.github.io/dRiftDM/reference/simulate_traces.md).
Here, `traces_dm` objects are entries of the returned list.

## Usage

``` r
# S3 method for class 'traces_dm'
summary(object, ...)

# S3 method for class 'summary.traces_dm'
print(x, ..., round_digits = drift_dm_default_rounding())

# S3 method for class 'traces_dm_list'
summary(object, ...)

# S3 method for class 'summary.traces_dm_list'
print(x, ..., round_digits = drift_dm_default_rounding())
```

## Arguments

- object:

  an object of class `traces_dm` or `traces_dm_list`.

- ...:

  additional arguments passed forward.

- x:

  an object of type `summary.traces_dm` or `summary.traces_dm_list`.

- round_digits:

  integer, specifying the number of decimal places for rounding in the
  printed summary. Default is 3.

## Value

`summary.traces_dm()` returns a list of class `summary.traces_dm` (see
the Details section summarizing each entry of this list).

`summary.traces_dm_list()` returns a list of class
`summary.traces_dm_list` (see the Details section summarizing each entry
of this list).

`print.summary.traces_dm()` returns the `summary.traces_dm` object
invisibly.

`print.summary.traces_dm_list()` returns the `summary.traces_dm_list`
object invisibly.

## Details

The `summary.traces_dm()` function constructs a summary list with
information about the `traces_dm` object, including:

- **k**: The number of traces in the object.

- **add_x**: A logical, indicating whether starting values were added.

- **orig_model_class**: The class label of the original model.

- **orig_prms**: The parameters with which the traces were simulated
  (for the respective condition)

- **prms_solve**: The solver settings with which the traces were
  simulated.

- **fpt_desc**: A summary of the first passage times, including mean,
  standard deviation, and response probabilities for upper and lower
  boundaries.

The `summary.traces_dm_list()` function constructs a summary list with
information about the `traces_dm_list` object, including:

- **k**: A numeric vector, providing the number of traces per condition.

- **add_x**: A logical vector, indicating whether starting values were
  added for each condition.

- **orig_prms**: A matrix, containing the original parameter values per
  condition, with which the traces were simulated.

- **orig_model_class**: The class label of the original model

- **prms_solve**: A matrix of solver settings per condition.

- **fpt_desc**: A summary of the first passage times per condition,
  including mean, standard deviation, and response probabilities for the
  upper or lower boundary.

The `print.summary.traces_dm()` and `print.summary.traces_dm_list()`
functions display the summary in a formatted way.

## Examples

``` r
# get a couple of traces a cross conditions
traces <- simulate_traces(dmc_dm(), k = c(5, 10))
summary(traces)
#> Starting Points Added:
#>   comp incomp 
#>     no     no 
#> 
#> 
#> Number of Traces:
#>   comp incomp 
#>      5     10 
#> 
#> 
#> Summary of First Passage Times:
#>         mean    sd p_corr p_err
#> comp   0.138 0.030      1     0
#> incomp 0.153 0.072      1     0
#> 
#> 
#> Orginal Parameter Values:
#>        muc   b non_dec sd_non_dec  tau a    A alpha
#> comp     4 0.6     0.3       0.02 0.04 2  0.1     4
#> incomp   4 0.6     0.3       0.02 0.04 2 -0.1     4
#> 
#> -------
#> Original Model Class(es): dmc_dm, drift_dm
#> 
#> Settings:
#>        sigma t_max    dt   dx  nt  nx
#> comp       1     3 0.007 0.02 400 100
#> incomp     1     3 0.007 0.02 400 100
#> 

# get a single traces object
one_traces_obj <- traces[[1]]
summary(one_traces_obj)
#> Starting Points Added: no
#> 
#> Number of Traces: 5
#> 
#> Summary of First Passage Times:
#>   mean     sd p_corr  p_err 
#>  0.138  0.030  1.000  0.000 
#> 
#> 
#> Orginal Parameter Values:
#>        muc          b    non_dec sd_non_dec        tau          a          A 
#>       4.00       0.60       0.30       0.02       0.04       2.00       0.10 
#>      alpha 
#>       4.00 
#> 
#> -------
#> Original Model Class(es): dmc_dm, drift_dm
#> Settings: sigma=1, t_max=3, dt=0.007, dx=0.02, nt=400, nx=100
```
