# Calculate Statistics

`calc_stats` provides an interface for calculating statistics/metrics on
model predictions and/or observed data. Supported statistics include
basic statistics on mean and standard deviation, Conditional Accuracy
Functions (CAFs), Quantiles, Delta Functions, and fit statistics.
Results can be aggregated across individuals.

## Usage

``` r
calc_stats(object, type, ...)

# S3 method for class 'data.frame'
calc_stats(
  object,
  type,
  ...,
  conds = NULL,
  resample = FALSE,
  progress = 1,
  level = "individual",
  b_coding = NULL
)

# S3 method for class 'drift_dm'
calc_stats(object, type, ..., conds = NULL, resample = FALSE)

# S3 method for class 'fits_ids_dm'
calc_stats(
  object,
  type,
  ...,
  conds = NULL,
  resample = FALSE,
  progress = 1,
  level = "individual"
)

# S3 method for class 'fits_agg_dm'
calc_stats(
  object,
  type,
  ...,
  conds = NULL,
  resample = FALSE,
  progress = 1,
  level = "group",
  messaging = TRUE
)

# S3 method for class 'stats_dm'
print(
  x,
  ...,
  round_digits = NULL,
  print_rows = NULL,
  some = NULL,
  show_header = NULL,
  show_note = NULL
)

# S3 method for class 'stats_dm_list'
print(x, ...)
```

## Arguments

- object:

  an object for which statistics are calculated. This can be a
  [data.frame](https://rdrr.io/r/base/data.frame.html) of observed data,
  a
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
  object, a `fits_ids_dm` object, or a `fits_agg_dm` object (see
  [`estimate_dm()`](https://bucky2177.github.io/dRiftDM/reference/estimate_dm.md)).

- type:

  a character vector, specifying the statistics to calculate. Supported
  values include `"basic_stats"`, `"cafs"`, `"quantiles"`,
  `"delta_funs"`, `"densities"`, and `"fit_stats"`.

- ...:

  additional arguments passed to the respective method and the
  underlying calculation functions (see Details for mandatory
  arguments).

- conds:

  optional character vector specifying conditions to include. Conditions
  must match those found in the `object`.

- resample:

  logical. If `TRUE`, then data is (re-)sampled to create an uncertainty
  estimate for the requested summary statistic. See Details for more
  information. Default is `FALSE`. Note that resampling does not work
  with `type = "fit_stats"`.

- progress:

  integer, indicating if information about the progress should be
  displayed. 0 -\> no information, 1 -\> a progress bar. Default is 1.

- level:

  a single character string, indicating at which "level" the statistic
  should be calculated. Options are `"group"` or `"individual"`. If
  `"individual"`, the returned `stats_dm` object contains an `"ID"`
  column.

- b_coding:

  a list for boundary coding (see
  [b_coding](https://bucky2177.github.io/dRiftDM/reference/b_coding.md)).
  Only relevant when `object` is a
  [data.frame](https://rdrr.io/r/base/data.frame.html). For other
  `object` types, the `b_coding` of the `object` is used.

- messaging:

  logical, if `FALSE`, no message is provided.

- x:

  an object of type `stats_dm` or `stats_dm_list`, as returned by the
  function `calc_stats()`.

- round_digits:

  integer, controls the number of digits shown. Default is 3.

- print_rows:

  integer, controls the number of rows shown.

- some:

  logical. If `TRUE`, a subset of randomly sampled rows is shown.

- show_header:

  logical. If `TRUE`, a header specifying the type of statistic will be
  displayed.

- show_note:

  logical. If `TRUE`, a footnote is displayed indicating that the
  underlying [data.frame](https://rdrr.io/r/base/data.frame.html) can be
  accessed as usual.

## Value

If `type` is a single character string, then a subclass of
[data.frame](https://rdrr.io/r/base/data.frame.html) is returned,
containing the respective statistic. Objects of type `sum_dist` will
have an additional attribute storing the boundary encoding (see also
[b_coding](https://bucky2177.github.io/dRiftDM/reference/b_coding.md)).
The reason for returning subclasses of
[data.frame](https://rdrr.io/r/base/data.frame.html) is to provide
custom [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods
(e.g.,
[plot.cafs](https://bucky2177.github.io/dRiftDM/reference/plot.cafs.md)).
To get rid of the subclass label and additional attributes (i.e., to get
just the plain underlying
[data.frame](https://rdrr.io/r/base/data.frame.html), users can use
[`unpack_obj()`](https://bucky2177.github.io/dRiftDM/reference/unpack_obj.md)).

If `type` contains multiple character strings (i.e., is a character
vector) a subclass of [list](https://rdrr.io/r/base/list.html) with the
calculated statistics is returned. The list will be of type
`stats_dm_list` (to easily create multiple panels using the respective
[`plot.stats_dm_list()`](https://bucky2177.github.io/dRiftDM/reference/plot.stats_dm_list.md)
method).

The print methods `print.stats_dm()` and `print.stats_dm_list()` each
invisibly return the supplied object `x`.

## Details

`calc_stats` is a generic function to handle the calculation of
different statistics/metrics for the supported object types. Per
default, it returns the requested statistics/metrics.

### List of Supported Statistics

**Basic Statistics**

With "basic statistics", we refer to a summary of the mean and standard
deviation of response times, including a proportion of response choices.

**Conditional Accuracy Function (CAFs)**

CAFs are a way to quantify response accuracy against speed. To calculate
CAFs, RTs (whether correct or incorrect) are first binned and then the
percent correct responses per bin is calculated.

When calculating model-based CAFs, a joint CDF combining both the pdf of
correct and incorrect responses is calculated. Afterwards, this CDF is
separated into even-spaced segments and the contribution of the pdf
associated with a correct response relative to the joint CDF is
calculated.

The number of bins can be controlled by passing the argument `n_bins`.
The default is 5.

**Quantiles**

For observed response times, the function
[stats::quantile](https://rdrr.io/r/stats/quantile.html) is used with
default settings.

Which quantiles are calcuated can be controlled by providing the
probabilites, `probs`, with values in \\\[0, 1\]\\. Default is
`seq(0.1, 0.9, 0.1)`.

**Delta Functions**

Delta functions calculate the difference between quantiles of two
conditions against their mean:

- \\Delta_i = Q\_{i,j} - Q\_{i,k}\\

- \\Avg_i = 0.5 \cdot Q\_{i,j} + 0.5 \cdot Q\_{i,k}\\

With i indicating a quantile, and j and k two conditions.

To calculate delta functions, users have to specify:

- `minuends`: character vector, specifying condition(s) j. Must be in
  `conds(drift_dm_obj)`.

- `subtrahends`: character vector, specifying condition(s) k. Must be in
  `conds(drift_dm_obj)`

- `dvs`: character, indicating which quantile columns to use. Default is
  "Quant\_\<u_label\>". If multiple dvs are provided, then minuends and
  subtrahends must have the same length, and matching occurs pairwise.
  In this case, if only one minuend/subtrahend is specified, minuend and
  subtrahend are recycled to the necessary length.

- specifying `probs` is possible (see Quantiles)

**Densities**

With "densities", we refer to a summary of the distribution of observed
or predicted data. For observed data, histogram values and kernel
density estimates are provided. For predicted data, the model's
predicted PDFs are provided.

Optional arguments are:

- `discr`: numeric, the band-width when calculating the histogram or the
  kernel density estimates. Defaults to `0.015` seconds

- `t_max`: numeric, the maximum time window when calculating the
  distribution summaries of observe data. Defaults to the longest RT
  (for observed data) or the maximum of the time domain of a model
  (which is the preferred choice, if possible). If necessary, `t_max` is
  slightly adjusted to match with `discr`.

- `scale_mass`: logical, only relevant if observed data is available. If
  `TRUE`, density masses are scaled proportional to the number of trials
  per condition.

**Fit Statistics**

Calculates the Log-Likelihood, Akaike and Bayesian Information Criteria,
and root-mean squared-error statistic.

Optional arguments are:

- `k`: numeric, for penalizing the AIC statistic (see also
  [stats::AIC](https://rdrr.io/r/stats/AIC.html) and
  [AIC.fits_ids_dm](https://bucky2177.github.io/dRiftDM/reference/logLik.fits_ids_dm.md)).

- `n_bins`, `probs`: numeric vectors, see the section on CAFs and
  Quantiles above

- `weight_err`: numeric scalar, determines how CAFs and quantiles are
  weighted. Default is `1.5`.

### Resampling

When `resampling = TRUE`, an uncertainty interval is provided via
simulation. The default number of iterations is `R = 100`, which can be
changed by passing the optional argument `R`.

If resampling is requested, the returned `stats_dm` object contains the
column `"Estimate"`, coding the interval. The interval width is
controlled via the optional argument `interval_level`, a single numeric
value between 0 and 1 (default: `0.95`). The interpretation of this
interval depends on the specific situation (see below).

**Resampling at the Individual Level**

If `object` is a `drift_dm` object (i.e., a single model created by
[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)),
synthetic data are simulated under the model, and for each synthetic
data set the requested statistic is calculated. The interval then
reflects the range of these simulated statistics. To determine the
number of trials for each synthetic data set, dRiftDM either uses the
observed data attached to the model (if available) or the optional
argument `n_sim` (passed to
[`simulate_data()`](https://bucky2177.github.io/dRiftDM/reference/simulate_data.md)).
Note that `n_sim` must be provided if no observed data are available,
and that `n_sim` always has priority.

If `object` is a `drift_dm` object with attached observed data,
resampling is also performed for the observed data. In this case, trials
are bootstrapped, and for each bootstrap sample the requested statistic
is calculated.

If `object` is a `data.frame`, `fits_agg_dm`, or `fits_ids_dm` object,
resampling is performed for each individual if `level = "individual"`.
For both models and observed data, synthetic or bootstrapped data sets
are generated as described above.

**Resampling at the Group Level**

Group-level resampling is possible only if `object` is a `data.frame`
(with an `"ID"` column), `fits_agg_dm`, or `fits_ids_dm` object. To
request this, set `level = "group"`. Participants are then bootstrapped,
and for each bootstrapped sample the aggregated statistic is calculated.

**Interpretation of Intervals**

For `level = "group"`, intervals represent bootstrapped confidence
intervals For `level = "individual"`, intervals represent the
variability in the statistic when data for a single participant are
resampled or simulated under the model.

**Note**

For objects of type `fits_agg_dm`, which contain a mixture of group- and
individual-level information, the `level` argument only affects
resampling for the observed data. For the model itself, resampling is
always performed under the fitted model, in the same way as for a
`drift_dm` object.

## Note

When a model's predicted density function integrates to a value of less
than
[`drift_dm_skip_if_contr_low()`](https://bucky2177.github.io/dRiftDM/reference/defaults.md),
means and quantiles return the values `NA`. Users can alter this by
explicitly passing the argument `skip_if_contr_low` when calling
`calc_stats()` (e.g., `calc_stats(..., skip_if_contr_low = -Inf)`)

## Examples

``` r
# Example 1: Calculate CAFs and Quantiles from a model ---------------------
# get a model for demonstration purpose
a_model <- ssp_dm()
# and then calculate cafs and quantiles
some_stats <- calc_stats(a_model, type = c("cafs", "quantiles"))
print(some_stats)
#> Element 1, contains cafs
#> 
#>    Source   Cond Bin P_corr
#> 1    pred   comp   1  0.981
#> 2    pred   comp   2  0.981
#> 3    pred   comp   3  0.981
#> 4    pred   comp   4  0.981
#> 5    pred   comp   5  0.981
#> 6    pred incomp   1  0.675
#> 7    pred incomp   2  0.925
#> 8    pred incomp   3  0.960
#> 9    pred incomp   4  0.973
#> 10   pred incomp   5  0.979
#> 
#> 
#> Element 2, contains quantiles
#> 
#>    Source   Cond Prob Quant_corr Quant_err
#> 1    pred   comp  0.1      0.362     0.362
#> 2    pred   comp  0.2      0.383     0.383
#> 3    pred   comp  0.3      0.403     0.403
#> 4    pred   comp  0.4      0.422     0.422
#> 5    pred   comp  0.5      0.445     0.444
#> 6    pred   comp  0.6      0.471     0.470
#> 7    pred   comp  0.7      0.503     0.503
#> 8    pred   comp  0.8      0.549     0.549
#> 9    pred   comp  0.9      0.626     0.626
#> 10   pred incomp  0.1      0.407     0.344
#> ...
#> 
#> (extract the list's elements as usual, e.g., with $cafs)

# Example 2: Calculate a Delta Function from a data.frame ------------------
# get a data set for demonstration purpose
some_data <- ulrich_simon_data
conds(some_data) # relevant for minuends and subtrahends
#> [1] "incomp" "comp"  
some_stats <- calc_stats(
  a_model,
  type = "delta_funs",
  minuends = "incomp",
  subtrahends = "comp"
)
print(some_stats, print_rows = 5)
#> Type of Statistic: delta_funs
#> 
#>   Source Prob Quant_corr_comp Quant_corr_incomp Delta_incomp_comp
#> 1   pred  0.1           0.362             0.407             0.045
#> 2   pred  0.2           0.383             0.437             0.053
#> 3   pred  0.3           0.403             0.462             0.060
#> 4   pred  0.4           0.422             0.487             0.065
#> 5   pred  0.5           0.445             0.514             0.069
#>   Avg_incomp_comp
#> 1           0.385
#> 2           0.410
#> 3           0.432
#> 4           0.455
#> 5           0.479
#> ...
#> 
#> (access the data.frame's columns/rows as usual)


# Example 3: Calculate Quantiles from a fits_ids_dm object -----------------
# get an auxiliary fits_ids_dm object
all_fits <- get_example_fits("fits_ids_dm")
some_stats <- calc_stats(all_fits, type = "quantiles")
print(some_stats, print_rows = 5) # note the ID column
#> Type of Statistic: quantiles
#> 
#>   ID Source Cond Prob Quant_corr Quant_err
#> 1  1    obs comp  0.1      0.335     0.361
#> 2  1    obs comp  0.2      0.368     0.388
#> 3  1    obs comp  0.3      0.385     0.415
#> 4  1    obs comp  0.4      0.385     0.441
#> 5  1    obs comp  0.5      0.401     0.468
#> ...
#> 
#> (access the data.frame's columns/rows as usual)

# one can also request that the statistics are averaged across individuals
print(
  calc_stats(all_fits, type = "quantiles", average = TRUE)
)
#> Warning: The `average` argument of `calc_stats.data.frame()` is deprecated as of dRiftDM
#> 0.3.0.
#> ℹ Please use the `level` argument instead.
#> Type of Statistic: quantiles
#> 
#>    Source   Cond Prob Quant_corr Quant_err
#> 1     obs   comp  0.1      0.324     0.311
#> 2     obs   comp  0.2      0.346     0.330
#> 3     obs   comp  0.3      0.362     0.348
#> 4     obs   comp  0.4      0.368     0.365
#> 5     obs   comp  0.5      0.385     0.381
#> 6     obs   comp  0.6      0.396     0.383
#> 7     obs   comp  0.7      0.407     0.386
#> 8     obs   comp  0.8      0.424     0.391
#> 9     obs   comp  0.9      0.470     0.396
#> 10    obs incomp  0.1      0.351     0.311
#> ...
#> 
#> (access the data.frame's columns/rows as usual)
```
