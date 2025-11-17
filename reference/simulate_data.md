# Simulate Synthetic Responses

This function simulates data based on the provided model. To this end,
random samples from the predicted PDFs are drawn via approximate inverse
CDF sampling.

## Usage

``` r
simulate_data(object, ...)

# S3 method for class 'drift_dm'
simulate_data(
  object,
  ...,
  n,
  conds = NULL,
  k = 1,
  lower = NULL,
  upper = NULL,
  df_prms = NULL,
  seed = NULL,
  progress = 1
)
```

## Arguments

- object:

  an object inheriting from
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md).

- ...:

  further arguments passed on to other functions, i.e.,
  [`simulate_values()`](https://bucky2177.github.io/dRiftDM/reference/simulate_values.md)
  and
  [`simulate_one_data_set()`](https://bucky2177.github.io/dRiftDM/reference/simulate_one_data_set.md).
  This allows users to control the distribution from which original
  parameter values are drawn (if `k` \> 0) and the number of decimal
  places that the simulated RTs should have. If users want to use a
  different distribution than uniform for
  [`simulate_values()`](https://bucky2177.github.io/dRiftDM/reference/simulate_values.md),
  they must provide the additional arguments (e.g., `means` and `sds`)
  in a format like `lower/upper`.

- n:

  numeric, the number of trials per condition to draw. If a single
  numeric, then each condition will have `n` trials. Can be a (named)
  numeric vector with the same length as there are conditions to allow a
  different number of trials per condition.

- conds:

  character vector, specifying the conditions to sample from. Default
  `NULL` is equivalent to `conds(object)`.

- k:

  numeric larger than 0, indicating how many data sets shall be
  simulated. If \> 1, users must specify `lower/upper`.

- lower, upper:

  vectors or a list, specifying the simulation space for each parameter
  of the model (see Details). Only relevant for `k > 1`

- df_prms:

  an optional data.frame providing the parameters that should be used
  for simulating the data. `df_prms` must provide column names matching
  with (`coef(object, select_unique = TRUE)`), plus a column `ID` that
  will identify each simulated data set.

- seed:

  a single numeric, an optional seed for reproducible sampling

- progress:

  an integer, indicating if information about the progress should be
  displayed. 0 -\> no information, 1 -\> a progress bar. Default is 1.
  Only effective when `k > 1`.

## Value

The return value depends on whether a user specifies `lower/upper` or
`df_prms`. If none of these are specified and if `k = 1`, then a
[data.frame](https://rdrr.io/r/base/data.frame.html) containing the
columns `RT`, `Error`, and `Cond` is returned.

If `lower/upper` or `df_prms` are provided, then a list with entries
`synth_data` and `prms` is returned. The entry `synth_data` contains a
[data.frame](https://rdrr.io/r/base/data.frame.html), with the columns
`RT`, `<b_column>`, `Cond`, and `ID` (the name of the second column,
`<b_column>`, depends on the
[b_coding](https://bucky2177.github.io/dRiftDM/reference/b_coding.md) of
the model object). The entry `prms` contains a data.frame with an `ID`
column and the parameters used for simulating each synthetic data set.

## Details

`simulate_data` is a generic function for simulating data based on
approximate inverse CDF sampling. CDFs are derived from the model's PDFs
and data is drawn by mapping samples from a uniform distribution (in
\\\[0, 1\]\\) to the values of the CDF. Note that sampled response times
will correspond to the values of the time space (i.e., they will
correspond to `seq(0, t_max, dt)`, see
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)).

For `drift_dm` objects, the behavior of `simulate_data` depends on `k`.
If `k = 1` and no `lower/upper` or `df_prms` arguments are supplied,
then the parameters currently set to the model are used to generate the
synthetic data. If `k > 1`, then `k` parameter combinations are either
randomly drawn via
[simulate_values](https://bucky2177.github.io/dRiftDM/reference/simulate_values.md)
or gathered from the provided data.frame `df_prms`, and then data is
simulated for each parameter combination.

When specifying `lower/upper`, parameter combinations are simulated via
[simulate_values](https://bucky2177.github.io/dRiftDM/reference/simulate_values.md).
This comes in handy for simple parameter recovery exercises. If
`df_prms` is specified, then the parameter combinations from this
[data.frame](https://rdrr.io/r/base/data.frame.html) is used. Note that
the column names in `df_prms` must match with the (unique) parameter
combinations of the model (see `print(coef(object))`)

### Details on how to specify `lower/upper`.

When users want to simulate data with `k > 1` and `lower/upper`, then
parameter values have to be drawn. One great aspect about the
[flex_prms](https://bucky2177.github.io/dRiftDM/reference/flex_prms.md)
object within each
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
model, is that users can easily allow certain parameters to vary freely
across conditions. Consequently, the actual number of parameters varies
with the settings of the
[flex_prms](https://bucky2177.github.io/dRiftDM/reference/flex_prms.md)
object. In many cases, however, the simulation space for a parameter is
the same across conditions. For instance, in a model, the parameter "mu"
may vary across the conditions "easy", "medium", or "hard", but the
lower/upper limits are the same across conditions. To avoid that users
always have to re-specify the simulation space via the `lower/upper`
arguments, the `lower` and `upper` arguments refer to the parameter
labels, and `dRiftDM` figures out how to map these to all parameters
that vary across conditions.

Here is an example: Assume you have the model with parameters "A" and
"B", and the conditions "foo" and "bar". Now assume that "A" is allowed
to vary for "foo" and "bar". Thus, there are actually three parameters;
"A~foo", "A~bar", and "B". `dRiftDM`, however, can help with this. If we
provide `lower = c(A = 1, B = 2)`, `upper = c(A = 3, B = 4)`,
`simulate_data` checks the model, and creates the vectors
`temp_lower = c(1,1,2)` and `temp_upper = c(3,3,4)` as a basis to
simulate the parameters.

Users have three options to specify the simulation space:

- Plain numeric vectors (not very much recommended). In this case,
  `lower/upper` must be sorted in accordance with the free parameters in
  the `flex_prms_obj` object (call `print(<model>)` and have a look at
  the `Parameter Settings` output)

- Named numeric vectors. In this case `lower/upper` have to provide
  labels in accordance with the parameters that are considered "free" at
  least once across conditions.

- The most flexible way is when `lower/upper` are lists. In this case,
  the list requires an entry called "default_values" which specifies the
  named or plain numeric vectors as above. If the list only contains
  this entry, then the behavior is as if `lower/upper` were already
  numeric vectors. However, the `lower/upper` lists can also provide
  entries labeled as specific conditions, which contain named (!)
  numeric vectors with parameter labels. This will modify the value for
  the upper/lower parameter space with respect to the specified
  parameters in the respective condition.

## Note

A function for `fits_ids_dm` will be provided in the future.

## Examples

``` r
# Example 1 ----------------------------------------------------------------
# get a pre-built model for demonstration
a_model <- ratcliff_dm()

# define a lower and upper simulation space
lower <- c(1, 0.4, 0.1)
upper <- c(6, 0.9, 0.5)

# now simulate 5 data sets with each 100 trials
data_prms <- simulate_data(a_model,
  n = 100, k = 5, lower = lower,
  upper = upper, seed = 1, progress = 0
)
head(data_prms$synth_data)
#>      RT Error Cond ID
#> 1 0.600     0 null  1
#> 2 1.369     0 null  1
#> 3 0.416     0 null  1
#> 4 0.653     0 null  1
#> 5 0.928     0 null  1
#> 6 0.353     0 null  1
head(data_prms$prms)
#>   ID      muc         b   non_dec
#> 1  1 2.327543 0.8491948 0.1823898
#> 2  2 2.860619 0.8723376 0.1706227
#> 3  3 3.864267 0.7303989 0.3748091
#> 4  4 5.541039 0.7145570 0.2536415
#> 5  5 2.008410 0.4308931 0.4079366

# Example 2 ----------------------------------------------------------------
# more flexibility when defining lists for lower and upper
# get a pre-built model, and allow muc to vary across conditions
a_model <- dmc_dm(instr = "muc ~ ")

# define a lower and upper simulation space
# let muc vary between 2 and 6, but in incomp conditions, let it vary
# between 1 and 4
lower <- list(
  default_values = c(
    muc = 2, b = 0.4, non_dec = 0.1,
    sd_non_dec = 0.01, tau = 0.02, A = 0.05,
    alpha = 3
  ),
  incomp = c(muc = 1)
)
upper <- list(
  default_values = c(
    muc = 6, b = 0.9, non_dec = 0.4,
    sd_non_dec = 0.15, tau = 0.15, A = 0.15,
    alpha = 7
  ),
  incomp = c(muc = 4)
)

data_prms <- simulate_data(a_model,
  n = 100, k = 5, lower = lower,
  upper = upper, seed = 1, progress = 0
)
range(data_prms$prms$muc.comp)
#> [1] 2.806728 5.632831
range(data_prms$prms$muc.incomp)
#> [1] 1.185359 3.834026
```
