# Simulate Values

Draw values, most likely model parameters.

## Usage

``` r
simulate_values(
  lower,
  upper,
  k,
  distr = NULL,
  cast_to_data_frame = TRUE,
  add_id_column = "numeric",
  seed = NULL,
  ...
)
```

## Arguments

- lower, upper:

  Numeric vectors, indicating the lower/upper boundary of the drawn
  values.

- k:

  Numeric, the number of values to be drawn for each value pair of
  lower/upper. If named numeric, the labels are used for the column
  names of the returned object

- distr:

  Character, indicating which distribution to draw from. Currently
  available are: `"unif"` for a uniform distribution or `"tnorm"` for a
  truncated normal distribution. `NUll` will lead to `"unif"` (default).

- cast_to_data_frame:

  Logical, controls whether the returned object is of type data.frame
  (TRUE) or matrix (FALSE). Default is TRUE

- add_id_column:

  Character, controls whether an ID column should be added. Options are
  "numeric", "character", or "none". If "numeric" or "character" the
  column ID provides values from 1 to k of the respective type. If none,
  no column is added. Note that "character" casts all simulated values
  to character if the argument `cast_to_data_frame` is set to FALSE.

- seed:

  Numeric, optional seed for making the simulation reproducable (see
  details)

- ...:

  Further arguments relevant for the distribution to draw from

## Value

If `cast_to_data_frame` is TRUE, a data.frame with `k` rows and at least
`length(lower);length(upper)` columns. Otherwise a matrix with the same
number of rows and columns. Columns are labeled either from V1 to Vk or
in case `lower` and `upper` are named numeric vectors using the labels
of both vectors.

If `add_id_column` is not "none", an ID column is provided of the
respective data type.

The data type of the parameters will be numeric, unless `add_id_column`
is "character" and `cast_to_data_frame` is FALSE. In this case the
returned matrix will be of type character.

## Details

When drawing from a truncated normal distribution, users must provide
values for the arguments `means` and `sds`. These are numeric vectors of
the same size as `lower` and `upper`, and indicate the mean and the
standard deviation of the normal distributions.

## Examples

``` r
# Example 1: Draw from uniform distributions ------------------------------
lower <- c(a = 1, b = 1, c = 1)
upper <- c(a = 3, b = 4, c = 5)
values <- simulate_values(
  lower = lower,
  upper = upper,
  k = 50,
  add_id_column = "none"
)
summary(values)
#>        a               b               c        
#>  Min.   :1.033   Min.   :1.126   Min.   :1.004  
#>  1st Qu.:1.533   1st Qu.:2.128   1st Qu.:2.504  
#>  Median :1.849   Median :2.545   Median :3.428  
#>  Mean   :1.953   Mean   :2.640   Mean   :3.258  
#>  3rd Qu.:2.316   3rd Qu.:3.392   3rd Qu.:4.209  
#>  Max.   :2.901   Max.   :3.887   Max.   :4.943  

# Example 2: Draw from truncated normal distributions ---------------------
lower <- c(a = 1, b = 1, c = 1)
upper <- c(a = 3, b = 4, c = 5)
means <- c(a = 2, b = 2.5, c = 3)
sds <- c(a = 0.5, b = 0.5, c = 0.5)
values <- simulate_values(
  lower = lower,
  upper = upper,
  distr = "tnorm",
  k = 5000,
  add_id_column = "none",
  means = means,
  sds = sds
)
quantile(values$a, probs = c(0.025, 0.5, 0.975))
#>     2.5%      50%    97.5% 
#> 1.158297 1.997958 2.859411 
quantile(values$b, probs = c(0.025, 0.5, 0.975))
#>     2.5%      50%    97.5% 
#> 1.545730 2.491228 3.466518 
quantile(values$c, probs = c(0.025, 0.5, 0.975))
#>     2.5%      50%    97.5% 
#> 2.065234 3.008299 3.973892 
```
