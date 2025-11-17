# Create a New stats_dm Object

`new_stats_dm` initializes a `stats_dm` object to label statistic types
and store necessary attributes for the custom methods (such as `plot_*`)

## Usage

``` r
new_stats_dm(stat_df, type, ...)
```

## Arguments

- stat_df:

  a `data.frame`, containing calculated statistics to be encapsulated
  within the `stats_dm` class.

- type:

  a character string, specifying the type of statistic provided by
  `stat_df`. Valid options include `"basic_stats"`, `"cafs"`,
  `"quantiles"`, `"delta_funs"`, and `"fit_stats"`.

- ...:

  Additional arguments passed to set attributes. For `"cafs"`,
  `"quantiles"`, and `"delta_funs"`, a `b_coding` attribute is required.

## Value

An object of class `stats_dm`, with additional classes and attributes
depending on `type`.

## Details

`new_stats_dm` sets up the `stat_df` object by assigning it the class
`stats_dm`, along with additional classes based on the specified `type`.
For "basic_stats", "cafs", "quantiles", "delta_funs", this will be
c(`type`, "sum_dist", "stats_dm", "data.frame")". For "fit_stats", this
will be c(`type`, "stats_dm", "data.frame")".

For basic stats, Conditional Accuracy Functions (CAFs), Quantiles, and
Delta Functions, the function requires a `b_coding` argument, which
specifies boundary coding details and is set as an attribute.

The function performs validation through
[validate_stats_dm](https://bucky2177.github.io/dRiftDM/reference/validate_stats_dm.md)
to ensure that the `stats_dm` object is well formatted.
