# Unpack/Destroy dRiftDM Objects

When calling
[`simulate_traces()`](https://bucky2177.github.io/dRiftDM/reference/simulate_traces.md),
[calc_stats](https://bucky2177.github.io/dRiftDM/reference/calc_stats.md),
or
[coef.fits_ids_dm](https://bucky2177.github.io/dRiftDM/reference/coef.drift_dm.md)
the returned objects will be custom objects (e.g., subclasses of
[list](https://rdrr.io/r/base/list.html) or
[data.frame](https://rdrr.io/r/base/data.frame.html)). The respective
subclasses were created to provide convenient plotting and printing, but
they don't really provide any additional functionality.

The goal of `unpack_obj()` is to provide a convenient way to strip away
the attributes of the respective objects (revealing them as standard
[array](https://rdrr.io/r/base/array.html)s,
[data.frame](https://rdrr.io/r/base/data.frame.html)s, or
[list](https://rdrr.io/r/base/list.html)s).

## Usage

``` r
unpack_obj(object, ...)

# S3 method for class 'traces_dm'
unpack_obj(object, ..., unpack_elements = TRUE)

# S3 method for class 'traces_dm_list'
unpack_obj(object, ..., unpack_elements = TRUE, conds = NULL)

# S3 method for class 'stats_dm'
unpack_obj(object, ..., unpack_elements = TRUE)

# S3 method for class 'stats_dm_list'
unpack_obj(object, ..., unpack_elements = TRUE, type = NULL)

# S3 method for class 'coefs_dm'
unpack_obj(object, ..., unpack_elements = TRUE)
```

## Arguments

- object:

  an object of type `stats_dm`, `stats_dm_list`, `traces_dm`,
  `traces_dm_list`, or `coefs_dm`

- ...:

  further arguments passed on to the respective method.

- unpack_elements:

  logical, indicating if the `traces_dm`, `stats_dm`, or `coefs_dm`
  objects shall be unpacked. Default is `TRUE`.

- conds:

  optional character vector, indicating specific condition(s). The
  default `NULL` will lead to `conds = conds(object)`. Thus, per default
  all conditions are addressed

- type:

  optional character vector, indicating specific type(s) of statistics.
  The default `NULL` will access all types of statics.

## Value

For `traces_dm_list`, the returned value is a list, if `conds` specifies
more than one condition. For example, if `conds = c("foo", "bar")`, then
the returned value is a list with the two (named) entries "foo" and
"bar". If the returned list would only have one entry (either because
the `traces_dm_list` has only one condition, see
[conds](https://bucky2177.github.io/dRiftDM/reference/conds.md), or
because a user explicitly requested only one condition), then the
underlying [array](https://rdrr.io/r/base/array.html) or `traces_dm`
object is returned directly.

For `stats_dm_list`, the returned value is a list, if `type` specifies
more than one condition. If the returned list would only have one entry,
then the underlying [data.frame](https://rdrr.io/r/base/data.frame.html)
or `stats_dm` object is returned directly.

For `traces_dm`, `unpack_obj()` returns an
[array](https://rdrr.io/r/base/array.html) with the traces, if
`unpack=TRUE`. If `unpack=FALSE`, the unmodified object is returned.

For `stats_dm`, `unpack_obj()` returns a
[data.frame](https://rdrr.io/r/base/data.frame.html) with the respective
statistic, if `unpack=TRUE`. If `unpack=FALSE`, the unmodified object is
returned.

For `coefs_dm`, `unpack_obj()` returns a
[data.frame](https://rdrr.io/r/base/data.frame.html) with the
parameters, if `unpack=TRUE`. If `unpack=FALSE`, the unmodified object
is returned.

## Details

`unpack_obj()` is a generic function to strip away the custom
information and class labels of `stats_dm`, `stats_dm_list`,
`traces_dm`, `traces_dm_list`, and `coefs_dm` objects. These objects are
created when calling
[`simulate_traces()`](https://bucky2177.github.io/dRiftDM/reference/simulate_traces.md),
[calc_stats](https://bucky2177.github.io/dRiftDM/reference/calc_stats.md),
or
[coef.fits_ids_dm](https://bucky2177.github.io/dRiftDM/reference/coef.drift_dm.md).

For `traces_dm_list`, `unpack_obj()` returns the requested conditions
(see the argument `conds`). The result contains objects of type
`traces_dm` if `unpack_elements = FALSE`. For `unpack_elements = TRUE`,
the result contains the plain
[array](https://rdrr.io/r/base/array.html)s with the traces.

For `stats_dm_list`, `unpack_obj()` returns the requested statistics
(see the argument `type`). The result contains objects of type
`stats_dm` if `unpack_elements = FALSE`. For `unpack_elements = TRUE`,
the result contains the plain
[data.frame](https://rdrr.io/r/base/data.frame.html)s with the
statistics.

## Examples

``` r
# get a pre-built model to demonstrate the function
my_model <- dmc_dm()

# get some traces ...
some_traces <- simulate_traces(my_model, k = 2, seed = 1)
some_traces <- some_traces$comp
class(some_traces)
#> [1] "traces_dm"
# ... unpack them to get the underlying arrays
class(unpack_obj(some_traces))
#> [1] "matrix" "array" 

# get some statistics ...
some_stats <- calc_stats(my_model, type = "cafs")
class(some_stats)
#> [1] "cafs"       "sum_dist"   "stats_dm"   "data.frame"
class(unpack_obj(some_stats))
#> [1] "data.frame"

# get some parameters ...
some_coefs <- coef(get_example_fits("fits_ids_dm"))
class(some_coefs)
#> [1] "coefs_dm"   "data.frame"
class(unpack_obj(some_coefs))
#> [1] "data.frame"
```
