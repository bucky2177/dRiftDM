# Unpack/Destroy Traces Objects

**\[deprecated\]**

`unpack_traces()` is deprecated. Please use the more general
[`unpack_obj()`](https://bucky2177.github.io/dRiftDM/reference/unpack_obj.md)
function.

## Usage

``` r
unpack_traces(object, ...)

# S3 method for class 'traces_dm'
unpack_traces(object, ..., unpack = TRUE)

# S3 method for class 'traces_dm_list'
unpack_traces(object, ..., unpack = TRUE, conds = NULL)
```

## Arguments

- object:

  an object of type `traces_dm` or `traces_dm_list` (see
  [`simulate_traces()`](https://bucky2177.github.io/dRiftDM/reference/simulate_traces.md))

- ...:

  further arguments passed on to the respective method.

- unpack:

  logical, indicating if the `traces_dm` objects shall be unpacked.
  Default is `TRUE`.

- conds:

  optional character, indicating specific condition(s). The default
  `NULL` will lead to `conds = conds(object)`. Thus, per default all
  conditions are accessed.

## Value

For `traces_dm_list`, the returned value is a list, if `conds` specifies
more than one condition. For example, if `conds = c("foo", "bar")`, then
the returned value is a list with the two (named) entries "foo" and
"bar". If the returned list would only have one entry (either because
the `traces_dm_list` has only one condition, see
[conds](https://bucky2177.github.io/dRiftDM/reference/conds.md), or
because a user explicitly requested only one condition), then the
underlying array or `traces_dm` object is returned directly.

For `traces_dm`, `unpack_traces()` returns an array with the traces, if
`unpack=TRUE`. If `unpack=FALSE`, the unmodified object is returned.

## Details

`unpack_traces()` was a generic function to strip away the "unnecessary"
information of `traces_dm_list` and `traces_dm` objects. These objects
are created when calling
[`simulate_traces()`](https://bucky2177.github.io/dRiftDM/reference/simulate_traces.md).

For `traces_dm_list`, `unpack_traces()` returns the requested conditions
(see the argument `conds`). The result contains objects of type
`traces_dm` if `unpack = FALSE`. For `unpack = TRUE`, the result
contains the plain arrays with the traces.
