# The Component Functions of A Model

Functions to get or set the "component functions" of an object. The
component functions are a list of functions providing the drift rate,
boundary, starting point distribution, and non-decision time
distribution They are at the heart of the package and shape the model's
behavior.

## Usage

``` r
comp_funs(object, ...) <- value

# S3 method for class 'drift_dm'
comp_funs(object, ..., eval_model = FALSE) <- value

comp_funs(object, ...)

# S3 method for class 'drift_dm'
comp_funs(object, ...)

# S3 method for class 'fits_ids_dm'
comp_funs(object, ...)

# S3 method for class 'fits_agg_dm'
comp_funs(object, ...)
```

## Arguments

- object:

  an object of type
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md),
  `fits_ids_dm`, or `fits_agg_dm` (see
  [`estimate_dm()`](https://bucky2177.github.io/dRiftDM/reference/estimate_dm.md)).

- ...:

  additional arguments passed down to the specific method.

- value:

  a named list which provides the component functions to set (see
  Details)

- eval_model:

  logical, indicating if the model should be re-evaluated or not when
  updating the component funtions (see
  [re_evaluate_model](https://bucky2177.github.io/dRiftDM/reference/re_evaluate_model.md)).
  Default is `FALSE`.

## Value

For `comp_funs()` the list of component functions.

For `comp_funs<-()` the updated
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
object.

## Details

`comp_funs()` is a generic accessor function, and `comp_funs<-()` is a
generic replacement function. The default methods get and set the
"component functions". The component functions are a list of functions,
with the following names (see also
[`vignette("customize_ddms", "dRiftDM")`](https://bucky2177.github.io/dRiftDM/articles/customize_ddms.md)
for examples):

- `mu_fun` and `mu_int_fun`, provide the drift rate and its integral,
  respectively, across the time space.

- `x_fun` provides a distribution of the starting point across the
  evidence space.

- `b_fun` and `dt_b_fun` provide the values of the upper decision
  boundary and its derivative, respectively, across the time space. It
  is assumed that boundaries are symmetric.

- `nt_fun` provides a distribution of the non-decision component across
  the time space.

All of the listed functions are stored in the list `comp_funs` of the
respective model (see also
[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)).

Each component function must take the model's parameters (i.e., one row
of `prms_matrix`), the parameters for deriving the PDFs, the time or
evidence space, a condition, and a list of optional values as arguments.
These arguments are provided with values when `dRiftDM` internally calls
them.

In order to work with `dRiftDM`, `mu_fun`, `mu_int_fun`, `b_fun`,
`dt_b_fun`, and `nt_fun` must have the following declaration:
`my_fun = function(prms_model, prms_solve, t_vec, one_cond, ddm_opts`).
Here, `prms_model` is one row of `prms_matrix`,
[prms_solve](https://bucky2177.github.io/dRiftDM/reference/prms_solve.md)
the parameters relevant for deriving the PDFs, `t_vec` the time space,
going from 0 to `t_max` with length `nt + 1` (see
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)),
and `one_cond` a single character string, indicating the current
condition. Finally `dmm_opts` may contain additional values. Each
function must return a numeric vector of the same length as `t_vec`. For
`mu_fun`, `mu_int_fun`, `b_fun`, `dt_b_fun` the returned values provide
the respective boundary/drift rate (and their derivative/integral) at
every time step \\t\\. For `nt_fun` the returned values provide the
density of the non-decision time across the time space (which get
convoluted with the pdfs when solving the model)

In order to work with `dRiftDM`, `x_fun` must have the following
declaration:
`my_fun = function(prms_model, prms_solve, x_vec, one_cond, ddm_opts`).
Here, `x_vec` is the evidence space, going from -1 to 1 with length
`nx + 1` (see
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)).
Each function must return a numeric vector of the same length as
`x_vec`, providing the density values of the starting points across the
evidence space.

### Drift rate and its integral:

The drift rate is the first derivative of the expected time-course of
the diffusion process. For instance, if we assume that the diffusion
process \\X\\ is linear with a slope of \\v\\... \$\$E(X) = v \cdot
t\$\$ ...then the drift rate at every time step \\t\\ is the constant
\\v\\, obtained by taking the derivative of the expected time-course
with respect to \\t\\: \$\$\mu(t) = v\$\$ Conversely, the integral of
the drift rate is identical to the expected time-course:
\$\$\mu\_{int}(t) = v \cdot t\$\$

For the drift rate `mu_fun`, the default function when calling
[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
is a numeric vector containing the number \\3\\. Its integral
counterpart `mu_int_fun` will return a numeric vector containing the
values `t_vec*3`.

### Starting Point Distribution:

The starting point of a diffusion model refers to the initial value
taken by the evidence accumulation process at time \\t=0\\. This is a
PDF over the evidence space.

The default function when calling
[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
will be a function returning a dirac delta on zero, meaning that every
potential diffusion process starts at 0.

### Boundary:

The Boundary refers to the values of the absorbing boundaries at every
time step \\t\\ in a diffusion model. In most cases, this will be a
constant. For instance: \$\$b(t) = b\$\$ In this case, its derivative
with respect to \\t\\ is 0.

The default function when calling
[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
will be function for `b_fun` returning a numeric vector of length
`length(t_vec)` containing the number \\0.5\\. Its counterpart `dt_b`
will return a numeric vector of the same length containing its
derivative, namely, `0`.

### Non-Decision Time:

The non-decision time refers to an additional time-requirement. Its
distribution across the time space will be convoluted with the PDFs
derived from the diffusion process.

In psychology, the non-decision time captures time-requirements outside
the central decision process, such as stimulus perception and motor
execution.

The default function when calling
[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
returns a dirac delta on \\t = 0.3\\.

## Note

There is only a replacement function for
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
objects. This is because replacing the component functions after the
model has been fitted (i.e., for a `fits_ids_dm` object) doesn't make
sense.

## See also

[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)

## Examples

``` r
# get a pre-built model for demonstration
my_model <- ratcliff_dm()
names(comp_funs(my_model))
#> [1] "mu_fun"     "mu_int_fun" "x_fun"      "b_fun"      "dt_b_fun"  
#> [6] "nt_fun"    

# direct replacement (see customize_ddms for a more information on
# how to write custom component functions)
# 1. Choose a uniform non-decision time from the pre-built component_shelf()
nt_uniform <- component_shelf()$nt_uniform
# swap it in
comp_funs(my_model)[["nt_fun"]] <- nt_uniform

# now update the flex_prms object to ensure that this model has the required
# parameters
prms <- c(muc = 3, b = 0.6, non_dec = 0.3, range_non_dec = 0.05)
conds <- "null"
new_flex_prms <- flex_prms(prms, conds = conds)
flex_prms(my_model) <- new_flex_prms

# accessor method also available for fits_ids_dm objects
# (see estimate_model_ids)
# get an exemplary fits_ids_dm object
fits <- get_example_fits("fits_ids_dm")
names(comp_funs(fits))
#> [1] "mu_fun"     "mu_int_fun" "x_fun"      "b_fun"      "dt_b_fun"  
#> [6] "nt_fun"    
```
