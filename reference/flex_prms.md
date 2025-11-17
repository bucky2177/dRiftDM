# Flex_Prms

Functions for creating, accessing replacing, or printing a `flex_prms`
object. Any object of type `flex_prms` provides a user-friendly way to
specify dependencies, parameter values etc. for a model.

## Usage

``` r
flex_prms(object, ...) <- value

# S3 method for class 'drift_dm'
flex_prms(object, ..., eval_model = FALSE) <- value

flex_prms(object, ...)

# S3 method for class 'numeric'
flex_prms(object, ..., conds, instr = NULL, messaging = NULL)

# S3 method for class 'flex_prms'
flex_prms(object, ...)

# S3 method for class 'drift_dm'
flex_prms(object, ...)

# S3 method for class 'flex_prms'
print(
  x,
  ...,
  round_digits = drift_dm_default_rounding(),
  dependencies = TRUE,
  cust_parameters = TRUE
)
```

## Arguments

- object:

  an `R` object (see Details)

- ...:

  additional arguments passed on to the specific method.

- value:

  an object of type `flex_prms`.

- eval_model:

  logical, indicating if the model should be re-evaluated or not when
  replacing the `flex_prms` object (see
  [re_evaluate_model](https://bucky2177.github.io/dRiftDM/reference/re_evaluate_model.md)).

- conds:

  A character vector, giving the names of the model's conditions. values
  within `conds` will be used when addressing the data and when deriving
  the model's predictions.

- instr:

  optional string with "instructions", see
  [`modify_flex_prms()`](https://bucky2177.github.io/dRiftDM/reference/modify_flex_prms.md).

- messaging:

  optional logical, indicates if messages shall be displayed when
  processing `instr`.

- x:

  an object of type `flex_prms`

- round_digits:

  integer, controls the number of digits shown when printing out a
  `flex_prms` object. Default is `3`.

- dependencies:

  logical, controlling if a summary of the special dependencies shall be
  printed.

- cust_parameters:

  logical, controlling if a summary of the custom parameters shall be
  printed.

## Value

The specific value returned depends on which method is called

### Creating an object of type `flex_prms`

Can be achieved by calling `flex_prms()` with a named numeric vector,
thus when calling the underlying method `flex_prms.numeric` (see the
example below). In this case a list with the class label `"flex_prms"`
is returned. It containts three entries:

- A nested list `internal_list`. This list specifies the dependencies
  and restrains enforced upon the parameters across conditions. Integers
  \>= 1 indicate that this parameter will be estimated for a specific
  condition, and conditions with the same number refer to a single
  parameter. Integers == 0 indicate thtat this parameter will not be
  esitmated for a specific condition (i.e., it is considered "fixed").
  Expressions will be evaluated at run time and specify special
  dependencies among parameters.

- A nested list `linear_internal_list`. This list essentially contains
  the same information as `internal_list`, but the parameters are sorted
  so that they can be mapped to an integer vector (relevant only in the
  depths of the package for the minimization routines).

- A numeric matrix `prms_matrix` which contains the currently set values
  for each parameter across all conditions. Per default, the values of
  each parameter are set equal across all conditions. Additionally, each
  parameter is assumed to be restrained as equal across all conditions.
  The values for all parameters given a condition will be passed to the
  component functions (see
  [comp_funs](https://bucky2177.github.io/dRiftDM/reference/comp_funs.md)).

- (optional) A list of additional parameters `cust_prms` that are
  derived from the parameters in `prms_matrix`.

### Accessing an object of type `flex_prms`

Users can access/get the `flex_prms` object when calling `flex_prms()`
with an object of type
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md),
`fits_ids_dm` (see
[`estimate_model_ids()`](https://bucky2177.github.io/dRiftDM/reference/estimate_model_ids.md)),
or `flex_prms`. In this case, the stored `flex_prms` object is returned.

### Replacing an object of type `flex_prms`

The `flex_prms` object stored within an object of type
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
can be replaced by calling the generic `flex_prms<-` replacement
function. In this case, the modified
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
object is returned.

### Printing an object of type `flex_prms`

The `print.flex_prms()` method invisibly returns the supplied
`flex_prms` object.

## Details

Objects of type `flex_prms` can be modified using the generic
[`modify_flex_prms()`](https://bucky2177.github.io/dRiftDM/reference/modify_flex_prms.md)
function and a corresponding set of "instructions" (see the respective
function for more details).

`flex_prms()` is a generic function. If called with a named numeric
vector, then this will create an object of type `flex_prms` (requires
`conds` to be specified). If called with other data types, gives the
respective `flex_prms` object

`flex_prms<-()` is a generic replacement function. Currently this only
supports objects of type
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md).
It will replace/update the model with a new instance of type
`flex_prms`.

## Note

There is only a replacement function for
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
objects. This is because replacing the solver settings after the model
has been fitted (i.e., for a `fits_ids_dm` object) doesn't make sense.

## See also

[`estimate_model_ids()`](https://bucky2177.github.io/dRiftDM/reference/estimate_model_ids.md),
[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md),
[`summary.flex_prms()`](https://bucky2177.github.io/dRiftDM/reference/summary.flex_prms.md),
[`modify_flex_prms()`](https://bucky2177.github.io/dRiftDM/reference/modify_flex_prms.md)

## Examples

``` r
# Create a flex_prms object -----------------------------------------------
conds <- c("one", "two")
prms <- c(muc = 3, b = 0.5)
one_instr <- "muc ~ one + two"
flex_prms_obj <- flex_prms(
  prms,
  conds = conds,
  instr = one_instr
)
print(flex_prms_obj)
#> Parameter Values:
#>     muc   b
#> one   3 0.5
#> two   3 0.5
#> 
#> Parameter Settings:
#>     muc b
#> one   1 3
#> two   2 3
#> 


# Access a flex_prms object of a model ------------------------------------
my_model <- ratcliff_dm() # the Ratcliff DDM comes with dRiftDM
print(flex_prms(my_model))
#> Parameter Values:
#>      muc   b non_dec
#> null   3 0.6     0.3
#> 
#> Parameter Settings:
#>      muc b non_dec
#> null   1 2       3
#> 


# Replace the flex_prms object of a model ---------------------------------
# create a new flex_prms object
conds <- c("one", "two")
prms <- c(muc = 3, b = 0.6, non_dec = 0.3)
new_flex_prms_obj <- flex_prms(
  prms,
  conds = conds
)

flex_prms(my_model) <- new_flex_prms_obj

# acess the new flex_prms object
print(flex_prms(my_model))
#> Parameter Values:
#>     muc   b non_dec
#> one   3 0.6     0.3
#> two   3 0.6     0.3
#> 
#> Parameter Settings:
#>     muc b non_dec
#> one   1 2       3
#> two   1 2       3
#> 


# Control the print method -------------------------------------------------
dmc_model <- dmc_dm() # another, more complex, model; comes with dRiftDM
print(flex_prms(dmc_model), round_digits = 1, cust_parameters = FALSE)
#> Parameter Values:
#>        muc   b non_dec sd_non_dec tau a    A alpha
#> comp     4 0.6     0.3          0   0 2  0.1     4
#> incomp   4 0.6     0.3          0   0 2 -0.1     4
#> 
#> Parameter Settings:
#>        muc b non_dec sd_non_dec tau a A alpha
#> comp   1   2 3       4          5   0 6 7    
#> incomp 1   2 3       4          5   0 d 7    
#> 
#> Special Dependencies:
#> A ~ incomp == -(A ~ comp)
#> 
```
