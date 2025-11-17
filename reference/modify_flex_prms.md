# Set Instructions to a flex_prms object

Functions to carry out the "instructions" on how to modify a flex_prms
object, specified as a string.

## Usage

``` r
modify_flex_prms(object, instr, ...)

# S3 method for class 'drift_dm'
modify_flex_prms(object, instr, ..., eval_model = FALSE)

# S3 method for class 'flex_prms'
modify_flex_prms(object, instr, ..., messaging = NULL)
```

## Arguments

- object:

  an object of type `drift_dm` or `flex_prms`.

- instr:

  a character string, specifying a set of instructions (see Details).

- ...:

  further arguments passed forward to the respective method.

- eval_model:

  logical, indicating if the model should be re-evaluated or not when
  updating modifying the flex_prms object (see
  [re_evaluate_model](https://bucky2177.github.io/dRiftDM/reference/re_evaluate_model.md)).
  Default is `FALSE`.

- messaging:

  logical, indicating if messages shall be displayed or not. Can happen,
  for example, when setting a parameter value for a specific condition,
  although the parameter values are assumed to be the identical across
  conditions.

## Value

For
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
objects, the updated
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
object.

For
[flex_prms](https://bucky2177.github.io/dRiftDM/reference/flex_prms.md),
the updated
[flex_prms](https://bucky2177.github.io/dRiftDM/reference/flex_prms.md)
object.

## Details

`modify_flex_prms` is a generic function. The default methods pass
forward a set of "instructions" to modify the (underlying)
[flex_prms](https://bucky2177.github.io/dRiftDM/reference/flex_prms.md)
object.

These instructions are inspired by the model syntax of the `lavaan`
package. Note that specifying multiple instructions is possible, but
each instruction has to be defined in its own line. Comments with '#'
are possible, also line continuations are possible, if the last symbol
is a "+","-", "\*", "/", "(", or "\[". The following instructions are
implemented:

The **"vary"** instruction:

- Looks something like "a ~ foo + bar"

- This means that the parameter 'a' is allowed to vary independently for
  the conditions 'foo' and 'bar'

- Thus, when estimating the model, the user will have independent values
  for 'a' in conditions 'foo' and 'bar'

The **"restrain"** instruction:

- Looks something like "a ~! foo + bar "

- This means that the parameter 'a' is assumed to be identical for the
  conditions 'foo' and 'bar'

- Thus, when estimating the model, the user will have only a single
  value for 'a' in conditions 'foo' and 'bar'

The **"set"** instruction:

- Users may not always estimate a model directly but rather explore the
  model behavior. In this case setting the value of a parameter is
  necessary.

- The corresponding instruction looks something like "a ~ foo =\> 0.3"

- This will set the value for 'a' in condition 'foo' to the value of 0.3

The **"fix"** instruction:

- Oftentimes, certain parameters of a model are considered "fixed", so
  that they don't vary while the remaining parameters are estimated. An
  example would be the shape parameter 'a' of DMC (see
  [dmc_dm](https://bucky2177.github.io/dRiftDM/reference/dmc_dm.md)).

- The corresponding instruction looks something like "a \<!\> foo + bar"

- Usually, users want to call the "set" instruction prior or after the
  "fix" instruction, to set the corresponding parameter to a certain
  value.

The **"special dependency"** instruction:

- Sometimes, users want to allow one parameter to depend on another. For
  instance, in DMC (see
  [dmc_dm](https://bucky2177.github.io/dRiftDM/reference/dmc_dm.md)),
  the parameter A is positive in the congruent condition, but negative
  in the incongruent condition. Thus, parameters may have a 'special
  depencency' which can be expressed as an equation.

- To define a special dependency, users can use the operation "==". The
  parameter that should have the dependency is on the left-hand side,
  while the mathematical relationship to other parameters is defined on
  the right-hand side.

- This then looks something like "a ~ foo == -(a ~ bar)".

- This means that the parameter a in condition foo will always be -1 \*
  the parameter a in condition bar. Thus, if a in condition bar has the
  value 5, then a in condition foo will be -5.

- The expression on the right-side can refer to any arbitrary
  mathematical relation.

- Important: Make sure that each 'parameter ~ condition' combination on
  the right-hand side of the equation are set in brackets.

- Another example: Parameter a in condition foo should be the mean of
  the parameter b in conditions bar and baz; this would be the
  instruction "a ~ foo == 0.5\*(b ~ bar) + 0.5\*(b ~ baz)"

The **"additional/custom parameter combination"** instruction:

- Sometimes, users may want to combine multiple parameters to summarize
  a certain property of the model. For example, in DMC (see
  [dmc_dm](https://bucky2177.github.io/dRiftDM/reference/dmc_dm.md)),
  the shape and rate parameter jointly determine the peak latency.

- To avoid having to calculate this manually, users can define "custom"
  parameter combinations using the ":=" operation:

- An exemplary instruction might look like this: "peak_l := (a - 1) \*
  tau"

- Expressions and values that provide calculations for those parameters
  are stored in a separate list `cust_prms`.

## See also

[`flex_prms()`](https://bucky2177.github.io/dRiftDM/reference/flex_prms.md)

## Examples

``` r
# Example 1: Modify a flex_prms object  directly ---------------------------
# create an auxiliary flex_prms object
a_flex_prms_obj <- flex_prms(
  c(muc = 3, b = 0.5, non_dec = 0.3),
  conds = c("foo", "bar")
)

# then carry out some "instructions". Here (arbitrary operations):
# 1.) Consider b as fixed
# 2.) Let muc vary independently for the conditions foo and bar
# 3.) Set non_dec in condition bar to be half as large as non_dec in
#     condition bar
instr <-
  "b <!>
 muc ~
 non_dec ~ bar == (non_dec ~ foo) / 2
"
modify_flex_prms(object = a_flex_prms_obj, instr = instr)
#> Parameter Values:
#>     muc   b non_dec
#> foo   3 0.5    0.30
#> bar   3 0.5    0.15
#> 
#> Parameter Settings:
#>     muc b non_dec
#> foo 1   0 3      
#> bar 2   0 d      
#> 
#> Special Dependencies:
#> non_dec ~ bar == (non_dec ~ foo)/2
#> 


# Example 2: Modify a flex_prms object stored inside a drift_dm object -----
a_model <- ratcliff_dm() # get a model for demonstration purpose
modify_flex_prms(object = a_model, instr = "muc ~ => 4")
#> Class(es) ratcliff_dm, drift_dm
#> (model has not been estimated yet)
#> 
#> Parameter Values:
#>      muc   b non_dec
#> null   4 0.6     0.3
#> 
#> Parameter Settings:
#>      muc b non_dec
#> null   1 2       3
#> 
#> Deriving PDFS:
#>   solver: kfe
#>   values: sigma=1, t_max=3, dt=0.0075, dx=0.02, nt=400, nx=100
#> 
#> Cost Function: neg_log_like
#> 
#> Observed Data: NULL
```
