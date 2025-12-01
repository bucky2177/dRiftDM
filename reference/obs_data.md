# The Observed Data

Functions to get or set the "observed data" of an object.

## Usage

``` r
obs_data(object, ...) <- value

# S3 method for class 'drift_dm'
obs_data(object, ..., eval_model = FALSE) <- value

obs_data(object, ...)

# S3 method for class 'drift_dm'
obs_data(object, ..., messaging = TRUE)

# S3 method for class 'fits_ids_dm'
obs_data(object, ...)

# S3 method for class 'fits_agg_dm'
obs_data(object, ...)
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

  a [data.frame](https://rdrr.io/r/base/data.frame.html) which provides
  three columns: (1) `RT` for the response times, (2) a column for
  boundary coding according to the model's
  [`b_coding()`](https://bucky2177.github.io/dRiftDM/reference/b_coding.md), (3)
  `Cond` for specifying the conditions.

- eval_model:

  logical, indicating if the model should be re-evaluated or not when
  updating the solver settings (see
  [re_evaluate_model](https://bucky2177.github.io/dRiftDM/reference/re_evaluate_model.md)).
  Default is `FALSE`.

- messaging:

  logical, indicating if messages shall be displayed or not.

## Value

For `obs_data()` a [data.frame](https://rdrr.io/r/base/data.frame.html)
of the observed data. The method `obs_data.drift_dm()` per default
displays a message to remind the user that the returned
[data.frame](https://rdrr.io/r/base/data.frame.html) is likely sorted
differently than expected.

For `obs_data<-()` the updated
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
object.

## Details

`obs_data()` is a generic accessor function, and `obs_data<-()` is a
generic replacement function. The default methods get and set the
"observed data". Their behavior, however, may be a bit unexpected.

In [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
objects, the observed data are not stored as a
[data.frame](https://rdrr.io/r/base/data.frame.html). Instead, any
supplied observed data set is disassembled into RTs for the upper and
lower boundary and with respect to the different conditions (ensures
more speed and easier programming in the depths of the package). Yet,
`obs_data()` returns a `data.frame` for
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
objects. This implies that `obs_data()` does not merely access the
observed data, but re-assembles it. Consequently, a returned
[data.frame](https://rdrr.io/r/base/data.frame.html) for the observed
data is likely sorted differently than the
[data.frame](https://rdrr.io/r/base/data.frame.html) that was originally
set to the model via `obs_data<-()`. Also, when the originally supplied
data set provided more conditions than the model, the unused conditions
will not be part of the returned
[data.frame](https://rdrr.io/r/base/data.frame.html).

For `fits_ids_dm` (see
[load_fits_ids](https://bucky2177.github.io/dRiftDM/reference/load_fits_ids.md)),
the observed data are stored as a
[data.frame](https://rdrr.io/r/base/data.frame.html) in the general fit
procedure info. This is the
[data.frame](https://rdrr.io/r/base/data.frame.html) that `obs_data()`
will return. Thus, the returned
[data.frame](https://rdrr.io/r/base/data.frame.html) will match with the
[data.frame](https://rdrr.io/r/base/data.frame.html) that was initially
supplied to
[estimate_model_ids](https://bucky2177.github.io/dRiftDM/reference/estimate_model_ids.md),
although with unused conditions being dropped.

In theory, it is possible to update parts of the "observed data".
However, because `obs_data()` returns a re-assembled
[data.frame](https://rdrr.io/r/base/data.frame.html) for
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
objects, great care has to be taken with respect to the ordering of the
argument `value`. A message is displayed to remind the user that the
returned [data.frame](https://rdrr.io/r/base/data.frame.html) may be
sorted differently than expected.

## Note

There is only a replacement function for
[drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
objects. This is because replacing the observed data after the model has
been fitted (i.e., for a `fits_ids_dm` object) doesn't make sense.

## See also

[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)

## Examples

``` r
# Set some data to a model -------------------------------------------------
my_model <- dmc_dm() # DMC is pre-built and directly available
# synthetic data suitable for DMC; comes with dRiftDM
some_data <- dmc_synth_data
obs_data(my_model) <- some_data

# Extract data from a model ------------------------------------------------
head(obs_data(my_model))
#> Extracting observed data from a model object. Remember that the result may be sorted differently than expect!
#>      RT Error Cond
#> 1 0.349     0 comp
#> 2 0.444     0 comp
#> 3 0.441     0 comp
#> 4 0.572     0 comp
#> 5 0.438     0 comp
#> 6 0.535     0 comp

# Important: ---------------------------------------------------------------
# The returned data.frame may be sorted differently than the one initially
# supplied.
some_data <- some_data[sample(1:nrow(some_data)), ] #' # shuffle the data set
obs_data(my_model) <- some_data
all.equal(obs_data(my_model), some_data)
#> Extracting observed data from a model object. Remember that the result may be sorted differently than expect!
#> [1] "Attributes: < Component “row.names”: Mean relative difference: 0.6615267 >"
#> [2] "Component “RT”: Mean relative difference: 0.2195587"                       
#> [3] "Component “Error”: Mean relative difference: 2"                            
#> [4] "Component “Cond”: 294 string mismatches"                                   
# so don't do obs_data(my_model)["Cond"] <- ...

# Addition: ----------------------------------------------------------------
# accessor method also available for fits_ids_dm objects
# (see estimate_model_ids)
# get an exemplary fits_ids_dm object
fits <- get_example_fits("fits_ids_dm")
head(obs_data(fits))
#>   ID        RT Error   Cond
#> 1  1 0.6016572     0   comp
#> 2  1 0.4514293     0   comp
#> 3  1 0.4180971     0   comp
#> 4  1 0.4514365     0 incomp
#> 5  1 0.4013124     0   comp
#> 6  1 0.4347489     0 incomp
```
