# Create a drift_dm object

This function creates an object of type drift_dm, which serves as the
parent class for all further created drift diffusion models (all of
which have a child class label, e.g., `dmc_dm`). The objects created by
`drift_dm()` are the backbone of the dRiftDM package. For a list of all
pre-built models, see
[`vignette("dRiftDM", "dRiftDM")`](https://bucky2177.github.io/dRiftDM/articles/dRiftDM.md).

## Usage

``` r
drift_dm(
  prms_model,
  conds,
  subclass,
  instr = NULL,
  obs_data = NULL,
  sigma = 1,
  t_max = 3,
  dt = 0.001,
  dx = 0.001,
  solver = "kfe",
  cost_function = "neg_log_like",
  mu_fun = NULL,
  mu_int_fun = NULL,
  x_fun = NULL,
  b_fun = NULL,
  dt_b_fun = NULL,
  nt_fun = NULL,
  b_coding = NULL
)

# S3 method for class 'drift_dm'
print(x, ..., round_digits = drift_dm_default_rounding())
```

## Arguments

- prms_model:

  a named numeric vector of the model parameters. The names indicate the
  model's parameters, and the numeric entries provide the current
  parameter values.

- conds:

  a character vector, giving the names of the model's conditions. values
  within `conds` will be used when addressing the data and when deriving
  the model's predictions.

- subclass:

  a character string, with a name for the newly created diffusion model
  (e.g., `my_dmc_dm`). This will be the child class.

- instr:

  an optional character string, providing "instructions" for the
  underlying
  [flex_prms](https://bucky2177.github.io/dRiftDM/reference/flex_prms.md)
  object.

- obs_data:

  an optional data.frame, providing a data set (see
  [`obs_data()`](https://bucky2177.github.io/dRiftDM/reference/obs_data.md)
  for more information).

- sigma:

  the diffusion constant. Default is `1`.

- t_max:

  the maximum of the time space. Default is set `3` (seconds).

- dt, dx:

  the step size of the time and evidence space discretization,
  respectively. Default is set to `.001` (which refers to seconds for
  dt). Note that these values are set conservatively per default. In
  many cases, users can increase the discretization.

- solver:

  a character string, specifying which approach to use for deriving the
  first passage time. Options are `kfe` or `im_zero`. Default is `kfe`,
  which provides access to the numerical discretization of the
  Kolmogorov Forward Equation.

- cost_function:

  a character string, specifying the cost function used during
  estimation. Options are `neg_log_like` (negative log-likelihood),
  `rmse` (root-mean-squared error). Default is `neg_log_like`.

- mu_fun, mu_int_fun, x_fun, b_fun, dt_b_fun, nt_fun:

  Optional custom functions defining the components of a diffusion
  model. See
  [`comp_funs()`](https://bucky2177.github.io/dRiftDM/reference/comp_funs.md).
  If an argument is `NULL`, dRiftDM falls back to the respective default
  functions, which are documented in
  [`comp_funs()`](https://bucky2177.github.io/dRiftDM/reference/comp_funs.md).

- b_coding:

  an optional list, specifying how boundaries are coded. See
  [`b_coding()`](https://bucky2177.github.io/dRiftDM/reference/b_coding.md).
  Default refers to accuracy coding.

- x:

  an object of type `drift_dm`

- ...:

  additional parameters

- round_digits:

  integer, controls the number of digits shown for `print.drift_dm()`.
  Default is `3`.

## Value

For `drift_dm()`, a list with the parent class label `"drift_dm"` and
the child class label `<subclass>`. The list contains the following
entries:

- An instance of the class
  [flex_prms](https://bucky2177.github.io/dRiftDM/reference/flex_prms.md)
  for controlling the model parameters. Provides information about the
  number of parameters, conditions etc.

- Parameters used for deriving the model predictions,
  [prms_solve](https://bucky2177.github.io/dRiftDM/reference/prms_solve.md),
  containing the diffusion constant (`sigma`), the maximum of the time
  space (`t_max`), the evidence and space discretization (`dt` and `dx`,
  respectively), and the resulting number of steps for the time and
  evidence space discretization (`nt` and `nx`, respectively).

- A character string
  [solver](https://bucky2177.github.io/dRiftDM/reference/solver.md),
  indicating the method for deriving the model predictions.

- A character string
  [cost_function](https://bucky2177.github.io/dRiftDM/reference/cost_function.md),
  indicating the cost function used for model estimation.

- A list of functions called
  [comp_funs](https://bucky2177.github.io/dRiftDM/reference/comp_funs.md),
  providing the components of the diffusion model (i.e., `mu_fun`,
  `mu_int_fun`, `x_fun`, `b_fun`, `dt_b_fun`, `nt_fun`). These functions
  are called in the depths of the package and will determine the
  behavior of the model

If (optional) observed data were passed via
[`obs_data()`](https://bucky2177.github.io/dRiftDM/reference/obs_data.md),
the list will contain an entry `obs_data`. This is a (nested) list with
stored response times for the upper and lower boundary and with respect
to each condition. If the cost function is a summary statistic requiring
quantiles, CAFs, etc., the model also contains the entries `stats_agg`
and `stats_agg_info`. The former is a (nested) list with descriptive
statistics. The latter contains information about the descriptive
statistics (e.g., the quantile levels).

If the model has been evaluated (see
[`re_evaluate_model()`](https://bucky2177.github.io/dRiftDM/reference/re_evaluate_model.md)),
the list will contain...

- ... the cost value; can be addressed via
  [`cost_value()`](https://bucky2177.github.io/dRiftDM/reference/cost_function.md).

- ... the PDFs of the first passage time; can be addressed via
  [`pdfs()`](https://bucky2177.github.io/dRiftDM/reference/pdfs.md).

If the model was estimated (which includes its evaluation), the list
will contain `estimate_info`. This entry contains a convergence flag
(`conv_flag`, logical) and the `optimizer` (a string).

Finally, if arbitrary R objects were passed via
[`ddm_opts()`](https://bucky2177.github.io/dRiftDM/reference/ddm_opts.md)
(to access these objects when evaluating the component functions) the
list will contain an entry `ddm_opts`.

Every model also has the attribute
[b_coding](https://bucky2177.github.io/dRiftDM/reference/b_coding.md),
which summarizes how the boundaries are labeled.

For `print.drift_dm()`, the supplied `drift_dm` object `x` (invisible
return).

## Details

To modify the entries of a model users can use the replacement methods
and the
[`modify_flex_prms()`](https://bucky2177.github.io/dRiftDM/reference/modify_flex_prms.md)
method (see also
[`vignette("dRiftDM", "dRiftDM")`](https://bucky2177.github.io/dRiftDM/articles/dRiftDM.md)
and
[`vignette("customize_ddms", "dRiftDM")`](https://bucky2177.github.io/dRiftDM/articles/customize_ddms.md)).

## See also

[`conds()`](https://bucky2177.github.io/dRiftDM/reference/conds.md),
[`flex_prms()`](https://bucky2177.github.io/dRiftDM/reference/flex_prms.md),
[`prms_solve()`](https://bucky2177.github.io/dRiftDM/reference/prms_solve.md),
[`solver()`](https://bucky2177.github.io/dRiftDM/reference/solver.md),
[`obs_data()`](https://bucky2177.github.io/dRiftDM/reference/obs_data.md),
[`comp_funs()`](https://bucky2177.github.io/dRiftDM/reference/comp_funs.md),
[`b_coding()`](https://bucky2177.github.io/dRiftDM/reference/b_coding.md),
[`coef()`](https://rdrr.io/r/stats/coef.html),
[`pdfs()`](https://bucky2177.github.io/dRiftDM/reference/pdfs.md)

## Examples

``` r
# Plain call, with default component functions -----------------------------
# create parameter and condition vectors
prms <- c(muc = 4, b = 0.5)
conds <- c("one", "two")

# then call the backbone function (note that we don't provide any component
# functions, so dRiftDM uses the default functions as documented in
# comp_funs())
my_model <- drift_dm(prms_model = prms, conds = conds, subclass = "example")
print(my_model)
#> Class(es) example, drift_dm
#> (model has not been estimated yet)
#> 
#> Parameter Values:
#>     muc   b
#> one   4 0.5
#> two   4 0.5
#> 
#> Parameter Settings:
#>     muc b
#> one   1 2
#> two   1 2
#> 
#> Deriving PDFS:
#>   solver: kfe
#>   values: sigma=1, t_max=3, dt=0.001, dx=0.001, nt=3000, nx=2000
#> 
#> Cost Function: neg_log_like
#> 
#> Observed Data: NULL
```
