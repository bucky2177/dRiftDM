# Fit a DDM to Observed Data

`estimate_dm()` is the main function to fit a drift diffusion model
(DDM) in `dRiftDM`. Several ways of fitting a model are supported:
fitting a single participant, fitting multiple participants separately
or aggregated, and fitting a (hierarchical) Bayesian model. The
particular way is controlled via the `approach` argument.

## Usage

``` r
estimate_dm(
  drift_dm_obj,
  obs_data = NULL,
  approach = NULL,
  optimizer = NULL,
  control = list(),
  n_cores = 1,
  parallelization_strategy = NULL,
  lower = NULL,
  upper = NULL,
  start_vals = NULL,
  means = NULL,
  sds = NULL,
  shapes = NULL,
  rates = NULL,
  n_chains = 40,
  burn_in = 500,
  samples = 1000,
  prob_migration = 0.1,
  prob_re_eval = 1,
  messaging = TRUE,
  seed = NULL,
  ...
)

# S3 method for class 'fits_agg_dm'
print(x, ...)

# S3 method for class 'fits_ids_dm'
print(x, ...)

# S3 method for class 'mcmc_dm'
print(x, ..., round_digits = drift_dm_default_rounding())
```

## Arguments

- drift_dm_obj:

  a
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
  object containing the model to be fitted.

- obs_data:

  an optional [data.frame](https://rdrr.io/r/base/data.frame.html) (see
  also
  [obs_data](https://bucky2177.github.io/dRiftDM/reference/obs_data.md)).
  If no `ID` column is present, a single-individual setup is assumed. If
  an `ID` column is present, the model is fitted separately for each
  individual.

- approach:

  an optional character string, specifying the approach to fitting the
  model. Options are `"sep_c"`, `"agg_c"`, `"sep_b"`, `"hier_b"` (see
  the Details).

- optimizer:

  a character string. For classical optimization, one of `"nmkb"`,
  `"Nelder-Mead"`, `"BFGS"`, `"L-BFGS-B"`, `"DEoptim"`. For the Bayesian
  framework, only `"DE-MCMC"` is currently supported. If `NULL` and if a
  classical optimization approach is used, defaults to `"DEoptim"` or
  `"Nelder-Mead"`, depending on whether `lower/upper` are provided or
  not. If `NULL` and if a Bayesian framework is used, defaults to
  `"DE-MCMC`. Note that `"BFGS"` and `"L-BFGS-B"` are often unstable.

- control:

  a list of control parameters passed to the optimizer (for Nelder-Mead,
  BFGS, and L-BFGS-B, see
  [stats::optim](https://rdrr.io/r/stats/optim.html); for nmkb, see
  [dfoptim::nmkb](https://rdrr.io/pkg/dfoptim/man/nmkb.html); for
  DEoptim, see
  [DEoptim::DEoptim](https://rdrr.io/pkg/DEoptim/man/DEoptim.html)). Per
  default, we set the `trace` control argument for
  [DEoptim::DEoptim](https://rdrr.io/pkg/DEoptim/man/DEoptim.html) to
  `FALSE`. Also, we set the `parscale` control argument for
  "Nelder-Mead" via [stats::optim](https://rdrr.io/r/stats/optim.html)
  to `pmax(x0, 1e-6)`.

- n_cores:

  an integer \> 0, indicating the number of CPU cores/threads to use (at
  the moment, this doesn't have an effect when fitting a single
  individual within the Bayesian framework).

- parallelization_strategy:

  an integer, controlling how parallelization is performed when fitting
  multiple individuals with the classical approach. If `1`,
  parallelization is across individuals. If `2`, parallelization is
  within individuals (currently only supported for `"DEoptim"`).
  Defaults to `1`.

- lower, upper:

  numeric vectors or lists, specifying the lower and upper bounds on
  each parameter to be optimized (see Details).

- start_vals:

  optional starting values for classical single-subject fits and when
  using an optimizer that requires a starting value. Can be a numeric
  vector of model parameters when fitting a single individual, or a
  `data.frame` with columns for each model parameter. In the latter
  case, enables multi-start (one row per start). For
  `'approach = "separately"'`, a `data.frame` with an `ID` column is
  required.

- means, sds, shapes, rates:

  optional numeric vectors for prior specification (when using the
  Bayesian framework, see Details).

- n_chains:

  an integer, providing the number of MCMC chains (Bayesian framework).

- burn_in:

  an integer, number of burn-in iterations (Bayesian framework).

- samples:

  an integer, number of post-burn-in samples per chain ( Bayesian
  framework).

- prob_migration:

  a numeric in `[0,1]`, controlling the migration probability of the
  `DE-MCMC` algorithm (Bayesian framework).

- prob_re_eval:

  a numeric in `[0,1]`, probability to re-evaluate the model at current
  group-level parameters during sampling (Bayesian framework; only
  relevant for the hierarchical case).

- messaging:

  a logical, if `TRUE` progress/info messages are printed

- seed:

  an optional integer to set the RNG seed for reproducibility.

- ...:

  additional arguments forwarded to lower-level routines. Options are:
  `progress/verbose` (integers, for controlling progress bars and
  verbosity of estimation infos), `round_digits` (for controlling the
  number of digits for rounding when printing individual model
  evaluations; if `verbose = 2`), `return_runs` (when fitting a single
  individual and starting the estimation routine with multiple starting
  points; if `TRUE`, then a list of all routines is returned),
  `probs/n_bins` (the quantile levels and the number of CAF bins when
  fitting aggregated data using the RMSE cost function), `use_ez/n_lhs`
  (logical and integer; the first controls if EZ-Diffusion Parameter
  Estimates shall be used for determining starting points; the latter
  controls the number of parameters to sample per dimension for the
  latin hypercube sampling when searching for starting values)

- x:

  an object of type `fits_agg_dm`, `fits_ids_dm`, or `mcmc_dm`

- round_digits:

  integer, specifying the number of decimal places for rounding in the
  printed summary. Default is 3.

## Value

- If fitting a single individual: either a `drift_dm` object with fitted
  parameters and additional fit information (for the classical
  optimization framework) or an object of type `mcmc_dm` (for the
  Bayesian framework)

- If fitting multiple individuals separately: a `fits_ids_dm` object or
  a list of `mcmc_dm` objects, containing all the individual model fits.

- If fitting aggregated data: a `fits_agg_dm` object containing the
  model itself and the raw data.

- If fitting multiple individuals hierarchically: an object of type
  `mcmc_dm`.

## Details

### Fitting Approaches

The function supports different "approaches" to fitting data.

- `"sep_c"`: This means that data is always considered `sep`arately for
  each participant (if there are multiple participants) and that a
  `c`lassical approach to parameter optimization is used. This means
  that a standard
  [cost_function](https://bucky2177.github.io/dRiftDM/reference/cost_function.md)
  is minimized (e.g., the negative log-likelihood). If users provide
  only a single participant or a data set without an `ID` column, then
  the model is fitted just once to that data set.

- `"agg_c"`: This fits the model to aggregated data. For each individual
  in a data set, summary statistics (e.g., quantiles, accuracies) are
  calculated, and the model is fitted once to the average of these
  summary statistics.

- `"sep_b"`: Similar to `sep_b"`, although a Bayesian approach is used
  to sample from the posterior distribution.

- `"hier_b"`: A hierarchical approach to parameter estimation. In this
  case all participants are considered simultaneously and samples are
  drawn both at the individual-level and group-level.

The optimizers `"nmkb"`, `"L-BFGS-B"`, and `"DEoptim"` (for classical
parameter optimization) require the specification of the `lower/upper`
arguments.

### Fitting to Aggregated Data

For aggregated fits, aggregated statistics are set to the model and the
cost function is switched to `"rmse"`. If incompatible settings are
requested, the function switches to a compatible configuration and
informs the user with messages (these messages can be suppressed via the
`messaging` argument).

### Specifying `lower/upper` for Classical optimization

the function `estimate_model_dm()` provides a flexible way of specifying
the optimization space; this is identical to specifying the parameter
simulation space in
[`simulate_data.drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/simulate_data.md).

Users have three options to specify the search space (see also the
examples below):

- Plain numeric vectors (not very much recommended). In this case,
  `lower/upper` must be sorted in accordance with the parameters in the
  underlying
  [flex_prms](https://bucky2177.github.io/dRiftDM/reference/flex_prms.md)
  object of `drift_dm_obj` that vary for at least one condition (call
  `print(drift_dm_obj)` and have a look at the columns of the
  `Parameter Settings` output; for each column that has a number \> 0,
  specify an entry in `lower/upper`).

- Named numeric vectors. In this case `lower/upper` have to provide
  labels in accordance with the parameters that are considered "free" at
  least once across conditions (call `coef(drift_dm_obj)` and provide
  one named entry for each parameter; dRiftDM will try to recycle
  parameter values across conditions).

- The most precise way is when `lower/upper` are lists. In this case,
  the list requires an entry called "default_values" which specifies the
  named or plain numeric vectors as above. If the list only contains
  this entry, then the behavior is as if `lower/upper` were already
  numeric vectors. However, the `lower/upper` lists can also provide
  entries labeled as specific conditions, which contain named (!)
  numeric vectors with parameter labels. This will modify the value for
  the upper/lower parameter space with respect to the specified
  parameters in the respective condition.

### Specifying Priors for Bayesian Estimation

**(Default) Prior settings in the non-hierarchical case:**

Let \\\theta^{(j)}\\ indicate parameter \\j\\ of a model (e.g., the
drift rate). The prior on \\\theta^{(j)}\\ is a truncated normal
distribution: \$\$ \theta^{(j)} \sim NT(\mu^{(j)}, \sigma^{(j)},
l^{(j)}, u^{(j)}) \$\$ With \\\mu^{(j)}\\ and \\\sigma^{(j)}\\
representing the mean and standard deviation of parameter \\j\\.
\\l^{(j)}\\ and \\u^{(j)}\\ represent the lower and upper boundary.
\\\mu^{(j)}\\ is taken from the `mean` argument or the currently set
model parameters (i.e., from `coef(drift_dm_obj)`) when calling the
function. \\\sigma^{(j)}\\ is, per default, equal to \\\mu^{(j)}\\. This
can be changed by passing the `sd` argument. The lower and upper
boundaries of the truncated normal are `-Inf` and `Inf` per default.
This can be altered by passing the arguments `lower` and `upper` (see
the examples below).

**(Default) Prior settings in the hierarchical case:**

Let \\\theta_i^{(j)}\\ indicate parameter \\j\\ for participant \\i\\
(e.g., the drift rate estimated for individual \\i\\). The prior on
\\\theta_i^{(j)}\\ is a truncated normal distribution: \$\$
\theta_i^{(j)} \sim NT(\mu^{(j)}, \sigma^{(j)}, l^{(j)}, u^{(j)}) \$\$
With \\\mu^{(j)}\\ and \\\sigma^{(j)}\\ representing the mean and
standard deviation of parameter \\j\\ at the group level. \\l^{(j)}\\
and \\u^{(j)}\\ represent the lower and upper boundary. The lower and
upper boundaries of the truncated normal are `-Inf` and `Inf` per
default. This can be altered by passing the arguments `lower` and
`upper`.

For a group-level mean parameter, \\\mu^{(j)}\\, the prior is also a
truncated normal distributions: \$\$ \mu^{(j)} \sim NT(M^{(j)},
SD^{(j)}, l^{(j)}, u^{(j)}) \$\$ With \\M^{(j)}\\ specified by the
`mean` argument or the currently set model parameters. \\SD^{(j)}\\ is,
per default, equal to \\M^{(j)}\\. This can be changed by passing the
`sd` argument.

For a group-level standard deviation parameter, \\\sigma^{(j)}\\, the
prior is a gamma distribution: \$\$ \sigma^{(j)} \sim
\Gamma(shape^{(j)},rate^{(j)}) \$\$ With \\shape^{(j)}\\ and
\\rate^{(j)}\\ being `1` by default. This can be changed by passing the
arguments `shape` and `rate`.

**Specifying Prior Settings/Arguments**

Argument specification for `mean`, `sd`, `lower`, `upper`, `shape` and
`rate` is conceptually identical to specifying `lower/upper` for the
classical optimization approach (see the subsection above and the
examples below).

## Note

`estimate_dm` dispatches to underlying estimation routines that are not
exported:

- Classical optimization of one individual via
  [`estimate_classical()`](https://bucky2177.github.io/dRiftDM/reference/estimate_classical.md)

- Classical optimization of multiple individuals via
  [`estimate_classical_wrapper()`](https://bucky2177.github.io/dRiftDM/reference/estimate_classical_wrapper.md)

- Bayesian estimation via
  [`estimate_bayesian()`](https://bucky2177.github.io/dRiftDM/reference/estimate_bayesian.md).

- Aggregated fitting is handled within `estimate_dm()` in combination
  with
  [`estimate_classical()`](https://bucky2177.github.io/dRiftDM/reference/estimate_classical.md)

When fitting a model with `optimizer = "DEoptim"`, the corresponding
minimization routine always runs for 200 iterations by default,
irrespective of whether a minimum has already been reached (see
[DEoptim::DEoptim.control](https://rdrr.io/pkg/DEoptim/man/DEoptim.control.html)).
Therefore, with default optimization settings, `estimate_dm()` returns
the convergence flag `NA` for `optimizer = "DEoptim"`, because the
termination of the routine does not necessarily indicate convergence.
However, this is typically not an issue, as 200 iterations are generally
sufficient for the algorithm to find the global minimum. If users
explicitly define convergence criteria via the `control` argument of
`estimate_dm()` (which is passed on to
[DEoptim::DEoptim.control](https://rdrr.io/pkg/DEoptim/man/DEoptim.control.html)),
valid convergence messages and flags are returned.

## See also

[`estimate_classical()`](https://bucky2177.github.io/dRiftDM/reference/estimate_classical.md),
[`estimate_bayesian()`](https://bucky2177.github.io/dRiftDM/reference/estimate_bayesian.md),
[`estimate_classical_wrapper()`](https://bucky2177.github.io/dRiftDM/reference/estimate_classical_wrapper.md),
[`get_parameters_smart()`](https://bucky2177.github.io/dRiftDM/reference/get_parameters_smart.md)

## Examples

``` r
##########
# Note: The following examples were trimmed for speed to ensure they run
# within seconds. They do not always provide realistic settings.
##########

####
# Setup

# get a model for the examples (DMC with just two free parameters)
model <- dmc_dm(
  instr = "
   b <!>
   non_dec <!>
   sd_non_dec <!>
   tau <!>
   alpha <!>
   "
)

# get some data (the first two participants in the data set of Ulrich et al.)
data <- ulrich_flanker_data[ulrich_flanker_data$ID %in% 1:2, ]


####
# Fit a single individual (using unbounded Nelder-Mead)
fit <- estimate_dm(
  drift_dm_obj = model,
  obs_data = data[data$ID == 1, ],
  optimizer = "Nelder-Mead"
)
#> Using the data supplied via the 'obs_data' argument.
#> Using optimizer 'Nelder-Mead'.
#> Fitting a single data set/participant (cost function: 'neg_log_like'). The returned object will be the model itself.
#> Using EZ-Diffusion estimates for: muc
#> Performing latin hypercube sampling (n_lhs = 10) on: A
#> Starting optimizer 'Nelder-Mead' with the following starting values:
#> muc=3.868, A=0.076
#> Optimization routine exited after 91 function evaluations
#> Final Parameters:
#> muc = 4.608
#> A = 0.081
#> ==> gave a neg_log_like of -368.96
print(fit)
#> Class(es) dmc_dm, drift_dm
#> Optimizer: Nelder-Mead
#> Convergence: TRUE
#> 
#> Parameter Values:
#>          muc   b non_dec sd_non_dec  tau a      A alpha
#> comp   4.608 0.6     0.3       0.02 0.04 2  0.081     4
#> incomp 4.608 0.6     0.3       0.02 0.04 2 -0.081     4
#> 
#> Parameter Settings:
#>        muc b non_dec sd_non_dec tau a A alpha
#> comp   1   0 0       0          0   0 2 0    
#> incomp 1   0 0       0          0   0 d 0    
#> 
#> Special Dependencies:
#> A ~ incomp == -(A ~ comp)
#> 
#> Custom Parameters:
#>        peak_l
#> comp     0.04
#> incomp   0.04
#> 
#> Deriving PDFS:
#>   solver: kfe
#>   values: sigma=1, t_max=3, dt=0.0075, dx=0.02, nt=400, nx=100
#> 
#> Cost Function: neg_log_like
#> 
#> Observed Data: 168 trials comp; 168 trials incomp
#> 


####
# Fit a single individual (using bounded Nelder-Mead and custom starting
# values)
l_u <- get_lower_upper(model)
fit <- estimate_dm(
  drift_dm_obj = model,
  obs_data = data[data$ID == 1, ],
  optimizer = "nmkb",
  lower = l_u$lower, upper = l_u$upper,
  start_vals = c(muc = 4, A = 0.06)
)
#> Using the data supplied via the 'obs_data' argument.
#> Using optimizer 'nmkb'.
#> Fitting a single data set/participant (cost function: 'neg_log_like'). The returned object will be the model itself.
#> Starting optimizer 'nmkb' with the following starting values:
#> muc=4, A=0.06
#> Optimization routine exited after 94 function evaluations
#> Final Parameters:
#> muc = 4.608
#> A = 0.081
#> ==> gave a neg_log_like of -368.96
print(fit)
#> Class(es) dmc_dm, drift_dm
#> Optimizer: nmkb
#> Convergence: TRUE
#> 
#> Parameter Values:
#>          muc   b non_dec sd_non_dec  tau a      A alpha
#> comp   4.608 0.6     0.3       0.02 0.04 2  0.081     4
#> incomp 4.608 0.6     0.3       0.02 0.04 2 -0.081     4
#> 
#> Parameter Settings:
#>        muc b non_dec sd_non_dec tau a A alpha
#> comp   1   0 0       0          0   0 2 0    
#> incomp 1   0 0       0          0   0 d 0    
#> 
#> Special Dependencies:
#> A ~ incomp == -(A ~ comp)
#> 
#> Custom Parameters:
#>        peak_l
#> comp     0.04
#> incomp   0.04
#> 
#> Deriving PDFS:
#>   solver: kfe
#>   values: sigma=1, t_max=3, dt=0.0075, dx=0.02, nt=400, nx=100
#> 
#> Cost Function: neg_log_like
#> 
#> Observed Data: 168 trials comp; 168 trials incomp
#> 


####
# Fit a single individual (using DEoptim)
# Note: DEoptim always runs for 200 iterations per default; which is not
# necessary here -> in this simple example, we stop it after 10 iterations
# without improvement
l_u <- get_lower_upper(model)
set.seed(2)
fit <- estimate_dm(
  drift_dm_obj = model,
  obs_data = data[data$ID == 1, ],
  optimizer = "DEoptim",
  lower = l_u$lower, upper = l_u$upper,
  control = list(steptol = 10)
)
#> Using the data supplied via the 'obs_data' argument.
#> Using optimizer 'DEoptim'.
#> Fitting a single data set/participant (cost function: 'neg_log_like'). The returned object will be the model itself.
#> Starting optimizer 'DEoptim' 
#> Optimization routine exited after 65 iterations.
#> Final Parameters:
#> muc = 4.608
#> A = 0.081
#> ==> gave a neg_log_like of -368.96
print(fit)
#> Class(es) dmc_dm, drift_dm
#> Optimizer: DEoptim
#> Convergence: TRUE
#> 
#> Parameter Values:
#>          muc   b non_dec sd_non_dec  tau a      A alpha
#> comp   4.608 0.6     0.3       0.02 0.04 2  0.081     4
#> incomp 4.608 0.6     0.3       0.02 0.04 2 -0.081     4
#> 
#> Parameter Settings:
#>        muc b non_dec sd_non_dec tau a A alpha
#> comp   1   0 0       0          0   0 2 0    
#> incomp 1   0 0       0          0   0 d 0    
#> 
#> Special Dependencies:
#> A ~ incomp == -(A ~ comp)
#> 
#> Custom Parameters:
#>        peak_l
#> comp     0.04
#> incomp   0.04
#> 
#> Deriving PDFS:
#>   solver: kfe
#>   values: sigma=1, t_max=3, dt=0.0075, dx=0.02, nt=400, nx=100
#> 
#> Cost Function: neg_log_like
#> 
#> Observed Data: 168 trials comp; 168 trials incomp
#> 


####
# Fit multiple individuals (separately; using bounded Nelder-Mead)
l_u <- get_lower_upper(model)
fit <- estimate_dm(
  drift_dm_obj = model,
  obs_data = data, # contains the data for two individuals
  optimizer = "nmkb",
  lower = l_u$lower, upper = l_u$upper,
)
#> Using the data supplied via the 'obs_data' argument.
#> Using optimizer 'nmkb'.
#> Fitting the model separately to multiple participants (cost function:'neg_log_like'). The result will be a fit object of type 'fits_ids_dm'.
print(fit)
#> Fit approach: separately - classical
#> Fitted model type: dmc_dm, drift_dm
#> Optimizer: nmkb 
#> Convergence: TRUE 
#> N Individuals: 2 
#> Average Trial Numbers:
#>  168 trials comp; 168 trials incomp
#> Cost Function: neg_log_like
coef(fit)
#> Object Type: coefs_dm
#> 
#>   ID   muc     A
#> 1  1 4.595 0.081
#> 2  2 6.843 0.114
#> 
#> (access the data.frame's columns/rows as usual)


###
# Fit to aggregated data (using unbounded Nelder-Mead)
fit <- estimate_dm(
  drift_dm_obj = model,
  obs_data = data, # contains data for two individuals
  optimizer = "Nelder-Mead",
  approach = "agg_c"
)
#> Using the data supplied via the 'obs_data' argument.
#> Using optimizer 'Nelder-Mead'.
#> Changing the 'cost_function' to 'rmse'.
#> Aggregated data has been set to the model.
#> Fitting the model once to the aggregated data. The returned object will of type 'fits_agg_dm'.
#> Performing latin hypercube sampling (n_lhs = 10) on: muc, A
#> Starting optimizer 'Nelder-Mead' with the following starting values:
#> muc=5.843, A=0.222
#> Optimization routine exited after 63 function evaluations
#> Final Parameters:
#> muc = 5.549
#> A = 0.164
#> ==> gave a rmse of 0.025
print(fit)
#> Fit approach: aggregated - classical
#> Fitted model type: dmc_dm, drift_dm
#> Optimizer: Nelder-Mead
#> Convergence: TRUE
#> N Individuals: 2 
#> Average Trial Numbers:
#>  168 trials comp; 168 trials incomp
coef(fit)
#>       muc         A 
#> 5.5490172 0.1639635 


###
# EXPERIMENTAL
# Fit a single individual (using DE-MCMC; Bayesian; custom priors)
fit <- estimate_dm(
  drift_dm_obj = model,
  obs_data = data[data$ID == 1, ],
  approach = "sep_b",
  burn_in = 2, # this is usually way higher
  samples = 2, # this too
  n_chains = 10, # this too
  mean = c(muc = 3, A = 0.9),
  sd = c(muc = 2, A = 0.8),
)
#> Using the data supplied via the 'obs_data' argument.
#> Using optimizer 'DE-MCMC'.
#> Fitting a single data set/participant using the Bayesian framework. The result will be a fit object of type 'mcmc_dm'.
#> Finding starting values...
#> Starting the sampling procedure
print(fit)
#> Sampler: DE-MCMC 
#> Hierarchical: FALSE 
#> No. Parameters: 2 
#> No. Chains: 10 
#> Iterations Per Chain: 2 
coef(fit)
#>       muc         A 
#> 4.0081676 0.1581212 


###
# EXPERIMENTAL
# Fit multiple individuals (using DE-MCMC; hierarchical Bayesian)
fit <- estimate_dm(
  drift_dm_obj = model,
  approach = "hier_b",
  obs_data = data, # contains data for two individuals
  burn_in = 2, # this is usually way higher
  samples = 2, # this too
  n_chains = 10 # this too
)
#> Using the data supplied via the 'obs_data' argument.
#> Using optimizer 'DE-MCMC'.
#> Fitting the model hierarchically using a Bayesian framework. The result will be a fit object of type 'mcmc_dm'
#> Finding starting values...
#> Starting the sampling procedure
print(fit)
#> Sampler: DE-MCMC 
#> Hierarchical: TRUE 
#> No. Group-Level Parameters: 4 
#> No. Chains: 10 
#> Iterations Per Chain: 2 
coef(fit)
#>     M-muc     S-muc       M-A       S-A 
#> 3.9463775 2.2222783 0.0522379 1.4335086 
```
