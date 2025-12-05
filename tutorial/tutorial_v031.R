#' ---
#' title: "Reproducing dRiftDM Article Output - Package Version 0.3.1"
#' author: "Valentin Koob (bucky2177)"
#' date: "01.12.2025"
#' ---
#'
#' This document contains the code for dRiftDM's new tutorial, created alongside
#' its revision for the Journal of Statistical Software.

#+ setup, echo = FALSE
# to show a progress bar even in the html doc
options("knitr.in.progress" = NULL)


# -------------------------------------------------------------------------
#' # Getting Started

#' Load the package and choose a pre-built model.
#+ get_a_ddm
library("dRiftDM")
a_model <- dmc_dm() # the Diffusion Model for Conflict tasks (DMC)
print(a_model) # print the model to the console

#' Demonstration of basic accessor and replacement functions. Documentation
#' available via: `?coef.drift_dm`; `?prms_solve`; `?drift_dm`
#+ modify_a_ddm
coef(a_model) # the model parameters that are estimable
coef(a_model)["muc"] <- 3.2 # sets the value of muc to 3.2
coef(a_model)["b"] <- .5 # sets the value of b to 0.5

prms_solve(a_model) # the "solver settings" (= scaling and discretization)
prms_solve(a_model)["dt"] <- .005 # changes the time discretization
prms_solve(a_model)["t_max"] <- 2.5 # changes the maximum time

print(a_model) # see the changes


# -------------------------------------------------------------------------
#' # Exploring Model Behavior and Predictions
#' Simulate some traces (docu: `?simulate_traces`)
#+ sim_first_traces
set.seed(123) # for reproducible results
some_traces <- simulate_traces(a_model, k = 3) # simulate 3 traces
print(some_traces) # show in a formatted way
some_traces_no_noise <- simulate_traces(a_model, k = 1, sigma = 0) # no noise

#' Show the traces (docu: `?plot.traces_dm_list`)
#+ show_first_traces, fig.width = 4, fig.height = 4, dpi = 300, out.width="40%", fig.cap = ""
plot(some_traces, xlim = c(0, 0.4))
plot(some_traces_no_noise, xlim = c(0, 0.4))

#' Extra: Simulate traces with a finer discretization for dt
#+ show_finer_traces, fig.width = 4, fig.height = 4, dpi = 300, out.width="40%", fig.cap = ""
temp_model <- a_model
prms_solve(temp_model)["dt"] <- .001
finer_traces <- simulate_traces(temp_model, k = 3) # simulate 3 traces
plot(finer_traces, xlim = c(0, 0.4))


#' Calculate summary statistics (docu: `?calc_stats`)
#+ basic_stats
basic_stats <- calc_stats(object = a_model, type = "basic_stats")
print(basic_stats)

#+ densities
dens <- calc_stats(object = a_model, type = "densities")
print(dens, print_rows = 2L) # print_rows controls the output length

#' Plotting functions are available (except for `type = "basic_stats"`). See
#' `?plot.densities` for the documentation.
#+ plot_densities, fig.width = 4, fig.height = 4, dpi = 300, out.width="40%", fig.cap = ""
plot(dens, xlim = c(0, 2))

#' In the context of conflict tasks, conditional accuracy functions and delta
#' functions are common.
#+ cafs_deltas
cafs_deltas <- calc_stats(
  object = a_model,
  type = c("cafs", "delta_funs"), # we can request multiple statistics in one go
  minuends = "incomp", # for the delta function: contrast incompatible quantiles
  subtrahends = "comp" # against compatible quantiles
)

#' See also `?plot.stats_dm_list`, `?plot.cafs`, `?plot.delta_funs`
#+ plot_cafs_deltas, fig.width = 8, fig.height = 4, dpi = 300, out.width="80%", fig.cap = ""
par(mfrow = c(1, 2))
plot(cafs_deltas)

#' Many functions provided by `dRiftDM` return custom S3 objects, making it
#' easy to plot and summarize them. To get the true underlying data, we can
#' use the function `unpack_obj()`. For example, to get the raw density values,
#' we write:
#+ unpack_obj
raw_data <- unpack_obj(dens) # strips away all custom information ...
head(raw_data, 2) # and provides the raw underlying data.frame

# -------------------------------------------------------------------------
#' # Fitting and Validating a Model

# -------------------------------------------------------------------------
#' ## Estimation Basics and Fitting a Single Individual

#' Let's fit an example model (DMC in this case) to the data of one individual

#+ dmc_and_data
a_model <- dmc_dm() # get a fresh DMC model instance for the demonstration
data("dmc_synth_data") # (this one ships with dRiftDM)
head(dmc_synth_data) # contains the data for a single "individual"

#' Ensure the conditions of the data and model match
#+ conds
conds(a_model) # shows the model conditions; must match with ...
unique(dmc_synth_data$Cond) # ... the unique entries in the Cond column

#' Ensure the time domain of interest easily covers the maximum response time
#+ max_rt
prms_solve(a_model)["t_max"] # the maximum of the model's time interval ...
max(dmc_synth_data$RT) # ... easily covers the maximum RT of the data

#' Now let's call the estimation routine via `estimate_dm()`
#+ fit_go_1
set.seed(1) # for reproducability
fit_result <- estimate_dm(
  drift_dm_obj = a_model, # the model object
  obs_data = dmc_synth_data # the observed data (for one individual)
)

#' Try to avoid the warning by running Nelder-Mead longer
#+ fit_go_2
set.seed(1) # for reproducability

fit_result <- estimate_dm(
  drift_dm_obj = a_model, # the model object
  obs_data = dmc_synth_data, # the observed data (for one individual)
  control = list(maxit = 3000) # let the Nelder-Mead algorithm run longer
)
coef(fit_result)

#' Run the parameter estimation with Differential Evolution
#+ fit_go_de
l_u <- get_lower_upper(a_model) # get the search space ...
prms_lower <- l_u$lower # lower search boundary
prms_upper <- l_u$upper # upper search boundary
print(prms_lower)
print(prms_upper)

set.seed(1) # for reproducability
# this will run in about 20 seconds
fit_result <- estimate_dm(
  drift_dm_obj = a_model, # the model object
  obs_data = dmc_synth_data, # the observed data (for one individual)
  optimizer = "DEoptim", # the optimizer (Differential Evolution)
  lower = prms_lower, # the lower boundary of the parameter space
  upper = prms_upper # the upper boundary of the parameter space
)
coef(fit_result)


# -------------------------------------------------------------------------
#' ## Fitting Multiple Individuals

#+ data_ulrich_flanker
data("ulrich_flanker_data") # provided by dRiftDM
head(ulrich_flanker_data) # contains multiple individuals

# fresh model for this section
a_model <- dmc_dm()
l_u <- get_lower_upper(a_model)

set.seed(1)
# takes about 3 minutes
fit_result <- estimate_dm(
  drift_dm_obj = a_model, # model to fit
  obs_data = ulrich_flanker_data, # data.frame with IDs
  optimizer = "DEoptim", # optimizer (Differential Evolution)
  n_cores = 4, # number of cores to use
  lower = l_u$lower, # lower boundary of the parameter space
  upper = l_u$upper # upper boundary of the parameter space
)

print(fit_result)

#' Check the qualitative model fit
#+ model_fit_qual, fig.width = 8, fig.height = 4, dpi = 300, out.width="80%", fig.cap = ""
sum_stats <- calc_stats(
  fit_result, # the fit object
  type = c("cafs", "delta_funs"), # requested summary statistics
  minuends = "incomp", # for delta functions ...
  subtrahends = "comp", # -> incomp - comp
  level = "group", # get values averaged across individuals
  resample = TRUE # resample individuals for bootstrap
)
par(mfrow = c(1, 2))
plot(sum_stats)


#' Check the quantitative model fit
#+ fit_stats
calc_stats(
  fit_result, # the fit object
  type = "fit_stats", # request fit statistics
  level = "group" # get values averaged across individuals
)

#' Extract and plot the parameter estimates
#+ extract_coefs, fig.width = 8, fig.height = 8, dpi = 300, out.width="80%", fig.cap = ""
prms <- coef(fit_result)
head(prms)
hist(prms, breaks = 6)

# -------------------------------------------------------------------------
#' ## Fitting a Model to Aggregated Data

#+ fit_agg
# takes about 1 minute
fit_result <- estimate_dm(
  drift_dm_obj = a_model, # model to fit
  obs_data = ulrich_flanker_data, # data.frame with IDs
  optimizer = "DEoptim", # optimizer (Differential Evolution)
  lower = l_u$lower, # lower boundary of the parameter space
  upper = l_u$upper, # upper boundary of the parameter space
  approach = "agg_c" # aggregated data; classical minimization routine
)
coef(fit_result)


# -------------------------------------------------------------------------
#' ## Model Validation: Parameter and Model Recovery

#' We will now perform a parameter recovery. Let's create a model of interest
#' and generate synthetic data.

#+ model_for_recovery
# fresh model with fine discretization (to simulate data with high precision)
a_model <- dmc_dm()
prms_solve(a_model)["dx"] <- .001
prms_solve(a_model)["dt"] <- .001

# simulation space
lower_sim_bnd <- c(
  muc = 3,
  b = 0.35,
  non_dec = 0.275,
  sd_non_dec = .01,
  tau = 0.025,
  A = 0.05,
  alpha = 2.5
)
upper_sim_bnd <- c(
  muc = 6.5,
  b = 0.70,
  non_dec = 0.425,
  sd_non_dec = .04,
  tau = 0.200,
  A = 0.25,
  alpha = 6
)

# generate original parameter values and synthetic data
set.seed(1)
data_prms <- simulate_data(
  a_model, # the model
  n = 500, # how many trials per condition?
  k = 100, # how many data sets?
  lower = lower_sim_bnd, # the lower and
  upper = upper_sim_bnd # upper end of the simulation space
)
names(data_prms)

#' Next, we try to recover the parameters underlying the synthetic data and
#' quantify the recovery performance
#+ recovery
synth_data <- data_prms$synth_data # extract the synthetic data
a_model <- dmc_dm() # re-create the model for default dt/dx values
l_u <- get_lower_upper(a_model) # get or define a search space

# run the estimation (takes about 12 minutes)
set.seed(1)
fit_result <- estimate_dm(
  drift_dm_obj = a_model, # the model to fit
  obs_data = synth_data, # the synthetic data
  optimizer = "DEoptim", # choose DEoptim because it is more robust
  n_cores = 5, # 5 cores; use more or less, depending on your machine
  lower = l_u$lower, # lower bounds of the search space
  upper = l_u$upper # upper bounds of the search space
)

# quantify the recovery performance
recov_prms <- coef(fit_result) # the estimated parameter values
orig_prms <- data_prms$prms # the original parameter values

# correlations for each parameter
prms <- c("muc", "b", "non_dec", "sd_non_dec", "tau", "A", "alpha")
sapply(prms, function(one_prm) {
  cor_val <- cor(recov_prms[[one_prm]], orig_prms[[one_prm]])
  round(cor_val, 3)
})

# biases for each parameter
sapply(prms, function(one_prm) {
  # average difference ...
  mean_diff <- mean(recov_prms[[one_prm]] - orig_prms[[one_prm]])
  # relative to the range
  range <- diff(range(orig_prms[[one_prm]]))
  round(mean_diff / range, 3)
})


# -------------------------------------------------------------------------
#' ## Bayesian Parameter Estimation

#' Fit each individual separately (just one for demonstration purpose)

#+ sep_b
set.seed(1)
# takes about 2 minutes
mcmc_out <- estimate_dm(
  drift_dm_obj = a_model, # the model object
  obs_data = dmc_synth_data, # the data (just one individual here)
  approach = "sep_b" # fit individuals separately, using a Bayesian approach
)

summary(mcmc_out)

#' You can request diagnostic plots on the chains, see `?plot.mcmc_dm`.

#' Fit multiple individuals hierarchically (not yet fully reproducible!)
#+ hier_fit, eval = FALSE
# would take about 30 minutes
mcmc_out <- estimate_dm(
  drift_dm_obj = a_model, # the model to fit
  obs_data = ulrich_flanker_data, # a data set with multiple individuals
  approach = "hier_b", # choose hierarchical Bayesian estimation
  n_cores = 4 # number of cores (using more has little benefit for this example)
)
summary(mcmc_out)

# -------------------------------------------------------------------------
#' ## Remark: Discretization Settings

#+ check_discr
check_discretization(a_model)

#' The following code shows how to generate the table from the article
#+ table_check_discr
dmc_model <- dmc_dm()
dt <- c(0.001, 0.005, 0.01, 0.02)
dx <- c(0.001, 0.005, 0.01, 0.02)
grid <- expand.grid(dx = dx, dt = dt)

# helper function to calculate the hellinger distance for one dt/dx setting
eval_one <- function(dt_i, dx_i) {
  prms_solve(dmc_model)["dt"] <- dt_i
  prms_solve(dmc_model)["dx"] <- dx_i
  hell <- check_discretization(dmc_model)
  return(max(hell))
}

hells <- Map(eval_one, grid$dt, grid$dx)
hells <- round(unlist(hells), 3)
cbind(grid, hells)


# -------------------------------------------------------------------------
#' # Customize a Model: Using Flex_prms
#' Goal in this section: Introduce a neutral condition and demonstrate flex_prms
#' 1. Example: Restrain and free parameters across conditions
#+ flex_prms_1
my_dmc_model <- dmc_dm()
flex_prms(my_dmc_model)

instructions <- "
muc ~             # muc varies 'freely' across all conditions
non_dec <!> comp  # non_dec is 'fixed' for comp
"
my_dmc_model <- modify_flex_prms(my_dmc_model, instr = instructions)
flex_prms(my_dmc_model) # or just use print()


#' 2. Example: Create a neutral condition.
#+ flex_prms_2
# set new conditions to the model (this will reset parameter specifications)
my_dmc_model <- dmc_dm() # a fresh model
conds(my_dmc_model) <- c("comp", "neutral", "incomp")
flex_prms(my_dmc_model) # or just use print()

#' Now modify the new model to suit our needs.
#+ neutral_cond_1
instructions <- "
a <!>                      # a is 'fixed' across all conditions
A ~ incomp == -(A ~ comp)  # A in incomp conditions is
                           # -1 times A in comp conditions
A <!> neutral              # A is 'fixed' for neutral
A ~ neutral => 0           # sets A to zero for neutral
"

my_dmc_model <- modify_flex_prms(
  object = my_dmc_model,
  instr = instructions
)
flex_prms(my_dmc_model)


#' Check if it worked as expected
#+ plot_neutral_dmc, fig.width = 8, fig.height = 8, dpi = 300, out.width="80%", fig.cap = ""
plot(my_dmc_model, xlim = c(0, .4), legend.pos = "topleft")


# -------------------------------------------------------------------------
#' # Build a Model from Scratch

#' We can access each component function with `comp_funs()`
#+ comp_funs
names(comp_funs(a_model))

#' In the following, we re-implement DMC (for a = 2). To this end, we
#' first define the custom drift rate of this model, and then write a wrapper
#' around a function call to `drift_dm()`.
#+ dmc_code
# drift rate for DMC
mu_dmc <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
  # unpack values
  muc <- prms_model[["muc"]]
  tau <- prms_model[["tau"]]
  A <- prms_model[["A"]]

  # calculate the drift rate (for a = 2)
  mu <- muc + A / tau * exp(1 - t_vec / tau) * (1 - t_vec / tau)

  # and pass back
  return(mu)
}

# write a function for creating DMC
my_dmc_dm <- function() {
  # define parameters
  # - muc, tau, and A -> for the custom drift rate (with a = 2)
  # - alpha -> is required for the pre-built starting point distribution
  # - b -> for the pre-built boundary
  # - non_dec, sd_non_dec -> is required for the pre-built non-decision time
  prms_model <- c(
    muc = 4,
    tau = .04,
    A = .1,
    alpha = 4,
    b = .6,
    non_dec = .3,
    sd_non_dec = .02
  )

  # define conditions
  conds <- c("comp", "incomp")

  # get access to pre-built component functions
  comps <- component_shelf()

  # call the drift_dm() function which is the backbone of dRiftDM
  model <- drift_dm(
    prms_model = prms_model, # the model parameters
    conds = conds, # the conditions
    subclass = "my_dmc", #  a label for the model
    mu_fun = mu_dmc, # our custom drift rate function
    x_fun = comps$x_beta, # beta-shaped starting point distribution
    b_fun = comps$b_constant, # constant boundary b
    dt_b_fun = comps$dt_b_constant, # derivative of b
    nt_fun = comps$nt_truncated_normal # normally-distr. non-dec. time
  )

  # modify the underlying flex_prms object to achieve:
  # 'A' in incomp is the negative of 'A' in comp
  instructions <- "
  A ~ incomp == -(A ~ comp)
  "
  model <- modify_flex_prms(model, instr = instructions)

  return(model)
}

#' Call the function and explore some traces
#+ plot_dmc, fig.width = 4, fig.height = 4, dpi = 300, out.width="40%", fig.cap = ""
my_model <- my_dmc_dm()
some_traces_no_noise <- simulate_traces(my_model, k = 1, sigma = 0)
plot(some_traces_no_noise, xlim = c(0, 0.2))


# -------------------------------------------------------------------------
#' # Session Info
#+ session_info
sessionInfo()
