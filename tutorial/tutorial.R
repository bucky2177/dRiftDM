#' ---
#' title: "Reproducing the dRiftDM Introduction"
#' author: "Valentin Koob (bucky)"
#' date: "28.12.2024"
#' ---
#'
#' This document contains the code of dRiftDM's tutorial to facilitate review.
#' Note that it sometimes provides additional R statements that were skipped
#' in the pre-print/manuscript for brevity. Yet, those primarily show
#' alternative ways to access information or simple plots.

rm(list = ls())


# -------------------------------------------------------------------------
#' # Getting Started

#' Load the package and choose a pre-built model.
#+ get_a_ddm
library(dRiftDM)
a_model <- ratcliff_dm() # a basic DDM (the Ratcliff DDM)
print(a_model) # print the model to the console

#' Demonstration of basic accessor and replacement functions. Documentation
#' available via: `?coef.drift_dm`; `?prms_solve`; `?solver`; `?drift_dm`
#+ modify_a_ddm
coef(a_model) # the unique model parameters
coef(a_model)["muc"] <- 3.2 # sets the value of muc to 3.2
coef(a_model)["b"] <- 0.5 # sets the value of b to 0.5

prms_solve(a_model) # the "solver settings" (= scaling and discretization)
prms_solve(a_model)[c("t_max", "dt")] <- c(2, 0.005) # sets the time space

print(a_model) # see the changes



# -------------------------------------------------------------------------
#' # Exploring Model Behavior and Predictions
#' Simulate some traces.
#+ sim_first_traces
set.seed(123) # for reproducible results
some_traces <- simulate_traces(a_model, k = 3) # simulates k = 3 "traces"
print(some_traces) # show in a formatted way
some_traces_no_noise <- simulate_traces(a_model, k = 1, sigma = 0) # no noise

#' Show the traces (see also `?plot.traces_dm_list`)
#+ show_first_traces, fig.width = 8, fig.height = 4.5
par(mfrow = c(1, 2)) # to plot the traces with and without noise side-by-side
plot(
  some_traces, # the traces object
  col = "gray30", # the desired color for each trace
  xlim = c(0, 0.4) # controls the x-axis limit
)
plot(some_traces_no_noise, col = "gray30", xlim = c(0, 0.4))


#' Calculate summary statistics. For speed, evaluate the model before
#' calculating the statistics.
#+ pdfs_sum_stats
a_model <- re_evaluate_model(a_model)

# then calculate summary statistics
some_stats <- calc_stats(a_model, type = c("cafs", "quantiles"))
print(some_stats)

#' Plot the summary statistics (see also `?plot.stats_dm_list`; `?plot.cafs`;
#' `?plot.quantiles`)
#+ fig.width = 8, fig.height = 4.5
plot(some_stats, mfrow = c(1, 2), col = "black")



# -------------------------------------------------------------------------
#' # Change Solver
#' "kfe" is the default solver, but users can change this to the
#' integral approach for models that have dirac delta on zero for the starting
#' point
#+ change_solver
solver(a_model)
solver(a_model) <- "im_zero" # use the method based on integral equations
print(a_model)



# -------------------------------------------------------------------------
#' # Set and fit a single data set
#' Get a model and set some data
#+ set_data
# create a model
a_model <- dmc_dm() # creates the Diffusion Model for Conflict Tasks
print(a_model) # note the special dependencies and custom parameters

# set some data
head(dmc_synth_data) # available with dRiftDM
obs_data(a_model) <- dmc_synth_data
print(a_model)

#' Increase discretization for speed and then estimate the model using (bounded)
#' Nelder-Mead
#+ estimate_one_data_set_nmkb
# set the discretization
# t_max -> should be large enough to easily cover even the slowest observed RTs
# dt, dx -> depend on the model; no recommendation yet
prms_solve(a_model)[c("t_max", "dt", "dx")] <- c(1.5, .0025, .0025)

# create the search space -> defined this way, the lower and upper search
# space will be the same for all conditions (see ?estimate_model for more
# details on how to specify the search space)
lower_prm_bnd <- c(
  muc = 2, b = 0.3, non_dec = 0.2, sd_non_dec = 0.01,
  tau = 0.02, A = 0.02, alpha = 3
)
upper_prm_bnd <- c(
  muc = 6, b = 0.8, non_dec = 0.4, sd_non_dec = 0.06,
  tau = 0.15, A = 0.20, alpha = 7
)

# estimate the model with (bounded) Nelder-Mead
a_model <- estimate_model( # takes about 30 seconds
  drift_dm_obj = a_model, # the model (i.e., DMC)
  lower = lower_prm_bnd, # the lower end of the search space
  upper = upper_prm_bnd, # the upper end of the search space
  use_de_optim = F, # don't use the (default) DE algorithm
  use_nmkb = T # but use (bounded) Nelder-Mead
)

round(coef(a_model), 3)

#' Repeat the same, but now use Differential Evolution
#+ estimate_one_data_set_de
# uses 2 cores or less (if not available)
# users can increase this number (e.g., to about 6)
n_cores <- pmin(parallel::detectCores(), 2)

# use DE algorithm with multiple cores
a_model <- estimate_model( # takes about 12 minutes with 2 cores
  drift_dm_obj = a_model,
  lower = lower_prm_bnd,
  upper = upper_prm_bnd,
  seed = 1,
  de_n_cores = n_cores
)

#' Investigate results
#+ calc_stats_single_fit
summary(a_model)

# extract fit statistics
# gives log-likelihood, AIC, and BIC (see also ?logLik.drift_dm, ?AIC, ?BIC)
calc_stats(a_model, type = "fit_stats")

# calculate summary statistics
sum_stats <- calc_stats(
  a_model, # model
  type = c("cafs", "quantiles", "delta_funs"), # requested summary statistics
  minuends = "incomp", # for delta functions ...
  subtrahends = "comp" # -> incomp - comp
)

#' Plot the summary statistics
#+ plot_stats_single_fit, fig.width = 8, fig.height = 4
# separate plot calls for more control over each panel
plot(sum_stats$cafs,
  col = c("green", "red"),
  legend = NA
) # legend = NA -> no legend
plot(sum_stats$quantiles, col = c("green", "red"))
plot(sum_stats$delta_funs)



# -------------------------------------------------------------------------
#' # Set and Fit Multiple Data Sets
#' A data set with multiple participants
#+
large_dat <- ulrich_flanker_data # provided by dRiftDM
head(large_dat)

#' Fit a model to each participant
#+ fit_multiple_ids
# fresh model without data
a_model <- dmc_dm(t_max = 1.5, dx = .0025, dt = .0025)

# now call the fit procedure.
# we'll write each fit into the working directory
estimate_model_ids( # takes about 10-15 minutes
  drift_dm_obj = a_model, # the model to fit ...
  obs_data_ids = large_dat, # to each participant in large_dat
  lower = lower_prm_bnd, # lower boundary of the search space
  upper = upper_prm_bnd, # upper boundary of the search space
  fit_path = getwd() ,   # write in the working directory
  fit_procedure_name = "ulrich_flanker", # a label for the fit procedure
  use_de_optim = F, # DE is default
  use_nmkb = T # but use Nelder-Mead (faster; for the tutorial)
)

#' See the folder structure
#+ folder_str
# fit procedure structure (saved within working directory; getwd())
list.dirs()
list.files(file.path(getwd(), "./drift_dm_fits/ulrich_flanker/"))

#' Load all fits to investigate model fit and the parameter estimates
#+ load_plot_multiple_ids, fig.width = 8, fig.height = 4
# load a fit procedure
all_fits <- load_fits_ids(
  fit_procedure_name = "ulrich_flanker",
)
print(all_fits)

# access parameters
all_prms <- coef(all_fits) # returns data.frame of all prms across IDs
head(all_prms, 2)

# investigate model fit (as above)
sum_stats <- calc_stats(
  all_fits,
  type = c("cafs", "quantiles", "delta_funs"),
  minuends = "incomp",
  subtrahends = "comp"
)

# plot model fit
plot(sum_stats$cafs, col = c("green", "red"), legend = NA)
plot(sum_stats$quantiles, col = c("green", "red"))
plot(sum_stats$delta_funs)


#' # A Simple Parameter Recovery Study
#' Get the Ratcliff DDM as a dummy model for demonstration purposes.
#+
a_model <- ratcliff_dm(t_max = 2)

#' Generate the original parameters and simulate synthetic data
#+ gen_synth_data
# define a simulation space
lower_sim_bnd <- c(muc = 2, b = 0.3, non_dec = 0.2)
upper_sim_bnd <- c(muc = 6, b = 0.8, non_dec = 0.4)

# draw parameter values and simulate synthetic data
set.seed(1)
data_prms <- simulate_data(
  a_model, # the model
  n = 100, # how many trials per data set?
  k = 50, # how many data sets?
  lower = lower_sim_bnd, # the lower and
  upper = upper_sim_bnd # upper simulation space
)

synth_data <- data_prms$synth_data # extract the synthetic data
head(synth_data)

orig_prms <- data_prms$prms # the parameter underlying the synth. data
head(orig_prms)

#' Fit the model to the synthetic data
#+ fit_prm_recovery
# set discretization for the recovery
prms_solve(a_model)[c("dx", "dt")] <- c(.005, .005)

# fit the model to each synthetic data
n_cores <- pmin(parallel::detectCores(), 2) # use 2 cores (if available)

estimate_model_ids( # takes about 30 minutes with 2 cores
  drift_dm_obj = a_model, # the model to fit
  obs_data_ids = synth_data, # the synthetic data
  lower = lower_sim_bnd, # the lower search space
  upper = upper_sim_bnd, # the upper search space
  fit_path = getwd(),    # save to working directory
  fit_procedure_name = "ratcliff_recovery", # a label for the fit procedure
  de_n_cores = n_cores, # the number of cores
  seed = 2 # a seed for reproducible results
)

#' Load the model fits and extract parameters to calculate correlations and
#' biases.
#+ summarize_prm_recovery
recov_fits <- load_fits_ids(fit_procedure_name = "ratcliff_recovery")
recov_prms <- coef(recov_fits) # extracts parameters
stopifnot(recov_prms$ID == orig_prms$ID) # ensure that the order of IDs matches

# correlations
cor(recov_prms$muc, orig_prms$muc)
cor(recov_prms$b, orig_prms$b)
cor(recov_prms$non_dec, orig_prms$non_dec)

# biases
mean(recov_prms$muc - orig_prms$muc) /
  diff(range(orig_prms$muc))
mean(recov_prms$b - orig_prms$b) /
  diff(range(orig_prms$b))
mean(recov_prms$non_dec - orig_prms$non_dec) /
  diff(range(orig_prms$non_dec))



# -------------------------------------------------------------------------
#' # Customize a Model: Using Flex_prms
#' Goal in this section: Introduce a neutral condition and demonstrate flex_prms
#' 1. Example: Restrain and free parameters across conditions
#+ flex_prms_1
my_dmc_model <- dmc_dm()
flex_prms(my_dmc_model)
my_dmc_model <- modify_flex_prms(
  my_dmc_model,
  instr = "muc ~
           non_dec <!> comp"
)
print(my_dmc_model)


#' 2. Example: Create a neutral condition. Requires to create a new flex_prms
#' object and to swap it in.
#+ flex_prms_2
# create a new flex_prms object
# requires the parameters of the model and the conditions
dmc_prms <- c(
  muc = 4, b = 0.6, non_dec = 0.3, sd_non_dec = 0.02, tau = 0.04,
  a = 2, A = 0.1, alpha = 4
)
new_flex_prms <- flex_prms(dmc_prms, conds = c("comp", "neutral", "incomp"))
print(new_flex_prms)

#' Modify the new flex_prms object to suit our needs.
#+
# 1.) fix a so that it is not estimated
# 2.) set A = 0 for neutral condition and keep it fixed
# 3.) set A negative for incomp condition
# -> can be done via a set of "instructions" (see ?modify_flex_prms)
instructions <- "
a <!>             # a is 'fixed' across all conditions
A ~ incomp == -(A ~ comp)  # A in incomp conditions is
                           # -1 times A in comp conditions
A <!> neutral     # A is 'fixed' for neutral
A ~ neutral => 0  # sets A to zero for neutral
"

new_flex_prms <- modify_flex_prms(
  object = new_flex_prms,
  instr = instructions
)

print(new_flex_prms)

#' Now swap in the new flex_prms object and visualize the results
#+ plot_neutral_dmc, fig.width = 10, fig.height = 10
flex_prms(my_dmc_model) <- new_flex_prms
print(my_dmc_model)

# visualize the results
plot(my_dmc_model,
  xlim = c(0, 0.5),
  col = c("green", "orange", "red")
)


# -------------------------------------------------------------------------
#' # Build a Model from Scratch: SPP
#' Here, we create the Shrinking Spotlight Model. To this end, we first
#' define the custom drift rate of this model, and then write a wrapper around
#' a function call to `drift_dm()`.
#+ ssp_code
# drift rate for SSP
mu_ssp <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
  # extract all parameters (one row of the parameter matrix)
  p <- prms_model[["p"]]
  sd_0 <- prms_model[["sd_0"]]
  r <- prms_model[["r"]]
  sign <- prms_model[["sign"]] # this will be controlled via flex_prms


  # calculate the standard deviation at each time step
  sd_t <- pmax(sd_0 - r * t_vec, 0.001)

  # calculate attentional weights
  a_tar_t <- pnorm(q = 0.5, mean = 0, sd = sd_t) -
    pnorm(q = -0.5, mean = 0, sd = sd_t)
  a_fl_t <- 1 - a_tar_t

  # pass back the drift rate, depending on the condition
  mu_t <- a_tar_t * p + sign * a_fl_t * p
  return(mu_t)
}

# write a function for creating the complete SSP
ssp_dm <- function(obs_data = NULL, sigma = 1, t_max = 3, dt = .001,
                   dx = .001) {
  # define parameters and conditions
  prms_model <- c(p = 3.3, sd_0 = 1.2, r = 10, b = .6, non_dec = .2, sign = 1)
  conds <- c("comp", "incomp")

  # provides access to pre-build component functions
  comps <- component_shelf()

  # call the drift_dm function which is the backbone of dRiftDM
  ssp_dm <- drift_dm(
    prms_model = prms_model,
    conds = conds,
    subclass = "ssp",
    obs_data = obs_data,
    sigma = sigma,
    t_max = t_max,
    dt = dt,
    dx = dx,
    mu_fun = mu_ssp, # custom defined
    mu_int_fun = comps$dummy_t, # no integral of drift
    x_fun = comps$x_dirac_0, # dirac delta on zero
    b_fun = comps$b_constant, # constant boundary b
    dt_b_fun = comps$dt_b_constant, # derivative of b
    nt_fun = comps$nt_constant # constant non-decision time non_dec
  )

  # modify the flex_prms object to achieve the desired behavior of 'sign'
  # -> don't consider 'sign' a free parameter to estimate and set it to -1
  # for incompatible conditions
  ssp_dm <- modify_flex_prms(ssp_dm, instr = "sign <!>
                                             sign ~ incomp => -1")

  return(ssp_dm)
}


#' Call the function and explore some traces
#+ plot_ssp, fig.width = 5, fig.height = 4
my_ssp <- ssp_dm()
print(my_ssp)

# simulate expected time course
set.seed(1)
some_traces_no_noise <- simulate_traces(my_ssp, k = 1, sigma = 0)

plot(some_traces_no_noise, col = c("green", "red"))




# -------------------------------------------------------------------------
#' # Build a Custom Model 2: Collapsing Boundaries and Custom Starting Point
#' Here, we want to implement a model with collapsing boundaries according to a
#' hyperbolic ratio function. Furthermore, we want to have a starting point
#' variability that matches with a centered beta distribution.
#+ coll_b_start
# the boundary function
cust_b <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
  b0 <- prms_model[["b0"]]
  kappa <- prms_model[["kappa"]]
  t05 <- prms_model[["t05"]]


  return(b0 * (1 - kappa * t_vec / (t_vec + t05)))
}

# the derivative of the boundary function
cust_dt_b <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
  b0 <- prms_model[["b0"]]
  kappa <- prms_model[["kappa"]]
  t05 <- prms_model[["t05"]]

  return(-(b0 * kappa * t05) / (t_vec + t05)^2)
}


# the custom beta-shaped starting point distribution
cust_x <- function(prms_model, prms_solve, x_vec, one_cond, ddm_opts) {
  alpha <- prms_model[["alpha"]]
  start_dist <- dbeta(x_vec / 2 + 0.5, alpha, alpha)

  # ensure that the distribution always integrates to 1
  dx <- prms_solve[["dx"]]
  start_dist <- start_dist / (sum(start_dist) * dx)
  return(start_dist)
}

#' Now write a function that assembles everything
#+
coll_dm <- function(obs_data = NULL, sigma = 1, t_max = 3, dt = .001,
                    dx = .001) {
  # define parameters and conditions
  prms_model <- c(
    b0 = 0.6, kappa = 0.5, t05 = 0.15, alpha = 3,
    muc = 4, non_dec = 0.3
  )
  conds <- c("null")

  # provides access to pre-build component functions
  comps <- component_shelf()

  # call the drift_dm function which is the backbone of dRiftDM
  coll_dm <- drift_dm(
    prms_model = prms_model,
    conds = conds,
    subclass = "coll_dm",
    obs_data = obs_data,
    sigma = sigma,
    t_max = t_max,
    dt = dt,
    dx = dx,
    mu_fun = comps$mu_constant, # constant drift muc
    mu_int_fun = comps$mu_int_constant, # integral of drift
    x_fun = cust_x, # custom beta-shaped start distribution
    b_fun = cust_b, # custom collapsing boundary
    dt_b_fun = cust_dt_b, # derivative of b
    nt_fun = comps$nt_constant # constant non-decision time non_dec
  )

  return(coll_dm)
}

#' Create the model and show some trajectories
#+ plot_coll_start, fig.width = 5, fig.height = 4
a_cust_model <- coll_dm()
set.seed(2)
some_traces <- simulate_traces(a_cust_model, k = 10, add_x = T)
plot(some_traces, xlim = c(0, 0.4), col = "gray30")



# -------------------------------------------------------------------------
#' # Final Remark: Using ddm_opts
#' Users can "inject" arbitrary code/objects by using the ddm_opts argument of
#' any component function.
#+ art_example_ddm_opts
a_model <- ratcliff_dm()

cust_mu <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
  print(ddm_opts$my_string)
  muc <- rep(prms_model[["muc"]], length(t_vec))
  return(muc)
}

comp_funs(a_model)[["mu_fun"]] <- cust_mu
a_model$ddm_opts$my_string <- "Hello World"
a_model <- re_evaluate_model(a_model)


# -------------------------------------------------------------------------
#' # Session Info
#+ session_info
sessionInfo()

