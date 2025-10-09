#' ---
#' title: "Reproducing dRiftDM Article Output - package version 0.2.2"
#' author: "Valentin Koob (bucky2177)"
#' date: "04.03.2025"
#' ---
#'
#' This document contains the code for dRiftDM's old tutorial.

rm(list = ls())

# -------------------------------------------------------------------------
#' # Getting Started

#' Load the package and choose a pre-built model.
#+ get_a_ddm
library("dRiftDM")
a_model <- ratcliff_dm() # a basic DDM (the Ratcliff DDM)
print(a_model) # print the model to the console

#' Demonstration of basic accessor and replacement functions. Documentation
#' available via: `?coef.drift_dm`; `?prms_solve`; `?solver`; `?drift_dm`
#+ modify_a_ddm
coef(a_model) # the unique model parameters
coef(a_model)["muc"] <- 3.2 # sets the value of muc to 3.2
coef(a_model)["b"] <- .5 # sets the value of b to 0.5

prms_solve(a_model) # the "solver settings" (= scaling and discretization)
prms_solve(a_model)["t_max"] <- 2 # changes the maximum time
prms_solve(a_model)["dt"] <- .005 # changes the time discretization

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
  xlim = c(0, .4) # controls the x-axis limit
)
plot(some_traces_no_noise, col = "gray30", xlim = c(0, .4))


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
#' integral approach for models that have a dirac delta on zero for the starting
#' point
#+ change_solver
solver(a_model) # shows the current method
solver(a_model) <- "im_zero" # changes method to the integral approach



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

#' Increase discretization for speed and then estimate the model using (bounded)
#' Nelder-Mead
#+ estimate_one_data_set_nmkb
# set the discretization
# t_max -> should be large enough to easily cover even the largest observed RTs
# dt, dx -> depend on the model; no general recommendation yet
prms_solve(a_model)["t_max"] <- 2
prms_solve(a_model)["dt"] <- .01
prms_solve(a_model)["dx"] <- .01

# create the search space -> defined this way, the lower and upper search
# space will be the same for all conditions (see ?estimate_model for more
# details on how to specify the search space)
lower_prm_bnd <- c(
  muc = 2, b = .3, non_dec = .2, sd_non_dec = .01,
  tau = .02, A = .02, alpha = 3
)
upper_prm_bnd <- c(
  muc = 6, b = .8, non_dec = .4, sd_non_dec = .06,
  tau = .15, A = .20, alpha = 7
)

# estimate the model with (bounded) Nelder-Mead
a_model <- estimate_model( # takes about 15 seconds
  drift_dm_obj = a_model, # the model (i.e., DMC)
  lower = lower_prm_bnd, # the lower end of the search space
  upper = upper_prm_bnd, # the upper end of the search space
  use_de_optim = FALSE, # don't use the (default) DE algorithm
  use_nmkb = TRUE # but use (bounded) Nelder-Mead
)

#' Repeat the same, but now use Differential Evolution
#+ estimate_one_data_set_de
# uses 2 cores or less (if not available)
# users can increase this number (e.g., to about 6)
n_cores <- pmin(parallel::detectCores(), 2)

# use DE algorithm with multiple cores
a_model <- estimate_model( # takes about 7 minutes with 2 cores
  drift_dm_obj = a_model,
  lower = lower_prm_bnd,
  upper = upper_prm_bnd,
  seed = 1,
  de_control = list(
    reltol = 1e-8, steptol = 50,
    itermax = 200, trace = TRUE
  ),
  de_n_cores = n_cores
)

#' Investigate results
#+ calc_stats_single_fit
summary(a_model)

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
a_model <- dmc_dm(t_max = 2, dx = .01, dt = .01)

# now call the fit procedure.
# we'll write each fit into the working directory
estimate_model_ids( # takes about 10 minutes
  drift_dm_obj = a_model, # the model to fit ...
  obs_data_ids = large_dat, # to each participant in large_dat
  lower = lower_prm_bnd, # lower boundary of the search space
  upper = upper_prm_bnd, # upper boundary of the search space
  fit_path = getwd(), # write in the working directory
  fit_procedure_name = "ulrich_flanker", # a label for the fit procedure
  use_de_optim = FALSE, # DE is default
  use_nmkb = TRUE # but use Nelder-Mead (faster; for the tutorial)
)

#' See the folder structure
#+ folder_str
# fit procedure structure (saved within working directory; getwd())
list.dirs()

#' Load all fits to investigate model fit and the parameter estimates
#+ load_plot_multiple_ids, fig.width = 8, fig.height = 4
# load a fit procedure
all_fits <- load_fits_ids(
  fit_procedure_name = "ulrich_flanker",
)

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
lower_sim_bnd <- c(muc = 2, b = .3, non_dec = .2)
upper_sim_bnd <- c(muc = 6, b = .8, non_dec = .4)

# draw parameter values and simulate synthetic data
set.seed(1)
data_prms <- simulate_data(
  a_model, # the model
  n = 100, # how many trials per data set?
  k = 50, # how many data sets?
  lower = lower_sim_bnd, # the lower and
  upper = upper_sim_bnd # upper end of the simulation space
)


#' Fit the model to the synthetic data
#+ fit_prm_recovery
synth_data <- data_prms$synth_data # extract the synthetic data

# increase discretization
prms_solve(a_model)["dx"] <- c(.005)
prms_solve(a_model)["dt"] <- c(.005)

# fit the model to each synthetic data
n_cores <- pmin(parallel::detectCores(), 2) # use 2 cores (if available)

estimate_model_ids( # takes about 30 minutes with 2 cores
  drift_dm_obj = a_model, # the model to fit
  obs_data_ids = synth_data, # the synthetic data
  lower = lower_sim_bnd, # the lower search space
  upper = upper_sim_bnd, # the upper search space
  fit_path = getwd(), # save to working directory
  fit_procedure_name = "ratcliff_recovery", # a label for the fit procedure
  de_n_cores = n_cores, # the number of cores
  seed = 2 # a seed for reproducible results
)

#' Load the model fits and extract parameters to calculate correlations and
#' biases.
#+ summarize_prm_recovery
recov_fits <- load_fits_ids(fit_procedure_name = "ratcliff_recovery")
recov_prms <- coef(recov_fits) # the estimated parameter values
orig_prms <- data_prms$prms # the original parameter values

# correlations
cor(recov_prms$muc, orig_prms$muc)
cor(recov_prms$b, orig_prms$b)
cor(recov_prms$non_dec, orig_prms$non_dec)

# biases
mean(recov_prms$muc - orig_prms$muc) / diff(range(orig_prms$muc))
mean(recov_prms$b - orig_prms$b) / diff(range(orig_prms$b))
mean(recov_prms$non_dec - orig_prms$non_dec) / diff(range(orig_prms$non_dec))



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


#' 2. Example: Create a neutral condition.
#+ flex_prms_2
# set new conditions to the model (this will reset parameter specifications)
my_dmc_model <- dmc_dm()
conds(my_dmc_model) <- c("comp", "neutral", "incomp")
flex_prms(my_dmc_model) # here we see the reset

#' Now modify the new model to suit our needs.
#+
# 1.) fix a so that it is not estimated
# 2.) set A negative for incomp condition
# 3.) set A = 0 for neutral condition and keep it fixed
# -> can be done via a set of "instructions" (see ?modify_flex_prms)
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
print(my_dmc_model)

#' Now Visualize the results
#+ plot_neutral_dmc, fig.width = 10, fig.height = 10

# visualize the results
plot(
  my_dmc_model,
  xlim = c(0, .4),
  col = c("green", "orange", "red"),
  legend_pos = "topleft"
)


# -------------------------------------------------------------------------
#' # Build a Model from Scratch: SPP
#'
#' We can access each component function with `comp_funs()`
#+ comp_funs
names(comp_funs(a_model))

#' In the following, we create the Shrinking Spotlight Model. To this end, we
#' first define the custom drift rate of this model, and then write a wrapper
#' around a function call to `drift_dm()`.
#+ ssp_code
# drift rate for SSP
mu_ssp <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
  # extract all parameters (one row of the parameter matrix)
  p <- prms_model[["p"]]
  sd_0 <- prms_model[["sd_0"]]
  r <- prms_model[["r"]]
  sign <- prms_model[["sign"]] # this will be controlled via flex_prms


  # calculate the standard deviation at each time step
  sd_t <- pmax(sd_0 - r * t_vec, .001)

  # calculate attentional weights
  a_tar_t <- pnorm(q = .5, mean = 0, sd = sd_t) -
    pnorm(q = -.5, mean = 0, sd = sd_t)
  a_fl_t <- 1 - a_tar_t

  # pass back the drift rate, depending on the condition
  mu_t <- a_tar_t * p + sign * a_fl_t * p
  return(mu_t)
}

# write a function for creating the complete SSP
ssp_dm <- function() {
  # define parameters and conditions
  # -> p, sd_0, r, and sign are required by our custom drift rate function
  # b is required for the boundary
  # non_dec is required for the non-decision time
  prms_model <- c(p = 3.3, sd_0 = 1.2, r = 10, b = .6, non_dec = .2, sign = 1)
  conds <- c("comp", "incomp")

  # provides access to pre-built component functions
  comps <- component_shelf()

  # call the drift_dm function which is the backbone of dRiftDM
  ssp_dm <- drift_dm(
    prms_model = prms_model,
    conds = conds,
    subclass = "ssp",
    mu_fun = mu_ssp, # our custom drift rate function
    mu_int_fun = comps$dummy_t, # no integral of drift needed
    x_fun = comps$x_dirac_0, # dirac delta on zero for the starting point
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

# simulate expected time course
set.seed(1)
some_traces_no_noise <- simulate_traces(my_ssp, k = 1, sigma = 0)
plot(some_traces_no_noise, col = c("green", "red"))


# -------------------------------------------------------------------------
#' # Session Info
#+ session_info
sessionInfo()
