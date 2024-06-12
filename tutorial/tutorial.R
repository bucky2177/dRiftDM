#===================================
# Author: Valentin Koob
# Date: 03.06.2024
# Description: This file provides all the code that is described in the
#              tutorial for dRiftDM
#===================================

rm(list = ls())

#========GETTING STARTED=========
# install the package
# install.packages("devtools")
devtools::install_github("bucky2177/dRiftDM")
# optional: install.packages("cowsay")

# load the package
library(dRiftDM)
a_model = ratcliff_dm() # a most basic DDM
print(a_model)

names(a_model)

# basic setter functions
a_model = set_model_prms(
  drift_dm_obj = a_model,
  new_prm_vals = c(muc = 2, b = 0.5) # named numeric vector
)

a_model = set_free_prms(
  drift_dm_obj = a_model,
  new_free_prms = c("muc", "non_dec") # alternative: new_fixed_prms = "b"
)

a_model = set_solver_settings(
  drift_dm_obj = a_model,
  new_solver_vals = c(sigma = 0.1, t_max = 2, dt = .002, dx = .002)
)

print(a_model)

a_model = set_solver_settings(
  drift_dm_obj = a_model,
  new_solver_vals = c(sigma = 1)
)


#========EXPLORE PREDICTIONS=========

# calculate and plot traces
set.seed(1)
one_trace = simulate_traces(
  drift_dm_obj = a_model,
  k = 1
)
head(round(one_trace,3))


pdf("./ratcliff_traces.pdf", width = 6, height = 3.5)
par(mfrow = c(1,2))
set.seed(3)
plot_traces(
  drift_dm_obj = a_model,
  k = 2,
  y_lim = c(-0.55, 0.55)
)
plot_traces(
  drift_dm_obj = a_model,
  k = 2,
  sigma = 0,
  y_lim = c(-0.55, 0.55)
)
dev.off()



a_model = re_evaluate_model(a_model)
names(a_model)


# calculate stats
preds = calc_stats(
  drift_dm_obj = a_model,
  type = c("cafs", "quantiles"),
  source = "pred"
)

preds$cafs
head(preds$quantiles)

# plot stats
pdf("./ratcliff_stats.pdf", width = 6, height = 3.5)
plot_stats(
  obj = a_model,
  type = c("cafs", "quantiles"),
  source = "pred",
  mfrow = c(1, 2),
  x_lim_quantiles = c(0, 1)
)
dev.off()


#========SET AND FIT A SINGLE DATA SET=========

# create a model
a_model = dmc_dm()
print(a_model)

# set some data
head(dmc_synth_data) # available with dRiftDM

a_model = set_obs_data(
  drift_dm_obj = a_model,
  obs_data = dmc_synth_data
)

print(a_model)

# now fit the data
a_model = set_solver_settings(
  drift_dm_obj = a_model,
  new_solver_vals = c(t_max = 1.5, dt = 0.002, dx = 0.002),
)

lower_prm_bnd = c(muc = 2, b = 0.3, non_dec = 0.2, sd_non_dec = 0.01,
                  tau = 0.02, A = 0.02, alpha = 2)
upper_prm_bnd = c(muc = 6, b = 0.8, non_dec = 0.4,  sd_non_dec = 0.06,
                  tau = 0.20, A = 0.20, alpha = 7)


a_model = estimate_model(  # takes about 30 seconds
  drift_dm_obj = a_model,
  lower = lower_prm_bnd,
  upper = upper_prm_bnd,
  use_de_optim = F,
  use_nmkb = T,
)

print(a_model)

a_model = estimate_model(   # takes about 4-6 minutes
  drift_dm_obj = a_model,
  lower = lower_prm_bnd,
  upper = upper_prm_bnd,
  seed = 1,
  de_n_cores = 6,
)

# investigate results
summary(a_model)
AIC(a_model)
BIC(a_model)
logLik(a_model)

pdf("./dmc_fit_single.pdf", width = 8, height = 3)
plot_stats(
  obj = a_model,
  type = c("cafs", "quantiles", "delta_funs"),
  mfrow = c(1,3),
  minuends_deltas = c("incomp"),
  subtrahends_deltas = c("comp"),
  line_cols_cafs = c("green", "red"),
  line_cols_quantiles = c("green", "red"),
  line_cols_deltas = "black",
  x_lim_quantiles = c(0.2, 0.9),
  x_lim_deltas = c(0.2, 0.9)
)
dev.off()



#========SET AND FIT MULTIPLE DATA SETS=========

large_dat = ulrich_flanker_data # provided by dRiftDM
head(large_dat)


a_model = dmc_dm( # fresh model without data
  t_max = 1.5,
  dx = .002,
  dt = .002
)

estimate_model_ids(
  drift_dm_obj = a_model,
  obs_data_ids = large_dat,
  lower = lower_prm_bnd,
  upper = upper_prm_bnd,
  fit_procedure_name = "ulrich_flanker",
  use_de_optim = F, # DE is default
  use_nmkb = T      # but Nelder-Mead is faster (for the tutorial)
)

# fit procedure structure
list.dirs()
list.files("./drift_dm_fits/ulrich_flanker/")

# load a fit procedure
all_fits = load_fits_ids(
  fit_procedure_name = "ulrich_flanker"
)

print(all_fits)

class(all_fits)

names(all_fits)

# access stats and parameters
all_prms = gather_parameters(fits_ids = all_fits)
head(all_prms, 2)

colMeans(all_prms)

some_stats = gather_stats(
  fits_ids = all_fits,
  type = "cafs"
)
head(some_stats)

# plot model fit
pdf("./dmc_fit_flanker.pdf", width = 8, height = 3)
plot_stats(
  obj = all_fits,
  type = c("cafs", "quantiles", "delta_funs"),
  mfrow = c(1,3),
  minuends_deltas = c("incomp"),
  subtrahends_deltas = c("comp"),
  line_cols_cafs = c("green", "red"),
  line_cols_quantiles = c("green", "red"),
  line_cols_deltas = "black",
  x_lim_quantiles = c(0.2, 0.7),
  x_lim_deltas = c(0.2, 0.6),
  y_lim_deltas = c(-0.02, 0.08)
)
dev.off()

#========A SIMPLE PARAMETER RECOVERY STUDY=========

# the model itself
a_model = ratcliff_dm()

# generate orig_prms and synthetic data sets
set.seed(1)
orig_prms = simulate_values(
  lower = c(2, 0.3, 0.2), # must match free_prms
  upper = c(6, 0.8, 0.4),
  k = 50
)
orig_prms = as.data.frame(orig_prms)
colnames(orig_prms)[1:3] = a_model$free_prms

synth_data = simulate_data(
  drift_dm_obj = a_model,
  n = 100,
  df_prms = orig_prms
)


# fit the model
a_model = set_solver_settings(
  drift_dm_obj = a_model,
  new_solver_vals = c(t_max = 2, dx = .002, dt = .002)
)

estimate_model_subjects( # takes about 30 minutes
  drift_dm_obj = a_model,
  obs_data_subject = synth_data,
  lower = c(1.5, 0.25, 0.15),
  upper = c(6.5, 0.85, 0.45),
  fit_procedure_name = "ratcliff_recovery",
  de_n_cores = 6,
  seed = 2, force_refit = T
)

# load the model and extract parameters
recov_fits = load_fits_ids(
  fit_procedure_name = "ratcliff_recovery"
)
recov_prms = gather_parameters(recov_fits)
stopifnot(recov_prms$Subject == orig_prms$Subject)

cor(recov_prms$muc, orig_prms$muc)
cor(recov_prms$b, orig_prms$b)
cor(recov_prms$non_dec, orig_prms$non_dec)



#=======BUILD A CUSTOM MODEL: SSP===========


# drift rate for SSP
mu_ssp = function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {

  # extract all parameters
  p = prms_model[["p"]]
  sd_0 = prms_model[["sd_0"]]
  r = prms_model[["r"]]

  # calculate the standard deviation at each time step
  sd_t = pmax(sd_0 - r * t_vec, 0.001)

  # calculate attentional weights
  a_tar_t = pnorm(q = 0.5, mean = 0, sd = sd_t) -
    pnorm(q = -0.5, mean = 0, sd = sd_t)
  a_fl_t = 1 - a_tar_t

  # pass back the drift rate, depending on the condition
  if (one_cond == "comp") {
    mu_t = a_tar_t * p + a_fl_t * p
  } else if (one_cond == "incomp") {
    mu_t = a_tar_t * p - a_fl_t * p
  }
  return(mu_t)
}



# write a function for creating the complete SSP
ssp_dm = function(obs_data = NULL, sigma = 1, t_max = 3, dt = .001,
                  dx = .001) {


  # define parameters and conditions
  prms_model = c(p = 3.3, sd_0 = 1.2, r = 10, b = .6, non_dec = .3)
  conds = c("comp", "incomp")

  # call the drift_dm function which is the backbone of dRiftDM
  ssp_dm = drift_dm(
    prms_model = prms_model,
    conds = conds,
    free_prms = NULL,
    obs_data = obs_data,
    sigma = sigma,
    t_max = t_max,
    dt = dt,
    dx = dx,
    mu_fun = mu_ssp,  # custom defined
    mu_int_fun = component_shelf()$dummy_t, # no integral of drift
    x_fun = component_shelf()$x_dirac_0,    # dirac delta on zero
    b_fun = component_shelf()$b_constant,   # constant boundary b
    dt_b_fun = component_shelf()$dt_b_constant,  # derivative of b
    nt_fun = component_shelf()$nt_constant  # constant non-decision time non_dec
  )

  # define it as a child of "drift_dm"
  class(ssp_dm) = c("ssp_dm", class(ssp_dm))

  return(ssp_dm)
}



#=======BUILD A CUSTOM MODEL: COLLAPSING THRESHOLD + variable Start Point===========

# hyperbolic ratio function
cust_b = function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {

  b0 = prms_model[["b0"]]
  kappa = prms_model[["kappa"]]
  t05 = prms_model[["t05"]]


  return(b0 * (1 - kappa * t_vec / (t_vec + t05)))
}

# derivative of the boundary function
cust_dt_b = function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {

  b0 = prms_model[["b0"]]
  kappa = prms_model[["kappa"]]
  t05 = prms_model[["t05"]]

  return(-(b0 * kappa * t05) / (t_vec + t05) ^ 2)
}

# a custom beta-shaped starting point distribution
cust_x = function(prms_model, prms_solve, x_vec, one_cond, ddm_opts) {

  alpha = prms_model[["alpha"]]
  start_dist = dbeta(x_vec/2 + 0.5, alpha, alpha)

  return(start_dist / 2) # / 2 to ensure that it integrates to 1
}


# write a function for creating the custom model
coll_dm = function(obs_data = NULL, sigma = 1, t_max = 3, dt = .001,
                   dx = .001) {


  # define parameters and conditions
  prms_model = c(b0 = 0.6, kappa = 0.2, t05 = 0.15, alpha = 3,
                 muc = 4, non_dec = 0.3)
  conds = c("null")

  # call the drift_dm function which is the backbone of dRiftDM
  coll_dm = drift_dm(
    prms_model = prms_model,
    conds = conds,
    free_prms = NULL,
    obs_data = obs_data,
    sigma = sigma,
    t_max = t_max,
    dt = dt,
    dx = dx,
    mu_fun = component_shelf()$mu_constant, # constant drift muc
    mu_int_fun = component_shelf()$mu_int_constant, # integral of drift
    x_fun = cust_x,    # custom beta-shaped start distribution
    b_fun = cust_b,    # custom collapsing boundary
    dt_b_fun = cust_dt_b,  # derivative of b
    nt_fun = component_shelf()$nt_constant  # constant non-decision time non_dec
  )

  # define it as a child of "drift_dm"
  class(coll_dm) = c("coll_dm", class(coll_dm))

  return(coll_dm)
}

a_cust_model = coll_dm()
set.seed(2)
plot_traces(a_cust_model, k = 10, sigma = 0, add_x = T)


#=======BUILD A CUSTOM MODEL WITH CONDITION DEPENDENT NON-DECISION TIME======

cust_nt = function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {

  if (one_cond == "low") {
    m = prms_model[["m_low"]]
  } else if (one_cond == "medium") {
    m = prms_model[["m_medium"]]
  } else if (one_cond == "high") {
    m = prms_model[["m_high"]]
  }
  sd = prms_model["sd"]

  dt = prms_solve[["dt"]]
  stopifnot(sd > dt) # to ensure that sd is not too small

  dens = dnorm(t_vec, m, sd)
  dens = dens/(sum(dens) * dt) # make it integrate to 1
  return(dens)
}



# write a function for creating the custom model
cond_dm = function(obs_data = NULL, sigma = 1, t_max = 3, dt = .001,
                   dx = .001) {


  # define parameters and conditions
  prms_model = c(m_low = 0.5, m_medium = 0.4, m_high = 0.3, sd = .005,
                 muc = 4, b = 0.6)
  conds = c("low", "medium", "high")

  # call the drift_dm function which is the backbone of dRiftDM
  cond_dm = drift_dm(
    prms_model = prms_model,
    conds = conds,
    free_prms = NULL,
    obs_data = obs_data,
    sigma = sigma,
    t_max = t_max,
    dt = dt,
    dx = dx,
    mu_fun = component_shelf()$mu_constant, # constant drift muc
    mu_int_fun = component_shelf()$mu_int_constant, # integral of drift
    x_fun = component_shelf()$x_dirac_0, # dirac delta on 0
    b_fun = component_shelf()$b_constant, # constant boundary b
    dt_b_fun = component_shelf()$dt_b_constant,  # derivative of b
    nt_fun = cust_nt # custom non-decision time
  )

  # define it as a child of "drift_dm"
  class(cond_dm) = c("cond_dm", class(cond_dm))

  return(cond_dm)
}

a_cond_dm = cond_dm()


# joint plot

pdf("./custom_models_plot.pdf", width = 9, height = 3.5)
par(mfrow = c(1,3))
plot_traces(a_ssp_model, k = 1, sigma = 0)
plot_traces(a_cust_model, k = 10, sigma = 0, add_x = T)
plot_stats(
  obj = a_cond_dm,
  type = "quantiles",
  source = "pred"
)
dev.off()



#=======USING DDM_OPTS======

a_model = ratcliff_dm()

cust_mu = function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
  print(ddm_opts$my_string)
  muc = rep(prms_model[["muc"]], length(t_vec))
  return(muc)
}

a_model = set_comp_funs(
  drift_dm_obj = a_model,
  comp_funs = list(mu_fun = cust_mu)
)

a_model$ddm_opts$my_string = "Hello World"
a_model = re_evaluate_model(a_model)
