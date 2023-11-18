
library("devtools")
library("tictoc")
library("Rcpp")
load_all()
rm(list = ls())
# Hallo Vali! :)

a_dm = ratcliff_dm(obs_data = ratcliff_data)

estimate_model(drift_dm_obj = a_dm,
               lower = c(1, 0.2, 0.1),
               upper = c(7, 0.8, 0.7),
               de_n_cores = 6)
