
library("devtools")
library("tictoc")
load_all()
rm(list = ls())
# Hallo Vali! :)

my_model <- function(type = "simple", obs_data = NULL, sigma = 1, t_max = 3,
                        dt = .005,
                        dx = .05) {
  prms_model <- c(x = 3, y = 0.6, z = 0.3)
  conds <- "null"
  r_dm <- drift_dm(
    prms_model = prms_model, conds = conds, free_prms = NULL,
    obs_data = obs_data, sigma = sigma, t_max = t_max,
    dt = dt, dx = dx
  )

  class(r_dm) <- c("my_model", class(r_dm))

  return(r_dm)
}


mu.my_model <- function(drift_dm_obj, t_vec, one_cond) {
  muc <- drift_dm_obj$prms_model[["x"]]
  if (!is.numeric(muc) | length(muc) != 1) {
    stop("parameter muc is not a single number")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }
  muc <- rep(muc, length(t_vec))
  return(muc)
}

mu_int.my_model <- function(drift_dm_obj, t_vec, one_cond) {
  muc <- drift_dm_obj$prms_model[["x"]]
  if (!is.numeric(muc) | length(muc) != 1) {
    stop("muc is not a single number")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }
  return(muc * t_vec)
}

b.my_model <- function(drift_dm_obj, t_vec, one_cond) {
  b <- drift_dm_obj$prms_model[["y"]]
  if (!is.numeric(b) | length(b) != 1) {
    stop("b is not a single number")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }
  b <- rep(b, length(t_vec))
  return(b)
}

nt.my_model <- function(drift_dm_obj, t_vec, one_cond) {
  non_dec_time <- drift_dm_obj$prms_model[["z"]]
  if (!is.numeric(non_dec_time) | length(non_dec_time) != 1) {
    stop("non_dec_time is not a single number")
  }

  if (non_dec_time < 0 | non_dec_time > drift_dm_obj$prms_solve[["t_max"]]) {
    stop("non_dec_time larger than t_max or smaller than 0!")
  }

  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }

  dt <- drift_dm_obj$prms_solve[["dt"]]
  d_nt <- numeric(length(t_vec))
  which_index <- as.integer(non_dec_time / dt)
  d_nt[which_index + 1] <- 1 / dt
  return(d_nt)
}

a_model = my_model(obs_data = ratcliff_data)

get_pdfs(a_model, solver = "kfe")
estimate_model(a_model, lower = c(1,0.4, 0.2), upper = c(6, 0.8, 0.8),
               de_n_cores = 5)
