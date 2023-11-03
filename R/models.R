# ==== Standard Ratcliff Diffusion Model

ratcliff_dm <- function(type = "simple", obs_data = NULL, sigma = 1, t_max = 3, dt = .005,
                        dx = .05) {
  prms_model <- c(muc = 3, b = 0.6, non_dec = 0.3)
  conds <- "null"
  r_dm <- drift_dm(
    prms_model = prms_model, conds = conds, free_prms = NULL,
    obs_data = obs_data, sigma = sigma, t_max = t_max,
    dt = dt, dx = dx
  )

  class(r_dm) <- c("ratcliff_dm_simple", class(r_dm))

  return(r_dm)
}


mu.ratcliff_dm_simple <- function(drift_dm_obj, t_vec, one_cond) {
  muc <- drift_dm_obj$prms_model[["muc"]]
  if (!is.numeric(muc) | length(muc) != 1) {
    stop("parameter muc is not a single number")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }
  muc <- rep(muc, length(t_vec))
  return(muc)
}

mu_int.ratcliff_dm_simple <- function(drift_dm_obj, t_vec, one_cond) {
  muc <- drift_dm_obj$prms_model[["muc"]]
  if (!is.numeric(muc) | length(muc) != 1) {
    stop("muc is not a single number")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }
  return(muc * t_vec)
}

b.ratcliff_dm_simple <- function(drift_dm_obj, t_vec, one_cond) {
  b <- drift_dm_obj$prms_model[["b"]]
  if (!is.numeric(b) | length(b) != 1) {
    stop("b is not a single number")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }
  b <- rep(b, length(t_vec))
  return(b)
}

nt.ratcliff_dm_simple <- function(drift_dm_obj, t_vec, one_cond) {
  non_dec_time <- drift_dm_obj$prms_model[["non_dec"]]
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



# === Diffusion Model for Conflict Task

dmc_dm <- function(obs_data = NULL, sigma = 1, t_max = 3, dt = .005, dx = .05) {
  prms_model <- c(
    muc = 4, b = .6, non_dec = .3, sd_non_dec = .02, tau = .04,
    a = 2, A = .1, alpha = 4
  )
  conds <- c("comp", "incomp")
  dmc_dm <- drift_dm(
    prms_model = prms_model, conds = conds,
    free_prms = c("muc", "b", "non_dec", "sd_non_dec", "tau", "A", "alpha"),
    obs_data = obs_data, sigma = sigma, t_max = t_max,
    dt = dt, dx = dx
  )

  class(dmc_dm) <- c("dmc_dm", class(dmc_dm))

  return(dmc_dm)
}


mu.dmc_dm <- function(drift_dm_obj, t_vec, one_cond) {
  # unpack values and conduct checks
  muc <- drift_dm_obj$prms_model[["muc"]]
  tau <- drift_dm_obj$prms_model[["tau"]]
  a <- drift_dm_obj$prms_model[["a"]]
  A <- drift_dm_obj$prms_model[["A"]]
  if (!is.numeric(muc) | length(muc) != 1) {
    stop("parameter muc is not a single number")
  }
  if (!is.numeric(tau) | length(tau) != 1) {
    stop("parameter tau is not a single number")
  }
  if (!is.numeric(a) | length(a) != 1) {
    stop("parameter a is not a single number")
  }
  if (!is.numeric(A) | length(A) != 1) {
    stop("parameter A is not a single number")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }

  # calculate the first derivative of the gamma-function
  if (a != 2) {
    t_vec <- t_vec + 0.0005 # general form can not be derived for t <= 0
    mua <- A * exp(-t_vec / tau) * ((t_vec * exp(1)) / ((a - 1) * tau))^(a - 1) *
      (((a - 1) / t_vec) - (1 / tau))
  } else {
    mua <- A / tau * exp(1 - t_vec / tau) * (1 - t_vec / tau)
  }

  # get drift rate, depending on the condition
  stopifnot(one_cond %in% c("comp", "incomp"))
  if (one_cond == "comp") {
    return(muc + mua)
  }
  if (one_cond == "incomp") {
    return(muc - mua)
  }
}

mu_int.dmc_dm <- function(drift_dm_obj, t_vec, one_cond) {
  # unpack values and conduct checks
  muc <- drift_dm_obj$prms_model[["muc"]]
  tau <- drift_dm_obj$prms_model[["tau"]]
  a <- drift_dm_obj$prms_model[["a"]]
  A <- drift_dm_obj$prms_model[["A"]]
  if (!is.numeric(muc) | length(muc) != 1) {
    stop("parameter muc is not a single number")
  }
  if (!is.numeric(tau) | length(tau) != 1) {
    stop("parameter tau is not a single number")
  }
  if (!is.numeric(a) | length(a) != 1) {
    stop("parameter a is not a single number")
  }
  if (!is.numeric(A) | length(A) != 1) {
    stop("parameter A is not a single number")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }

  # calculate the gamma-function and the linear function
  mua <- A * exp(-t_vec / tau) * ((t_vec * exp(1)) / ((a - 1) * tau))^(a - 1)
  muc <- muc * t_vec

  # get superimposed values, depending on the condition
  stopifnot(one_cond %in% c("comp", "incomp"))
  if (one_cond == "comp") {
    return(muc + mua)
  }
  if (one_cond == "incomp") {
    return(muc - mua)
  }
}

x.dmc_dm <- function(drift_dm_obj, one_cond) {
  alpha <- drift_dm_obj$prms_model[["alpha"]]
  nX <- drift_dm_obj$prms_solve[["nX"]]
  xx <- seq(0, 1, length.out = nX + 1)
  x <- stats::dbeta(xx, alpha, alpha) / 2
  return(x)
}

b.dmc_dm <- function(drift_dm_obj, t_vec, one_cond) {
  b <- drift_dm_obj$prms_model[["b"]]
  if (!is.numeric(b) | length(b) != 1) {
    stop("b is not a single number")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }
  b <- rep(b, length(t_vec))
  return(b)
}

nt.dmc_dm <- function(drift_dm_obj, t_vec, one_cond) {
  non_dec <- drift_dm_obj$prms_model[["non_dec"]]
  sd_non_dec <- drift_dm_obj$prms_model[["sd_non_dec"]]
  if (!is.numeric(non_dec) | length(non_dec) != 1) {
    stop("non_dec is not a single number")
  }
  if (!is.numeric(sd_non_dec) | length(sd_non_dec) != 1) {
    stop("sd_non_dec is not a single number")
  }
  if (non_dec < 0 | non_dec > drift_dm_obj$prms_solve[["t_max"]]) {
    stop("non_dec_time larger than t_max or smaller than 0!")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }

  if (sd_non_dec <= drift_dm_obj$prms_solve[["dt"]]) {
    stop("sd_non_dec should not be smaller than dt!")
  }

  t_max <- drift_dm_obj$prms_solve[["t_max"]]
  d_val <- stats::dnorm(t_vec, non_dec, sd_non_dec)
  p_u <- stats::pnorm(t_max, non_dec, sd_non_dec)
  p_l <- stats::pnorm(0, non_dec, sd_non_dec)
  d_nt <- d_val / (p_u - p_l) # truncated normal pdf over t_vec :)

  return(d_nt)
}