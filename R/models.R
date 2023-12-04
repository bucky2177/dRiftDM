# ==== Standard Ratcliff Diffusion Model

#' Create a basic Diffusion Model
#'
#' This function creates a [dRiftDM::drift_dm] that corresponds to the basic
#' Ratcliff diffusion model
#'
#' @param type character, indicatnig "which" Ratcliff diffusion model should
#' be build. Default and only option currently is "simple".
#'
#' @param obs_data data.frame, an optional data.frame with the observed data.
#' See [dRiftDM::set_obs_data].
#' @param sigma,t_max,dt,dx numeric, providing the settings for the
#' discretization (see [dRiftDM::drift_dm])
#'
#' @details
#'
#' ## Simple Model
#'
#' With "simple" Ratcliff Diffusion Model we refer to a diffusion model
#' with  a constant drift rate `muc`, a constant boundary `b`, a constant
#' non-decision time `non_dec`, and no starting point variability. See also
#' [dRiftDM::mu_constant].
#'
#'
#' @export
ratcliff_dm <- function(type = "simple", obs_data = NULL, sigma = 1, t_max = 3,
                        dt = .001, dx = .001) {
  prms_model <- c(muc = 3, b = 0.6, non_dec = 0.3)
  conds <- "null"
  r_dm <- drift_dm(
    prms_model = prms_model, conds = conds, free_prms = NULL,
    obs_data = obs_data, sigma = sigma, t_max = t_max, dt = dt, dx = dx
  )

  # set the custom functions
  r_dm <- set_mu_fun(drift_dm_obj = r_dm, mu_fun = mu_constant)
  r_dm <- set_mu_int_fun(drift_dm_obj = r_dm, mu_int_fun = mu_int_constant)
  r_dm <- set_b_fun(drift_dm_obj = r_dm, b_fun = b_constant)
  r_dm <- set_nt_fun(drift_dm_obj = r_dm, nt_fun = nt_constant)

  class(r_dm) <- c("ratcliff_dm_simple", class(r_dm))

  return(r_dm)
}


#' Basic drift diffusion model components
#'
#' @description
#' The following functions can be used to modify/customize the components of
#' a diffusion model
#'
#' `mu_constant`, this function provides the component function for a constant
#' drift rate with parameter `muc`.
#'
#' `mu_int_constant`, provides the complementary integral to `mu_constant`.
#'
#' `b_constant`, this function provides the component function for a constant
#' boundary with parameter `b`.
#'
#' `nt_constant`, this function provides the component function for a constant
#' non-decision time with parameter `non_dec`.
#'
#' @param drift_dm_obj an object inheriting from [dRiftDM::drift_dm].
#' @param t_vec numeric vector providing the time space
#' @param one_cond character, indicating a condition
#'
#' @details
#' See \code{vignette("use_ddm_models", "dRiftDM")} for more information on how
#' to set/modify/customize the components of a diffusion model.
#'
#'
#' @export
mu_constant <- function(drift_dm_obj, t_vec, one_cond) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }
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


#' @rdname mu_constant
#' @export
mu_int_constant <- function(drift_dm_obj, t_vec, one_cond) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }
  muc <- drift_dm_obj$prms_model[["muc"]]
  if (!is.numeric(muc) | length(muc) != 1) {
    stop("muc is not a single number")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }
  return(muc * t_vec)
}

#' @rdname mu_constant
#' @export
b_constant <- function(drift_dm_obj, t_vec, one_cond) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }
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

#' @rdname mu_constant
#' @export
nt_constant <- function(drift_dm_obj, t_vec, one_cond) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }
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

#' Create the diffusion model for conflict tasks
#'
#' @description
#' This function creates a [dRiftDM::drift_dm] object that corresponds to the
#' diffusion model for conflict tasks by
#' \insertCite{Ulrichetal.2015;textual}{dRiftDM}.
#'
#' @param obs_data data.frame, an optional data.frame with the observed data.
#' See [dRiftDM::set_obs_data].
#' @param sigma,t_max,dt,dx numeric, providing the settings for the
#' discretization (see [dRiftDM::drift_dm])
#'
#' @details
#'
#'
#' The diffusion model for conflict tasks is a model for describing conflict
#' tasks like the Stroop, Simon, or flanker task.
#'
#' It has the following properties:
#' - a constant boundary (parameter `b`; see [dRiftDM::b_constant])
#' - a staring point distribution in the shape of a beta distribution
#'   (parameter `alpha`; see [dRiftDM::x_beta])
#' - an evidence accumulation process that results from the sum of two
#'    subprocesses (see [dRiftDM::mu_dmc_dm]:
#'    - a controlled process with drift rate `muc`
#'    - a gamma-shaped process with a scale parameter `tau`, a shape
#'    parameter `a`, and an amplitude `A`.
#' - A non-decision time that follows a truncated normal distribution with
#'   mean `non_dec` and standard deviation `sd_non_dec`
#'   (see [dRiftDM::nt_truncated_normal]).
#'
#' Its parameters are:  `muc`, `b`, `non_dec`, `sd_non_dec`, `tau`, `a`, `A`,
#'  and `alpha`
#'
#' Per default the shape parameter `a` is set to 2 and not allowed to
#' vary (i.e., is not listed in `free_prms`).
#'
#'
#' @export
dmc_dm <- function(obs_data = NULL, sigma = 1, t_max = 3, dt = .001,
                   dx = .001) {
  prms_model <- c(
    muc = 4, b = .6, non_dec = .3, sd_non_dec = .02, tau = .04,
    a = 2, A = .1, alpha = 4
  )
  conds <- c("comp", "incomp")
  dmc_dm <- drift_dm(
    prms_model = prms_model, conds = conds,
    free_prms = c("muc", "b", "non_dec", "sd_non_dec", "tau", "A", "alpha"),
    obs_data = obs_data, sigma = sigma, t_max = t_max, dt = dt, dx = dx
  )

  # set the custom functions
  dmc_dm <- set_mu_fun(drift_dm_obj = dmc_dm, mu_fun = mu_dmc_dm)
  dmc_dm <- set_mu_int_fun(drift_dm_obj = dmc_dm, mu_int_fun = mu_int_dmc_dm)
  dmc_dm <- set_x_fun(drift_dm_obj = dmc_dm, x_fun = x_beta)
  dmc_dm <- set_b_fun(drift_dm_obj = dmc_dm, b_fun = b_constant)
  dmc_dm <- set_nt_fun(drift_dm_obj = dmc_dm, nt_fun = nt_truncated_normal)


  class(dmc_dm) <- c("dmc_dm", class(dmc_dm))

  return(dmc_dm)
}

#' Drift Rate for DMC
#'
#' @description
#' The following functions are used for the drift rate of DMC
#' (see [dRiftDM::dmc_dm]).
#'
#' `mu_dmc_dm`, this function provides the superimposed diffusion process
#'  of DMC. Necessary parameters `muc`, `a`, `A`, `tau`.
#'
#' `mu_int_dmc_dm`, provides the complementary integral to `mu_dmc_dm`.
#'
#' @param drift_dm_obj an object inheriting from [dRiftDM::drift_dm].
#' @param t_vec numeric vector providing the time space
#' @param one_cond character, indicating a condition
#'
#' @details
#' See \code{vignette("use_ddm_models", "dRiftDM")} for more information on how
#' to set/modify/customize the components of a diffusion model.
#'
#' @export
mu_dmc_dm <- function(drift_dm_obj, t_vec, one_cond) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }
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


#' @rdname mu_dmc_dm
#' @export
mu_int_dmc_dm <- function(drift_dm_obj, t_vec, one_cond) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }
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

#' A beta-shaped starting point component
#'
#' This function provides the function component for a symmetric beta-shaped
#' starting point distribution with parameter `alpha`.
#'
#' @param drift_dm_obj an object inheriting from [dRiftDM::drift_dm].
#' @param x_vec numeric vector providing the evidence space
#' @param one_cond character, indicating a condition
#'
#' @details
#' See \code{vignette("use_ddm_models", "dRiftDM")} for more information on how
#' to set/modify/customize the components of a diffusion model.
#'
#' @export
x_beta <- function(drift_dm_obj, x_vec, one_cond) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }
  alpha <- drift_dm_obj$prms_model[["alpha"]]
  if (!is.numeric(alpha) | length(alpha) != 1) {
    stop("parameter alpha is not a single number")
  }
  if (!is.numeric(x_vec) | length(x_vec) <= 1) {
    stop("x_vec is not a vector")
  }

  xx <- seq(0, 1, length.out = length(x_vec))
  x <- stats::dbeta(xx, alpha, alpha) / 2
  return(x)
}


#' A truncated-normal shaped non-decision time component
#'
#' This function provides the function component for a non-decision time
#' following a truncated-normal distribution with mean `non_dec` and
#' standard deviation `sd_non_dec`.
#'
#' @param drift_dm_obj an object inheriting from [dRiftDM::drift_dm].
#' @param t_vec numeric vector providing the time space
#' @param one_cond character, indicating a condition
#'
#' @details
#' See \code{vignette("use_ddm_models", "dRiftDM")} for more information on how
#' to set/modify/customize the components of a diffusion model.
#'
#' @export
nt_truncated_normal <- function(drift_dm_obj, t_vec, one_cond) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }
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
