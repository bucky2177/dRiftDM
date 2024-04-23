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
#' [dRiftDM::component_shelf].
#'
#'
#' @export
ratcliff_dm <- function(var_non_dec = FALSE, var_start = FALSE,
                        obs_data = NULL, sigma = 1, t_max = 3,
                        dt = .001, dx = .001) {
  prms_model <- c(muc = 3, b = 0.6, non_dec = 0.3)
  if (var_non_dec) prms_model <- append(prms_model, c(range_non_dec = 0.05))
  if (var_start) prms_model <- append(prms_model, c(range_start = 0.05))

  conds <- "null"
  r_dm <- drift_dm(
    prms_model = prms_model, conds = conds, free_prms = NULL,
    obs_data = obs_data, sigma = sigma, t_max = t_max, dt = dt, dx = dx
  )

  # set the custom functions
  r_dm <- set_mu_fun(drift_dm_obj = r_dm, mu_fun = mu_constant)
  r_dm <- set_mu_int_fun(drift_dm_obj = r_dm, mu_int_fun = mu_int_constant)
  r_dm <- set_b_fun(drift_dm_obj = r_dm, b_fun = b_constant)
  r_dm <- set_dt_b_fun(drift_dm_obj = r_dm, dt_b_fun = dt_b_constant)

  if (!var_non_dec) {
    r_dm <- set_nt_fun(drift_dm_obj = r_dm, nt_fun = nt_constant)
  } else {
    r_dm <- set_nt_fun(drift_dm_obj = r_dm, nt_fun = nt_uniform)
  }

  if (!var_start) {
    r_dm <- set_x_fun(drift_dm_obj = r_dm, x_fun = x_dirac_0)
  } else {
    r_dm <- set_x_fun(drift_dm_obj = r_dm, x_fun = x_uniform)
  }

  class(r_dm) <- c("ratcliff", class(r_dm))

  return(r_dm)
}

# ==== Standard Diffusion Model Components

#' Diffusion model components
#'
#' @description
#' The following function is meant as a convenient way to access pre-built
#' model component functions. It returns a list of the following functions.
#'
#' `mu_constant`, this function provides the component function for a constant
#' drift rate with parameter `muc`.
#'
#' `mu_dmc`, this function provides the superimposed diffusion process
#'  of DMC. Necessary parameters are `muc` (drift rate of the controlled
#'  process), `a` (shape..), `A` (amplitude...), `tau` (and scale of the
#'  automatic process.
#'
#' `mu_int_constant`, provides the complementary integral to `mu_constant`.
#'
#' `mu_int_dmc`, provides the complementary integral to `mu_dmc`.
#'
#' `x_dirac_0`, this function provides a dirac delta for a starting point
#' centered between the boundaries
#'
#' `x_beta`, this function provides the function component for a symmetric
#' beta-shaped starting point distribution with parameter `alpha`.
#'
#' `b_constant`, this function provides the component function for a constant
#' boundary with parameter `b`.
#'
#' `nt_constant`, this function provides the component function for a constant
#' non-decision time with parameter `non_dec`.
#'
#' `nt_truncated_normal`, this function provides the component function for
#' a normally distributed non-decision time with parameters `non_dec`,
#' `sd_non_dec`. The Distribution is truncated to \eqn{[0, t_max]}.
#'
#'
#' @details
#' See \code{vignette("use_ddm_models", "dRiftDM")} for more information on how
#' to set/modify/customize the components of a diffusion model.
#'
#' @export
component_shelf <- function() {

  components <- list()

  # mus
  components$mu_constant <- mu_constant
  components$mu_dmc <- mu_dmc

  # mus int
  components$mu_int_constant <- mu_int_constant
  components$mu_int_dmc <- mu_int_dmc

  # xs
  components$x_dirac_0 <- x_dirac_0
  components$x_beta <- x_beta
  components$x_uniform <- x_uniform

  # bs
  components$b_constant <- b_constant

  # bs dt
  components$dt_b_constant <- dt_b_constant

  # nts
  components$nt_constant <- nt_constant
  components$nt_uniform <- nt_uniform
  components$nt_truncated_normal <- nt_truncated_normal

  return(components)
}


mu_constant <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {

  muc <- prms_model[["muc"]]
  if (!is.numeric(muc) | length(muc) != 1) {
    stop("parameter muc is not a single number")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }
  muc <- rep(muc, length(t_vec))
  return(muc)
}



mu_int_constant <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {

  muc <- prms_model[["muc"]]
  if (!is.numeric(muc) | length(muc) != 1) {
    stop("muc is not a single number")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }
  return(muc * t_vec)
}


x_dirac_0 <- function(prms_model, prms_solve, x_vec, one_cond, ddm_opts) {

  dx <- prms_solve[["dx"]]
  if (!is.numeric(dx) | length(dx) != 1) {
    stop("dx is not a single number")
  }

  # starting point at 0
  if (!is.numeric(x_vec) | length(x_vec) <= 1) {
    stop("x_vec is not a vector")
  }

  x <- numeric(length = length(x_vec))
  x[(length(x) + 1) %/% 2] <- 1 / dx
  return(x)
}

x_uniform <- function(prms_model, prms_solve, x_vec, one_cond, ddm_opts) {

  range_start <- prms_model[["range_start"]]
  if (!is.numeric(range_start) | length(range_start) != 1) {
    stop("parameter range_start is not a single number")
  }
  if (!is.numeric(x_vec) | length(x_vec) <= 1) {
    stop("x_vec is not a vector")
  }

  # uniform around staring point of 0
  x <- stats::dunif(x = x_vec, min = 0 - range_start/2, max = 0 + range_start/2)
  return(x)
}


b_constant <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {

  b <- prms_model[["b"]]
  if (!is.numeric(b) | length(b) != 1) {
    stop("b is not a single number")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }
  b <- rep(b, length(t_vec))
  return(b)
}


dt_b_constant <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {

  # constant boundary
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }
  dt_b <- rep(0, length(t_vec))
  return(dt_b)
}


nt_constant <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {

  non_dec <- prms_model[["non_dec"]]
  tmax <- prms_solve[["t_max"]]
  dt <-  prms_solve[["dt"]]
  if (!is.numeric(non_dec) | length(non_dec) != 1) {
    stop("non_dec is not a single number")
  }
  if (!is.numeric(tmax) | length(tmax) != 1) {
    stop("tmax is not a single number")
  }
  if (!is.numeric(dt) | length(dt) != 1) {
    stop("dt is not a single number")
  }

  if (non_dec < 0 | non_dec > prms_solve[["t_max"]]) {
    stop("non_dec larger than t_max or smaller than 0!")
  }

  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }

  dt <- prms_solve[["dt"]]
  d_nt <- numeric(length(t_vec))
  which_index <- as.integer(non_dec / dt)
  d_nt[which_index + 1] <- 1 / dt
  return(d_nt)
}

nt_uniform <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
  non_dec <- prms_model[["non_dec"]]
  range_non_dec <- prms_model[["range_non_dec"]]
  t_max <- prms_solve[["t_max"]]
  dt <- prms_solve[["dt"]]

  if (!is.numeric(non_dec) | length(non_dec) != 1) {
    stop("non_dec is not a single number")
  }
  if (!is.numeric(range_non_dec) | length(range_non_dec) != 1) {
    stop("range_non_dec is not a single number")
  }
  if (non_dec < 0 | non_dec > t_max) {
    stop("non_dec_time larger than t_max or smaller than 0!")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }
  if (range_non_dec <= dt) {
    stop("range_non_dec should not be smaller than dt!")
  }


  d_nt <- stats::dunif(x = t_vec, min = non_dec - range_non_dec/2,
                       max = non_dec + range_non_dec/2)
  d_nt <- d_nt / (sum(d_nt) * dt) # ensure it integrates to 1
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
#' It has the following properties (see [dRiftDM::component_shelf]):
#' - a constant boundary (parameter `b`)
#' - a staring point distribution in the shape of a beta distribution
#'   (parameter `alpha`)
#' - an evidence accumulation process that results from the sum of two
#'    subprocesses:
#'    - a controlled process with drift rate `muc`
#'    - a gamma-shaped process with a scale parameter `tau`, a shape
#'    parameter `a`, and an amplitude `A`.
#' - A non-decision time that follows a truncated normal distribution with
#'   mean `non_dec` and standard deviation `sd_non_dec`.
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
  dmc_dm <- set_mu_fun(drift_dm_obj = dmc_dm, mu_fun = mu_dmc)
  dmc_dm <- set_mu_int_fun(drift_dm_obj = dmc_dm, mu_int_fun = mu_int_dmc)
  dmc_dm <- set_x_fun(drift_dm_obj = dmc_dm, x_fun = x_beta)
  dmc_dm <- set_b_fun(drift_dm_obj = dmc_dm, b_fun = b_constant)
  dmc_dm <- set_dt_b_fun(drift_dm_obj = dmc_dm, dt_b_fun = dt_b_constant)
  dmc_dm <- set_nt_fun(drift_dm_obj = dmc_dm, nt_fun = nt_truncated_normal)


  class(dmc_dm) <- c("dmc_dm", class(dmc_dm))

  return(dmc_dm)
}

#====
# DMC component functions

mu_dmc <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {

  # unpack values and conduct checks
  muc <- prms_model[["muc"]]
  tau <- prms_model[["tau"]]
  a <- prms_model[["a"]]
  A <- prms_model[["A"]]
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


mu_int_dmc <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {

  # unpack values and conduct checks
  muc <- prms_model[["muc"]]
  tau <- prms_model[["tau"]]
  a <- prms_model[["a"]]
  A <- prms_model[["A"]]
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


x_beta <- function(prms_model, prms_solve, x_vec, one_cond, ddm_opts) {

  alpha <- prms_model[["alpha"]]
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



nt_truncated_normal <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
  non_dec <- prms_model[["non_dec"]]
  sd_non_dec <- prms_model[["sd_non_dec"]]
  t_max <- prms_solve[["t_max"]]
  dt <- prms_solve[["dt"]]

  if (!is.numeric(non_dec) | length(non_dec) != 1) {
    stop("non_dec is not a single number")
  }
  if (!is.numeric(sd_non_dec) | length(sd_non_dec) != 1) {
    stop("sd_non_dec is not a single number")
  }
  if (non_dec < 0 | non_dec > t_max) {
    stop("non_dec_time larger than t_max or smaller than 0!")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }
  if (sd_non_dec <= dt) {
    stop("sd_non_dec should not be smaller than dt!")
  }

  d_val <- stats::dnorm(t_vec, non_dec, sd_non_dec)
  p_u <- stats::pnorm(t_max, non_dec, sd_non_dec)
  p_l <- stats::pnorm(0, non_dec, sd_non_dec)
  d_nt <- d_val / (p_u - p_l)
  return(d_nt)
}
