# ==== Standard Ratcliff Diffusion Model

#' Create a basic Diffusion Model
#'
#' This function creates a [dRiftDM::drift_dm] that corresponds to the basic
#' Ratcliff diffusion model
#'
#' @param var_non_dec,var_start logical, indicating whether the model should have
#' a (uniform) variable non-decision time or starting point, respectively
#' (see `nt_uniform` and `x_uniform` in [dRiftDM::component_shelf])
#'
#' @param obs_data data.frame, an optional data.frame with the observed data.
#' See [dRiftDM::set_obs_data].
#' @param sigma,t_max,dt,dx numeric, providing the settings for the diffusion
#' constant and discretization (see [dRiftDM::drift_dm])
#'
#' @details
#'
#' The Ratcliff diffusion model is a diffusion model with  a constant drift rate
#' `muc` and a constant boundary `b`. If `var_non_dec = FALSE`,  a constant
#' non-decision time `non_dec` is assumed, otherwise a uniform non-decision time
#' with mean `non_dec` and range `range_non_dec`. If `var_start = FALSE`,  a
#' constant starting point centered between the boundaries is assumed (i.e.,
#' a dirac delta over 0), otherwise a uniform starting point with mean 0 and
#' range `range_start`.
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
    obs_data = obs_data, sigma = sigma, t_max = t_max, dt = dt, dx = dx,
    mu_fun = mu_constant, mu_int_fun = mu_int_constant, x_fun = x_dirac_0,
    b_fun = b_constant, dt_b_fun = dt_b_constant, nt_fun = nt_constant
  )

  # set other functions if requested
  if (var_non_dec) {
    r_dm <- set_comp_funs(
      drift_dm_obj = r_dm,
      comp_funs = list(nt_fun = nt_uniform)
    )
  }

  if (var_start) {
    r_dm <- set_comp_funs(
      drift_dm_obj = r_dm,
      comp_funs = list(x_fun = x_uniform)
    )
  }

  class(r_dm) <- c("ratcliff_dm", class(r_dm))

  return(r_dm)
}

# ==== Standard Diffusion Model Components

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
  x <- stats::dunif(x = x_vec, min = 0 - range_start / 2, max = 0 + range_start / 2)
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
  dt <- prms_solve[["dt"]]
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


  d_nt <- stats::dunif(
    x = t_vec, min = non_dec - range_non_dec / 2,
    max = non_dec + range_non_dec / 2
  )
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
#' @param sigma,t_max,dt,dx numeric, providing the settings for the diffusion
#' constant and discretization (see [dRiftDM::drift_dm])
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
#' @references
#' \insertRef{Ulrichetal.2015}{dRiftDM}
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
    obs_data = obs_data, sigma = sigma, t_max = t_max, dt = dt, dx = dx,
    mu_fun = mu_dmc, mu_int_fun = mu_int_dmc, x_fun = x_beta,
    b_fun = b_constant, dt_b_fun = dt_b_constant, nt_fun = nt_truncated_normal
  )
  class(dmc_dm) <- c("dmc_dm", class(dmc_dm))

  return(dmc_dm)
}

# ====
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




# === Shrinking Spotlight Model
#' Create the Shrinking Spotlight Model
#'
#' @description
#' This function creates a [dRiftDM::drift_dm] object that corresponds to a
#' simple version of the shrinking spotlight model by
#' \insertCite{Whiteetal.2011;textual}{dRiftDM}.
#'
#' @param obs_data data.frame, an optional data.frame with the observed data.
#' See [dRiftDM::set_obs_data].
#' @param sigma,t_max,dt,dx numeric, providing the settings for the diffusion
#' constant and discretization (see [dRiftDM::drift_dm])
#'
#' @details
#'
#' The shrinking spotlight model is a model developed for the flanker task.
#'
#' It has the following properties (see [dRiftDM::component_shelf]):
#' - a constant boundary (parameter `b`)
#' - a constant starting point in between the decision boundaries
#' - an evidence accumulation process that is driven by an attentional
#' spotlight that covers both the flankers and the target. The area that covers
#' the flankers and target is modeled by normal distribution with mean 0:
#'    - At the beginning of the trial attention is wide-spread, and the width
#'    at t=0 is the standard deviation `sd_0`
#'    - As the trial progresses in time, the attentional spotlight narrows,
#'    reflected by a linear decline of the standard deviation with rate `r`
#'    (to a minimum of 0.001).
#'    - the attention attributed to both the flankers and the target is scaled
#'    by `p` which controls the strength of evidence accumulation
#' - A non-decision time that follows a truncated normal distribution with
#'   mean `non_dec` and standard deviation `sd_non_dec`.
#'
#' Its parameters are:  `b`, `non_dec`, `sd_non_dec`, `p`, `sd_0`, and `r`.
#' Per default, all parameters are assumed to be "free".
#'
#' @references
#' \insertRef{Whiteetal.2011}{dRiftDM}
#'
#'
#' @export
ssp_dm <- function(obs_data = NULL, sigma = 1, t_max = 3, dt = .001,
                   dx = .001) {
  prms_model <- c(
    b = .6, non_dec = .3, sd_non_dec = .005, p = 3.3, sd_0 = 1.2,
    r = 10
  )
  conds <- c("comp", "incomp")

  ssp_dm <- drift_dm(
    prms_model = prms_model, conds = conds,
    free_prms = NULL, obs_data = obs_data, sigma = sigma,
    t_max = t_max, dt = dt, dx = dx, mu_fun = mu_ssp,
    mu_int_fun = dummy_t, x_fun = x_dirac_0, b_fun = b_constant,
    dt_b_fun = dt_b_constant, nt_fun = nt_truncated_normal
  )

  class(ssp_dm) <- c("ssp_dm", class(ssp_dm))

  return(ssp_dm)
}

# ===
# SSP Components

mu_ssp <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
  # extract all parameters
  p <- prms_model[["p"]]
  sd_0 <- prms_model[["sd_0"]]
  r <- prms_model[["r"]]


  if (!is.numeric(p) | length(p) != 1) {
    stop("p is not a single number")
  }
  if (!is.numeric(sd_0) | length(sd_0) != 1) {
    stop("sd_0 is not a single number")
  }
  if (!is.numeric(r) | length(r) != 1) {
    stop("r is not a single number")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }

  stopifnot(one_cond %in% c("comp", "incomp"))


  sd_t <- pmax(sd_0 - r * t_vec, 0.001)
  a_tar_t <- (stats::pnorm(q = 0.5, mean = 0, sd = sd_t) - 0.5) * 2
  a_fl_t <- 1 - a_tar_t

  # pass back the drift rate, depending on the condition
  if (one_cond == "comp") {
    mu_t <- a_tar_t * p + a_fl_t * p
  } else {
    mu_t <- a_tar_t * p - a_fl_t * p
  }
  return(mu_t)
}




# === ADDITIONAL MODEL COMPONENTS

# Collapsing Boundary - Hyperbolic Ratio Function
b_hyperbol <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
  b0 <- prms_model[["b0"]]
  kappa <- prms_model[["kappa"]]
  t05 <- prms_model[["t05"]]

  if (!is.numeric(b0) | length(b0) != 1) {
    stop("b0 is not a single number")
  }
  if (!is.numeric(kappa) | length(kappa) != 1) {
    stop("kappa is not a single number")
  }
  if (!is.numeric(t05) | length(t05) != 1) {
    stop("t05 is not a single number")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }

  return(b0 * (1 - kappa * t_vec / (t_vec + t05)))
}

# derivative of the hyperbolic ratio function
dt_b_hyperbol <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
  b0 <- prms_model[["b0"]]
  kappa <- prms_model[["kappa"]]
  t05 <- prms_model[["t05"]]

  if (!is.numeric(b0) | length(b0) != 1) {
    stop("b0 is not a single number")
  }
  if (!is.numeric(kappa) | length(kappa) != 1) {
    stop("kappa is not a single number")
  }
  if (!is.numeric(t05) | length(t05) != 1) {
    stop("t05 is not a single number")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }

  return(-(b0 * kappa * t05) / (t_vec + t05)^2)
}


# Collapsing Boundary - Weibull Function
b_weibull <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
  b0 <- prms_model[["b0"]]
  lambda <- prms_model[["lambda"]]
  k <- prms_model[["k"]]
  kappa <- prms_model[["kappa"]]

  if (!is.numeric(b0) | length(b0) != 1) {
    stop("b0 is not b0 single number")
  }
  if (!is.numeric(lambda) | length(lambda) != 1) {
    stop("lambda is not a single number")
  }
  if (!is.numeric(k) | length(k) != 1) {
    stop("k is not a single number")
  }
  if (!is.numeric(kappa) | length(kappa) != 1) {
    stop("kappa is not a single number")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }

  return(b0 - (1 - exp(-(t_vec / lambda)^k)) * kappa * b0)
}


# derivative of the collapsing boundary - Weibull Function
dt_b_weibull <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
  b0 <- prms_model[["b0"]]
  lambda <- prms_model[["lambda"]]
  k <- prms_model[["k"]]
  kappa <- prms_model[["kappa"]]

  if (!is.numeric(b0) | length(b0) != 1) {
    stop("b0 is not b0 single number")
  }
  if (!is.numeric(lambda) | length(lambda) != 1) {
    stop("lambda is not a single number")
  }
  if (!is.numeric(k) | length(k) != 1) {
    stop("k is not a single number")
  }
  if (!is.numeric(kappa) | length(kappa) != 1) {
    stop("kappa is not a single number")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }

  numer <- -b0 * kappa * k * (t_vec / lambda)^(k - 1) * exp(-(t_vec / lambda)^k)
  return(numer / lambda)
}




# === COMPONENT SHELF
#' Diffusion model components
#'
#' @description
#' This function is meant as a convenient way to access pre-built
#' model component functions. It returns a list of the following functions.
#'
#' * `mu_constant`, provides the component function for a constant
#' drift rate with parameter `muc`.
#'
#' * `mu_dmc`, provides the superimposed diffusion process
#'  of DMC. Necessary parameters are `muc` (drift rate of the controlled
#'  process), `a` (shape..), `A` (amplitude...), `tau` (scale of the
#'  automatic process).
#'
#' * `mu_ssp`, provides the drift rate for SSP.
#'   Necessary parameters are `p` (perceptual input of flankers and
#'  target), `sd_0` (initial spotlight width), `r` (shrinking rate of the
#'  spotlight). Note that no `mu_int_ssp` exists.
#'
#'
#' * `mu_int_constant`, provides the complementary integral to `mu_constant`.
#'
#' * `mu_int_dmc`, provides the complementary integral to `mu_dmc`.
#'
#' * `x_dirac_0`, provides a dirac delta for a starting point
#' centered between the boundaries (now parameter required).
#'
#' * `x_uniform`, provides a uniform distribution for a start point
#'  centered between the boundaries. Requires a parameter `range_start`
#'  (between 0 and 2).
#'
#' * `x_beta`, provides the function component for a symmetric
#' beta-shaped starting point distribution with parameter `alpha`.
#'
#' * `b_constant`, provides a constant
#' boundary with parameter `b`.
#'
#' * `b_hyperbol`, provides a collapsing boundary in terms of a
#'   hyperbolic ratio function with parameters
#'  `b0` as the initial value of the (upper) boundary,
#'  `kappa` the size of the collapse, and `t05` the point in time where
#'  the boundary has collapsed by half.
#'
#' * `b_weibull`, provides a collapsing boundary in terms of a
#'   Weibull distribution with parameters
#'  `b0` as the initial value of the (upper) boundary,
#'  `lambda` controlling the time of the collapse,
#'  `k` the shape of the collapse, and `kappa` the size of the collapse.
#'
#' * `dt_b_constant`, the first derivative of `b_constant`.
#'
#' * `dt_b_hyperbol`, the first derivative of `b_hyperbol`.
#'
#' * `nt_constant`, provides a constant
#' non-decision time with parameter `non_dec`.
#'
#' * `nt_uniform`, provides a uniform distribution for the
#' non-decision time. Requires the parameters `non_dec` and `range_non_dec`.
#'
#' * `nt_truncated_normal`, provides the component function for
#' a normally distributed non-decision time with parameters `non_dec`,
#' `sd_non_dec`. The Distribution is truncated to \eqn{[0, t_{max}]}.
#'
#' * `dummy_t` a function that accepts all required arguments for `mu_fun` or
#' `mu_int_fun` but which throws an error. Might come in handy when a user
#' doesn't require a drift rate or its integral.
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
  components$mu_ssp <- mu_ssp


  # mus int
  components$mu_int_constant <- mu_int_constant
  components$mu_int_dmc <- mu_int_dmc

  # xs
  components$x_dirac_0 <- x_dirac_0
  components$x_beta <- x_beta
  components$x_uniform <- x_uniform

  # bs
  components$b_constant <- b_constant
  components$b_hyperbol <- b_hyperbol
  components$b_weibull <- b_weibull

  # bs dt
  components$dt_b_constant <- dt_b_constant
  components$dt_b_hyperbol <- dt_b_hyperbol
  components$dt_b_weibull <- dt_b_weibull

  # nts
  components$nt_constant <- nt_constant
  components$nt_uniform <- nt_uniform
  components$nt_truncated_normal <- nt_truncated_normal

  # dummies
  components$dummy_t <- dummy_t

  return(components)
}


dummy_t <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
  stop("dummy_t: this should not be called!")
}
