
# Standard Ratcliff Diffusion Model ---------------------------------------


#' Create a Basic Diffusion Model
#'
#' This function creates a [dRiftDM::drift_dm] model that corresponds to the
#' basic Ratcliff Diffusion Model
#'
#' @param var_non_dec,var_start,var_drift logical, indicating whether the model
#'   should have a (uniform) variable non-decision time, starting point, or
#'   (normally-distributed) variable drift rate.
#'   (see also `nt_uniform` and `x_uniform` in [dRiftDM::component_shelf])
#' @param instr optional string with "instructions", see
#'   [dRiftDM::modify_flex_prms()].
#' @param obs_data data.frame, an optional data.frame with the observed data.
#' See [dRiftDM::obs_data].
#' @param sigma,t_max,dt,dx numeric, providing the settings for the diffusion
#' constant and discretization (see [dRiftDM::drift_dm])
#' @param b_coding list, an optional list with the boundary encoding (see
#' [dRiftDM::b_coding])
#'
#' @details
#'
#' The Ratcliff Diffusion Model is a diffusion model with a constant drift rate
#' `muc` and a constant boundary `b`. If `var_non_dec = FALSE`,  a constant
#' non-decision time `non_dec` is assumed, otherwise a uniform non-decision time
#' with mean `non_dec` and range `range_non_dec`. If `var_start = FALSE`,  a
#' constant starting point centered between the boundaries is assumed (i.e.,
#' a dirac delta over 0), otherwise a uniform starting point with mean 0 and
#' range `range_start`.
#'
#' @seealso [dRiftDM::component_shelf()], [dRiftDM::drift_dm()]
#'
#' @export
ratcliff_dm <- function(var_non_dec = FALSE, var_start = FALSE,
                        var_drift = FALSE, instr = NULL, obs_data = NULL,
                        sigma = 1, t_max = 3, dt = .001, dx = .001,
                        b_coding = NULL) {
  prms_model <- c(muc = 3, b = 0.6, non_dec = 0.3)
  if (var_non_dec) prms_model <- append(prms_model, c(range_non_dec = 0.05))
  if (var_start) prms_model <- append(prms_model, c(range_start = 0.5))
  if (var_drift) prms_model <- append(prms_model, c(sd_muc = 1))

  conds <- "null"
  r_dm <- drift_dm(
    prms_model = prms_model, conds = conds, subclass = "ratcliff_dm",
    instr = instr, obs_data = obs_data, sigma = sigma, t_max = t_max, dt = dt,
    dx = dx, mu_fun = mu_constant, mu_int_fun = mu_int_constant,
    x_fun = x_dirac_0, b_fun = b_constant, dt_b_fun = dt_b_constant,
    nt_fun = nt_constant, b_coding = b_coding
  )

  # set other functions if requested
  if (var_non_dec) {
    comp_funs(r_dm)[["nt_fun"]] <- nt_uniform
  }
  if (var_start) {
    comp_funs(r_dm)[["x_fun"]] <- x_uniform
  }

  return(r_dm)
}


# STANDARD DIFFUSION MODEL COMPONENTS -------------------------------------


#' Constant Drift Rate
#'
#' @param prms_model the model parameters, containing muc
#' @param prms_solve solver settings
#' @param t_vec time space
#' @param one_cond one condition
#' @param ddm_opts optional arguments attached to an object
#'
#' @returns a vector of the same length as t_vec with the drift rate for
#' each element of the vector.
#'
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


#' Integral of Constant Drift Rate
#'
#' @param prms_model the model parameters, containing muc
#' @param prms_solve solver settings
#' @param t_vec time space
#' @param one_cond one condition
#' @param ddm_opts optional arguments attached to an object
#'
#' @returns a vector calculated as t_vec*muc
#'
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


#' Constant Starting Point at Zero
#'
#' A dirac delta on zero, to provide no bias and a constant starting point
#'
#' @param prms_model the model parameters; no prm name required
#' @param prms_solve solver settings
#' @param x_vec evidence space
#' @param one_cond one condition
#' @param ddm_opts optional arguments attached to an object
#'
#' @returns a vector of the same length as x_vec with zeros, except for the
#' element in the middle of the vector
#'
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

#' Uniform Starting Point Distribution Centered Around Zero
#'
#'
#' @param prms_model the model parameters; prm name "range_start" required
#' @param prms_solve solver settings
#' @param x_vec evidence space
#' @param one_cond one condition
#' @param ddm_opts optional arguments attached to an object
#'
#' @returns returns the PDF of a uniform distribution for x_vec, centered around
#' zero and with a range of "range_start".
#'
x_uniform <- function(prms_model, prms_solve, x_vec, one_cond, ddm_opts) {
  range_start <- prms_model[["range_start"]]
  dx <- prms_solve[["dx"]]
  if (!is.numeric(range_start) | length(range_start) != 1) {
    stop("parameter range_start is not a single number")
  }
  if (!is.numeric(x_vec) | length(x_vec) <= 1) {
    stop("x_vec is not a vector")
  }

  # uniform around staring point of 0
  x <- stats::dunif(
    x = x_vec,
    min = 0 - range_start / 2,
    max = 0 + range_start / 2
  )
  x <- x / (sum(x) * dx) # ensure it integrates to 1
  return(x)
}


#' Constant Boundary
#'
#' @param prms_model the model parameters, containing b
#' @param prms_solve solver settings
#' @param t_vec time space
#' @param one_cond one condition
#' @param ddm_opts optional arguments attached to an object
#'
#' @returns a vector of the same length as t_vec with the b value for
#' each element of the vector.
#'
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


#' Derivative of a Constant Boundary
#'
#' @param prms_model the model parameters, no prm label required
#' @param prms_solve solver settings
#' @param t_vec time space
#' @param one_cond one condition
#' @param ddm_opts optional arguments attached to an object
#'
#' @returns a vector of the same length as t_vec only zeros.
#'
dt_b_constant <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
  # constant boundary
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }
  dt_b <- rep(0, length(t_vec))
  return(dt_b)
}


#' Constant Non-Decision time
#'
#' A dirac delta on "non_dec", to provide a constant non-decision time.
#'
#' @param prms_model the model parameters; containing "non_dec"
#' @param prms_solve solver settings
#' @param t_vec time space
#' @param one_cond one condition
#' @param ddm_opts optional arguments attached to an object
#'
#' @returns a vector of the same length as t_vec with zeros, except for the
#' element matching with "non_dec" with respect to "t_vec"
#'
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


#' Uniform Non-Decision Time
#'
#'
#' @param prms_model the model parameters; including "non_dec" and
#' "range_non_dec"
#' @param prms_solve solver settings
#' @param t_vec time space
#' @param one_cond one condition
#' @param ddm_opts optional arguments attached to an object
#'
#' @returns returns the PDF of a uniform distribution for t_vec, centered around
#' "non_dec" and with a range of "range_non_dec".
#'
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


# DIFFUSION MODEL FOR CONFLICT TASKS --------------------------------------

#' Create the Diffusion Model for Conflict Tasks
#'
#' @description
#' This function creates a [dRiftDM::drift_dm] object that corresponds to the
#' Diffusion Model for Conflict Tasks by
#' \insertCite{Ulrichetal.2015;textual}{dRiftDM}.
#'
#' @param var_non_dec,var_start logical, indicating whether the model should
#'   have a normally-distributed non-decision time or beta-shaped starting point
#'   distribution, respectively.
#'   (see `nt_truncated_normal` and `x_beta` in [dRiftDM::component_shelf]).
#'   Defaults are `TRUE`. If `FALSE`, a constant non-decision time and
#'   starting point is set (see `nt_constant` and `x_dirac_0` in
#'   [dRiftDM::component_shelf]).
#' @param instr optional string with additional "instructions", see
#'   [dRiftDM::modify_flex_prms()] and the Details below.
#' @param obs_data data.frame, an optional data.frame with the observed data.
#' See [dRiftDM::obs_data].
#' @param sigma,t_max,dt,dx numeric, providing the settings for the diffusion
#' constant and discretization (see [dRiftDM::drift_dm])
#' @param b_coding list, an optional list with the boundary encoding (see
#' [dRiftDM::b_coding])
#'
#'
#' @details
#'
#' The Diffusion Model for Conflict Tasks is a model for describing conflict
#' tasks like the Stroop, Simon, or flanker task.
#'
#' It has the following properties (see [dRiftDM::component_shelf]):
#' - a constant boundary (parameter `b`)
#' - an evidence accumulation process that results from the sum of two
#'    subprocesses:
#'    - a controlled process with drift rate `muc`
#'    - a gamma-shaped process with a scale parameter `tau`, a shape
#'    parameter `a`, and an amplitude `A`.
#'
#' If `var_non_dec = TRUE`, a (truncated) normally distributed non-decision with
#' mean `non_dec` and standard deviation `sd_non_dec` is assumed. If
#' `var_start = TRUE`,  a beta-shaped starting point distribution is assumed
#' with shape and scale parameter `alpha`.
#'
#' If `var_non_dec = TRUE`, a constant non-decision time at `non_dec` is set. If
#' `var_start = FALSE`, a starting point centered between the boundaries is
#' assumed (i.e., a dirac delta over 0).
#'
#'
#' Per default the shape parameter `a` is set to 2 and not allowed to
#' vary. The model assumes the amplitude `A` to be negative for
#' incompatible trials. Also, the model contains the custom parameter
#' `peak_l`, containing the peak latency (`(a-2)*tau`).
#'
#' @references
#' \insertRef{Ulrichetal.2015}{dRiftDM}
#'
#' @export
dmc_dm <- function(var_non_dec = TRUE, var_start = TRUE, instr = NULL,
                   obs_data = NULL, sigma = 1, t_max = 3,
                   dt = .001, dx = .001, b_coding = NULL) {

  # get default instructions to setup the configuration of DMC
  default_instr = 'peak_l := (a-1) * tau
                   a <!>
                   A ~ incomp == -(A ~ comp)'

  if (is.null(instr)) {
    instr = default_instr
  } else {
    instr = paste(default_instr, instr, sep = "\n")
  }

  # get all parameters, and maybe throw away those that are not needed
  prms_model <- c(
    muc = 4, b = .6, non_dec = .3, sd_non_dec = .02, tau = .04,
    a = 2, A = .1, alpha = 4
  )

  if (!var_non_dec) {
    prms_model <- prms_model[!(names(prms_model) == "sd_non_dec")]
  }
  if (!var_start) {
    prms_model <- prms_model[!(names(prms_model) == "alpha")]
  }

  # get the conds and call the backbone
  conds <- c("comp", "incomp")
  dmc_dm <- drift_dm(
    prms_model = prms_model, conds = conds, subclass = "dmc_dm", instr = instr,
    obs_data = obs_data, sigma = sigma, t_max = t_max, dt = dt, dx = dx,
    mu_fun = mu_dmc, mu_int_fun = mu_int_dmc, x_fun = x_beta,
    b_fun = b_constant, dt_b_fun = dt_b_constant, nt_fun = nt_truncated_normal,
    b_coding = b_coding
  )

  # replace component functions, if desired
  if (!var_non_dec) {
    comp_funs(dmc_dm)[["nt_fun"]] = nt_constant
  }
  if (!var_start) {
    comp_funs(dmc_dm)[["x_fun"]] = x_dirac_0
  }

  return(dmc_dm)
}



# DMC COMPONENT FUNTIONS --------------------------------------------------


#' Drift Rate for DMC
#'
#' Provides the drift rate of the superimposed decision process. That is
#' the derivative of the rescaled gamma function plus a constant drift rate for
#' the controlled process.
#'
#' @param prms_model the model parameters, containing muc, tau, a, A
#' @param prms_solve solver settings
#' @param t_vec time space
#' @param one_cond one condition
#' @param ddm_opts optional arguments attached to an object
#'
#' @returns provides the first derivative of the superimposed process with
#' respect to t_vec.
#'
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
  return(muc + mua)
}

#' Integral of DMC's Drift Rate
#'
#' Provides the integral of the drift rate of the superimposed decision process.
#' This is the sum of the rescaled gamma function and the linear function.
#'
#' @param prms_model the model parameters, containing muc, tau, a, A
#' @param prms_solve solver settings
#' @param t_vec time space
#' @param one_cond one condition
#' @param ddm_opts optional arguments attached to an object
#'
#' @returns provides the scaled gamma distribution function of the superimposed
#' process for each time step in t_vec.
#'
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

  return(muc + mua)
}


#' Beta-Shaped Starting Point Distribution Centered Around Zero
#'
#'
#' @param prms_model the model parameters; prm name "alpha" required
#' @param prms_solve solver settings
#' @param x_vec evidence space
#' @param one_cond one condition
#' @param ddm_opts optional arguments attached to an object
#'
#' @returns returns the PDF of a beta-shaped distribution for x_vec, centered
#' around zero and with a shape parameter "alpha".
#'
x_beta <- function(prms_model, prms_solve, x_vec, one_cond, ddm_opts) {
  alpha <- prms_model[["alpha"]]
  dx <- prms_solve[["dx"]]

  if (!is.numeric(alpha) | length(alpha) != 1) {
    stop("parameter alpha is not a single number")
  }
  if (!is.numeric(x_vec) | length(x_vec) <= 1) {
    stop("x_vec is not a vector")
  }

  xx <- seq(0, 1, length.out = length(x_vec))
  x <- stats::dbeta(xx, alpha, alpha) / 2
  x <- x / (sum(x) * dx) # ensure it integrates to 1

  return(x)
}


#' Truncated Normally-Distributed Non-Decision Time
#'
#'
#' @param prms_model the model parameters; including "non_dec" and
#' "sd_non_dec"
#' @param prms_solve solver settings
#' @param t_vec time space
#' @param one_cond one condition
#' @param ddm_opts optional arguments attached to an object
#'
#' @returns returns the PDF of a truncated normal distribution for t_vec, with
#' mean "non_dec" and standard deviation "sd_non_dec". Lower truncation is 0.
#' Upper truncation is max(t_vec)
#'
nt_truncated_normal <- function(prms_model, prms_solve, t_vec, one_cond,
                                ddm_opts) {
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
  d_nt <- d_val / (sum(d_val) * dt) # ensure it integrates to 1

  return(d_nt)
}





# SHRINKING SPOTLIGHT MODEL -----------------------------------------------


#' Create the Shrinking Spotlight Model
#'
#' @description
#' This function creates a [dRiftDM::drift_dm] object that corresponds to a
#' simple version of the shrinking spotlight model by
#' \insertCite{Whiteetal.2011;textual}{dRiftDM}.
#'
#' @param instr optional string with additional "instructions", see
#'   [dRiftDM::modify_flex_prms()] and the Details below.
#' @param obs_data data.frame, an optional data.frame with the observed data.
#' See [dRiftDM::obs_data].
#' @param sigma,t_max,dt,dx numeric, providing the settings for the diffusion
#' constant and discretization (see [dRiftDM::drift_dm])
#' @param b_coding list, an optional list with the boundary encoding (see
#' [dRiftDM::b_coding])
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
#' - The model also contains the auxiliary parameter `sign`, which is used to
#'   control the influence of the flankers across conditions. It is not really
#'   a parameter and should not be estimated!
#'
#' Per default, the parameter `r` is assumed to be fixed (i.e., is not estimated
#' freely). The model also contains the custom parameter `interf_t`, quantifying
#' the interference time (`sd_0 / r`)
#'
#' @references
#' \insertRef{Whiteetal.2011}{dRiftDM}
#'
#'
#' @export
ssp_dm <- function(instr = NULL, obs_data = NULL, sigma = 1, t_max = 3,
                   dt = .001, dx = .001, b_coding = NULL) {

  # sign ~ ensures that sign is free, and thus avoids a message from
  # modify_flex_prms
  default_instr = 'interf_t := sd_0 / r
                   r <!>
                   sign ~
                   sign ~ incomp => -1
                   sign <!>'

  if (is.null(instr)) {
    instr = default_instr
  } else {
    instr = paste(default_instr, instr, sep = "\n")
  }


  prms_model <- c(
    b = .6, non_dec = .3, sd_non_dec = .005, p = 3.3, sd_0 = 1.2,
    r = 10, sign = 1
  )
  conds <- c("comp", "incomp")

  ssp_dm <- drift_dm(
    prms_model = prms_model, conds = conds, subclass = "ssp_dm", instr = instr,
    obs_data = obs_data, sigma = sigma,
    t_max = t_max, dt = dt, dx = dx, mu_fun = mu_ssp,
    mu_int_fun = dummy_t, x_fun = x_dirac_0, b_fun = b_constant,
    dt_b_fun = dt_b_constant, nt_fun = nt_truncated_normal,
    b_coding = b_coding
  )


  return(ssp_dm)
}



# SSP COMPONENTS ----------------------------------------------------------

#' Drift Rate for SSP
#'
#' Provides the drift rate for the SSP model. That is, the sum of attention
#' attributed to the flankers and central target, scaled by the perceptual
#' input.
#'
#' @param prms_model the model parameters, containing p, sd_0, r, sign
#' @param prms_solve solver settings
#' @param t_vec time space
#' @param one_cond one condition
#' @param ddm_opts optional arguments attached to an object
#'
#' @returns provides the drift rate for SSP with respect to t_vec
#'
mu_ssp <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
  # extract all parameters
  p <- prms_model[["p"]]
  sd_0 <- prms_model[["sd_0"]]
  r <- prms_model[["r"]]
  sign <- prms_model[["sign"]]


  if (!is.numeric(p) | length(p) != 1) {
    stop("p is not a single number")
  }
  if (!is.numeric(sd_0) | length(sd_0) != 1) {
    stop("sd_0 is not a single number")
  }
  if (!is.numeric(r) | length(r) != 1) {
    stop("r is not a single number")
  }
  if (!is.numeric(sign) | length(sign) != 1) {
    stop("sign is not a single number")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }

  sd_t <- pmax(sd_0 - r * t_vec, 0.001)
  a_tar_t <- (stats::pnorm(q = 0.5, mean = 0, sd = sd_t) - 0.5) * 2
  a_fl_t <- 1 - a_tar_t

  # pass back the drift rate, depending on the condition
  mu_t <- a_tar_t * p + a_fl_t * p * sign

  return(mu_t)
}





# ADDITIONAL MODEL COMPONENTS ---------------------------------------------


#' Collapsing Boundary - Hyperbolic Ratio Function
#'
#' Provides the boundary, collapsing as a hyperbolic ratio function.
#'
#' @param prms_model the model parameters, containing b0, kappa, t05
#' @param prms_solve solver settings
#' @param t_vec time space
#' @param one_cond one condition
#' @param ddm_opts optional arguments attached to an object
#'
#' @details
#' `b_hyperbol` and `dt_b_hyperbol` provide the plain boundary values and the
#' respective derivative, respectively.
#'
#'
#' @returns a vector of the same length as t_vec with the boundary values (or
#' the deriviative) for each element of the vector.
#'
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

#' @rdname b_hyperbol
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


#' Collapsing Boundary - Weibull Function
#'
#' Provides the boundary, collapsing in accordance with a Weibull function
#'
#' @param prms_model the model parameters, containing b0, lambda, k, kappa
#' @param prms_solve solver settings
#' @param t_vec time space
#' @param one_cond one condition
#' @param ddm_opts optional arguments attached to an object
#'
#' @details
#' `b_weibull` and `dt_b_weibull` provide the plain boundary values and the
#' respective derivative, respectively.
#'
#'
#' @returns a vector of the same length as t_vec with the boundary values (or
#' the deriviative) for each element of the vector.
#'
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


#' @rdname b_weibull
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




# COMPONENT SHELF ---------------------------------------------------------


#' Diffusion Model Components
#'
#' @description
#' This function is meant as a convenient way to access pre-built
#' model component functions. It returns a list of the following functions.
#'
#' * `mu_constant`, provides the component function for a constant
#' drift rate with parameter `muc`.
#'
#' * `mu_dmc`, provides the drift rate of the superimposed diffusion process
#'  of DMC. Necessary parameters are `muc` (drift rate of the controlled
#'  process), `a` (shape..), `A` (amplitude...), `tau` (scale of the
#'  automatic process).
#'
#' * `mu_ssp`, provides the drift rate for SSP.
#'   Necessary parameters are `p` (perceptual input of flankers and
#'  target), `sd_0` (initial spotlight width), `r` (shrinking rate of the
#'  spotlight) and 'sign' (an auxiliary parameter for controlling the
#'  contribution of the flanker stimuli). Note that no `mu_int_ssp` exists.
#'
#'
#' * `mu_int_constant`, provides the complementary integral to `mu_constant`.
#'
#' * `mu_int_dmc`, provides the complementary integral to `mu_dmc`.
#'
#' * `x_dirac_0`, provides a dirac delta for a starting point
#' centered between the boundaries (no parameter required).
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
#' doesn't require the integral of the drift rate.
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

# see ?component_functions
dummy_t <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
  stop("dummy_t: this should not be called!")
}
