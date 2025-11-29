# Standard Ratcliff Diffusion Model ---------------------------------------

#' Create a Basic Diffusion Model
#'
#' This function creates a [dRiftDM::drift_dm] model that corresponds to the
#' basic Ratcliff Diffusion Model
#'
#' @param var_non_dec,var_start,var_drift logical, indicating whether the model
#'  should have a variable non-decision time , starting point (uniform), or
#'  drift rate (normally-distributed).  (see also `nt_uniform` and `x_uniform`
#'  in [dRiftDM::component_shelf])
#' @param instr optional string with "instructions", see
#'   [dRiftDM::modify_flex_prms()].
#' @param obs_data data.frame, an optional data.frame with the observed data.
#' See [dRiftDM::obs_data].
#' @param sigma,t_max,dt,dx numeric, providing the settings for the diffusion
#' constant and discretization (see [dRiftDM::drift_dm])
#' @param solver character, specifying the [dRiftDM::solver].
#' @param b_coding list, an optional list with the boundary encoding (see
#' [dRiftDM::b_coding])
#'
#' @details
#'
#' The classical Ratcliff Diffusion Model is a diffusion model with a constant
#' drift rate `muc` and a constant boundary `b`. If `var_non_dec = FALSE`,  a
#' constant non-decision time `non_dec` is assumed, otherwise a uniform
#' non-decision time with mean `non_dec` and range `range_non_dec`. If
#' `var_start = FALSE`,  a constant starting point centered between the
#' boundaries is assumed (i.e., a dirac delta over 0), otherwise a uniform
#' starting point with mean 0 and range `range_start`. If `var_drift = FALSE`,
#' a constant drift rate is assumed, otherwise a normally distributed drift rate
#' with mean `mu_c` and standard deviation `sd_muc` (can be computationally
#' intensive). Important: Variable drift rate is only possible with dRiftDM's
#' `mu_constant` function. No custom drift rate is yet possible in this case.
#'
#' @returns
#'
#' An object of type `drift_dm` (parent class) and `ratcliff_dm` (child class),
#' created by the function [dRiftDM::drift_dm()].
#'
#'
#' @examples
#' # the model with default settings
#' my_model <- ratcliff_dm()
#'
#' # the model with a variable non-decision time and with finer space
#' # discretization
#' my_model <- ratcliff_dm(var_non_dec = TRUE, dx = .01)
#'
#' @seealso [dRiftDM::component_shelf()], [dRiftDM::drift_dm()]
#'
#' @references
#' \insertRef{Ratcliff1978}{dRiftDM}
#' @export
ratcliff_dm <- function(
  var_non_dec = FALSE,
  var_start = FALSE,
  var_drift = FALSE,
  instr = NULL,
  obs_data = NULL,
  sigma = 1,
  t_max = 3,
  dt = .0075,
  dx = .02,
  solver = "kfe",
  b_coding = NULL
) {
  prms_model <- c(muc = 3, b = 0.6, non_dec = 0.3)
  if (var_non_dec) {
    prms_model <- append(prms_model, c(range_non_dec = 0.05))
  }
  if (var_start) {
    prms_model <- append(prms_model, c(range_start = 0.5))
  }
  if (var_drift) {
    prms_model <- append(prms_model, c(sd_muc = 1))
  }

  conds <- "null"
  r_dm <- drift_dm(
    prms_model = prms_model,
    conds = conds,
    subclass = "ratcliff_dm",
    instr = instr,
    obs_data = obs_data,
    sigma = sigma,
    t_max = t_max,
    dt = dt,
    dx = dx,
    mu_fun = mu_constant,
    mu_int_fun = mu_int_constant,
    x_fun = x_dirac_0,
    b_fun = b_constant,
    dt_b_fun = dt_b_constant,
    nt_fun = nt_constant,
    b_coding = b_coding,
    solver = solver
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
#' @keywords internal
mu_constant <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
  muc <- prms_model[["muc"]]
  if (!is.numeric(muc) | length(muc) != 1) {
    stop("parameter muc is not a single number")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a numeric vector with more than one entry")
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
#' @keywords internal
mu_int_constant <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
  muc <- prms_model[["muc"]]
  if (!is.numeric(muc) | length(muc) != 1) {
    stop("muc is not a single number")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a numeric vector with more than one entry")
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
#' @keywords internal
x_dirac_0 <- function(prms_model, prms_solve, x_vec, one_cond, ddm_opts) {
  dx <- prms_solve[["dx"]]
  if (!is.numeric(dx) | length(dx) != 1) {
    stop("dx is not a single number")
  }

  # starting point at 0
  if (!is.numeric(x_vec) | length(x_vec) <= 1) {
    stop("x_vec is not a numeric vector with more than one entry")
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
#' @keywords internal
x_uniform <- function(prms_model, prms_solve, x_vec, one_cond, ddm_opts) {
  range_start <- prms_model[["range_start"]]
  dx <- prms_solve[["dx"]]
  if (!is.numeric(range_start) | length(range_start) != 1) {
    stop("parameter range_start is not a single number")
  }
  if (!is.numeric(x_vec) | length(x_vec) <= 1) {
    stop("x_vec is not a numeric vector with more than one entry")
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
#' @keywords internal
b_constant <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
  b <- prms_model[["b"]]
  if (!is.numeric(b) | length(b) != 1) {
    stop("b is not a single number")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a numeric vector with more than one entry")
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
#' @keywords internal
dt_b_constant <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
  # constant boundary
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a numeric vector with more than one entry")
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
#' @keywords internal
nt_constant <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
  non_dec <- prms_model[["non_dec"]]
  t_max <- prms_solve[["t_max"]]
  dt <- prms_solve[["dt"]]
  if (!is.numeric(non_dec) | length(non_dec) != 1) {
    stop("non_dec is not a single number")
  }
  if (!is.numeric(t_max) | length(t_max) != 1) {
    stop("t_max is not a single number")
  }
  if (!is.numeric(dt) | length(dt) != 1) {
    stop("dt is not a single number")
  }

  if (non_dec < 0 | non_dec > t_max) {
    stop("non_dec larger than t_max or smaller than 0!")
  }

  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a numeric vector with more than one entry")
  }

  d_nt <- numeric(length(t_vec))
  which_index <- round(non_dec / dt)
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
#' @keywords internal
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
  if (!is.numeric(t_max) | length(t_max) != 1) {
    stop("t_max is not a single number")
  }
  if (!is.numeric(dt) | length(dt) != 1) {
    stop("dt is not a single number")
  }
  if (non_dec < 0 | non_dec > t_max) {
    stop("non_dec_time larger than t_max or smaller than 0!")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a numeric vector with more than one entry")
  }
  if (range_non_dec <= dt) {
    stop("range_non_dec should not be smaller than dt!")
  }

  d_nt <- stats::dunif(
    x = t_vec,
    min = non_dec - range_non_dec / 2,
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
#' @inheritParams ratcliff_dm
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
#' vary. This is because the derivative of the scaled gamma-distribution
#' function does not exist at `t = 0` for `a < 2`. Currently, we recommend
#' keeping `a` fixed to 2. If users decide to set `a != 2`,
#' then a small value of `tol = 0.001` (default) is added to the time vector
#' `t_vec` before calculating the derivative of the scaled gamma-distribution as
#' originally introduced by \insertCite{Ulrichetal.2015;textual}{dRiftDM}. Users
#' can control this value by passing a value via `ddm_opts()` (see the example
#' below). Note, however, that varying `a` can lead to large numerical
#' inaccuracies if `a` gets smaller.
#'
#' The model assumes the amplitude `A` to be negative for
#' incompatible trials. Also, the model contains the custom parameter
#' `peak_l`, containing the peak latency (`(a-2)*tau`).
#'
#' @returns
#'
#' An object of type `drift_dm` (parent class) and `dmc_dm` (child class),
#' created by the function [dRiftDM::drift_dm()].
#'
#' @note
#'
#' The scaling of the parameters in `dRiftDM` is different to
#' \insertCite{Ulrichetal.2015;textual}{dRiftDM}. This is because `dRiftDM`
#' works in seconds and with a diffusion constant of 1, while the original
#' DMC parameterization is in milliseconds and with a diffusion constant of 4.
#' We describe how to convert the parameters on our
#' [website](https://bucky2177.github.io/dRiftDM/articles/convert_dmc_parameters.html).
#'
#' @examples
#' # the model with default settings
#' my_model <- dmc_dm()
#'
#' # the model with no variability in the starting point and a finer
#' # discretization
#' my_model <- dmc_dm(var_start = FALSE, dt = .005, dx = .01)
#'
#' # we don't recommend this, but if you really want a != 2, just do...
#' # (see the Details for more warnings/information about this)
#' my_model <- dmc_dm(instr = "a ~!")
#' coef(my_model)["a"] <- 1.9
#' # -> if you want to control the small value that is added to t_vec when
#' # calculating the drift rate for a != 2, just use ...
#' ddm_opts(my_model) <- 0.0001 # ==> t_vec + 0.0001
#' ddm_opts(my_model) <- NULL # default ==> t_vec + 0.001
#'
#'
#' @references
#' \insertRef{Ulrichetal.2015}{dRiftDM}
#'
#' @export
dmc_dm <- function(
  var_non_dec = TRUE,
  var_start = TRUE,
  instr = NULL,
  obs_data = NULL,
  sigma = 1,
  t_max = 3,
  dt = .0075,
  dx = .02,
  b_coding = NULL
) {
  # get default instructions to setup the configuration of DMC
  default_instr <- "peak_l := (a-1) * tau
                   a <!>
                   A ~ incomp == -(A ~ comp)"

  if (is.null(instr)) {
    instr <- default_instr
  } else {
    instr <- paste(default_instr, instr, sep = "\n")
  }

  # get all parameters, and maybe throw away those that are not needed
  prms_model <- c(
    muc = 4,
    b = .6,
    non_dec = .3,
    sd_non_dec = .02,
    tau = .04,
    a = 2,
    A = .1,
    alpha = 4
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
    prms_model = prms_model,
    conds = conds,
    subclass = "dmc_dm",
    instr = instr,
    obs_data = obs_data,
    sigma = sigma,
    t_max = t_max,
    dt = dt,
    dx = dx,
    mu_fun = mu_dmc,
    mu_int_fun = mu_int_dmc,
    x_fun = x_beta,
    b_fun = b_constant,
    dt_b_fun = dt_b_constant,
    nt_fun = nt_truncated_normal,
    b_coding = b_coding
  )

  # replace component functions, if desired
  if (!var_non_dec) {
    comp_funs(dmc_dm)[["nt_fun"]] <- nt_constant
  }
  if (!var_start) {
    comp_funs(dmc_dm)[["x_fun"]] <- x_dirac_0
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
#' @keywords internal
mu_dmc <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
  # unpack values and conduct checks
  muc <- prms_model[["muc"]]
  tau <- prms_model[["tau"]]
  a <- prms_model[["a"]]
  A <- prms_model[["A"]]
  tol <- ddm_opts %||% 0.001

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
  if (!is.numeric(tol) | length(tol) != 1 | tol <= 0) {
    stop("optional parameter tol is not a single number larger than 0")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a numeric vector with more than one entry")
  }

  # calculate the first derivative of the gamma-function
  if (a != 2) {
    t_vec <- t_vec + tol # general form can not be derived for t <= 0
    mua <- A *
      exp(-t_vec / tau) *
      ((t_vec * exp(1)) / ((a - 1) * tau))^(a - 1) *
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
#' @keywords internal
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
    stop("t_vec is not a numeric vector with more than one entry")
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
#' @keywords internal
x_beta <- function(prms_model, prms_solve, x_vec, one_cond, ddm_opts) {
  alpha <- prms_model[["alpha"]]
  dx <- prms_solve[["dx"]]

  if (!is.numeric(alpha) | length(alpha) != 1) {
    stop("parameter alpha is not a single number")
  }
  if (!is.numeric(x_vec) | length(x_vec) <= 1) {
    stop("x_vec is not a numeric vector with more than one entry")
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
#' @keywords internal
nt_truncated_normal <- function(
  prms_model,
  prms_solve,
  t_vec,
  one_cond,
  ddm_opts
) {
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
  if (!is.numeric(t_max) | length(t_max) != 1) {
    stop("t_max is not a single number")
  }
  if (!is.numeric(dt) | length(dt) != 1) {
    stop("dt is not a single number")
  }
  if (non_dec < 0 | non_dec > t_max) {
    stop("non_dec_time larger than t_max or smaller than 0!")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a numeric vector with more than one entry")
  }
  if (sd_non_dec < dt) {
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
#' @param var_non_dec,var_start logical, indicating whether the model
#'   should have a variable non-decision time or starting point
#'   (see also `nt_uniform` and `x_uniform` in [dRiftDM::component_shelf]
#' @inheritParams ratcliff_dm
#'
#' @details
#'
#' The shrinking spotlight model is a model developed for the flanker task.
#'
#' It has the following properties (see [dRiftDM::component_shelf]):
#' - a constant boundary (parameter `b`)
#' - a constant starting point in between the decision boundaries
#' - an evidence accumulation process that is driven by an attention
#' spotlight that covers both the flankers and the target. The area that covers
#' the flankers and target is modeled by normal distribution with mean 0:
#'    - At the beginning of the trial attention is wide-spread, and the width
#'    at t=0 is the standard deviation `sd_0`
#'    - As the trial progresses in time, the attention spotlight narrows,
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
#' the interference time (`sd_0 / r`).
#'
#'
#' @returns
#'
#' An object of type `drift_dm` (parent class) and `ssp_dm` (child class),
#' created by the function [dRiftDM::drift_dm()].
#'
#' @note
#'
#' The parameters of SSP in `dRiftDM` differ in their size from the original
#' publication of \insertCite{Whiteetal.2011;textual}{dRiftDM}. `dRiftDM`
#' uses symmetrical boundaries around zero and a diffusion constant of 1.
#' In the original publication, SSP was parameterized with boundaries ranging
#' from zero to `a` and a diffusion constant of 0.1.
#'
#' Thus, in `dRiftDM`, the boundary `b` corresponds to \eqn{b = a/2 \cdot 10}.
#' Additionally, `p` in `dRiftDM` is 10 times larger than `p` in the original
#' publication. Finally, `r` is expressed in seconds, and thus `r` is 1000 times
#' larger in `dRiftDM` than in the original publication.
#'
#'
#' @examples
#' # the model with default settings
#' my_model <- ssp_dm()
#'
#' # the model with a finer discretization
#' my_model <- ssp_dm(dt = .0025, dx = .01)
#'
#' @references
#' \insertRef{Whiteetal.2011}{dRiftDM}
#'
#'
#' @export
ssp_dm <- function(
  var_non_dec = TRUE,
  var_start = FALSE,
  instr = NULL,
  obs_data = NULL,
  sigma = 1,
  t_max = 3,
  dt = .005,
  dx = .02,
  b_coding = NULL
) {
  # sign ~ ensures that sign is free, and thus avoids a message from
  # modify_flex_prms
  default_instr <- "interf_t := sd_0 / r
                   r <!>
                   sign ~
                   sign ~ incomp => -1
                   sign <!>"

  if (is.null(instr)) {
    instr <- default_instr
  } else {
    instr <- paste(default_instr, instr, sep = "\n")
  }

  # get all parameters, and maybe throw away those that are not needed
  prms_model <- c(
    b = .6,
    non_dec = .3,
    range_non_dec = .05,
    p = 3.3,
    sd_0 = 1.2,
    r = 10,
    range_start = .5,
    sign = 1
  )

  if (!var_non_dec) {
    prms_model <- prms_model[!(names(prms_model) == "range_non_dec")]
  }
  if (!var_start) {
    prms_model <- prms_model[!(names(prms_model) == "range_start")]
  }
  conds <- c("comp", "incomp")

  ssp_dm <- drift_dm(
    prms_model = prms_model,
    conds = conds,
    subclass = "ssp_dm",
    instr = instr,
    obs_data = obs_data,
    sigma = sigma,
    t_max = t_max,
    dt = dt,
    dx = dx,
    mu_fun = mu_ssp,
    mu_int_fun = dummy_t,
    x_fun = x_uniform,
    b_fun = b_constant,
    dt_b_fun = dt_b_constant,
    nt_fun = nt_uniform,
    b_coding = b_coding
  )

  # replace component functions, if desired
  if (!var_non_dec) {
    comp_funs(ssp_dm)[["nt_fun"]] <- nt_constant
  }
  if (!var_start) {
    comp_funs(ssp_dm)[["x_fun"]] <- x_dirac_0
  }

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
#' @keywords internal
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
    stop("t_vec is not a numeric vector with more than one entry")
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
#' @keywords internal
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
    stop("t_vec is not a numeric vector with more than one entry")
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
    stop("t_vec is not a numeric vector with more than one entry")
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
#' @keywords internal
b_weibull <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
  b0 <- prms_model[["b0"]]
  lambda <- prms_model[["lambda"]]
  k <- prms_model[["k"]]
  kappa <- prms_model[["kappa"]]

  if (!is.numeric(b0) | length(b0) != 1) {
    stop("b0 is not a single number")
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
    stop("t_vec is not a numeric vector with more than one entry")
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
    stop("b0 is not a single number")
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
    stop("t_vec is not a numeric vector with more than one entry")
  }

  numer <- -b0 * kappa * k * (t_vec / lambda)^(k - 1) * exp(-(t_vec / lambda)^k)
  return(numer / lambda)
}


# COMPONENT SHELF ---------------------------------------------------------

#' Diffusion Model Components
#'
#' @description
#' This function is meant as a convenient way to access pre-built
#' model component functions.
#'
#'
#' @details
#'
#' The function provides the following functions:
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
#' See \code{vignette("customize_ddms", "dRiftDM")} for more information on how
#' to set/modify/customize the components of a diffusion model.
#'
#' @returns
#' A list of the respective functions; each entry/function can be accessed by
#' "name" (see the Example and Details).
#'
#' @examples
#' pre_built_functions <- component_shelf()
#' names(pre_built_functions)
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


# FUNCTION FOR GETTING LOWER/UPPER/START ----------------------------------

#' Get Default Parameter Ranges for a Model
#'
#' `get_lower_upper()` returns suggested default values for parameter
#' bounds of a `drift_dm` model. The function inspects the model's component
#' functions (e.g., drift, boundary, non-decision time, start) and provides
#' heuristic defaults for some of the pre-built components. Only parameters
#' that are currently considered *free* in the model are returned.
#'
#' @param object a [dRiftDM::drift_dm] model.
#' @param warn a single logical, if `TRUE` issue a warning listing components
#'   and parameters where no defaults could be provided.
#' @param ... additional arguments passed forward to the respective method.
#'
#' @return a list with two named numeric vectors:
#'   - `lower` --- suggested lower bounds for free parameters
#'   - `upper` --- suggested upper bounds for free parameters
#'
#'
#' @details
#'
#' Supported components include: `mu_constant`, `mu_dmc`,
#' `mu_ssp`, `b_constant`, `x_uniform`, `x_beta`, `nt_constant`,
#' `nt_uniform`, `nt_truncated_normal`. For some defaults we use the model's
#' discretization (`dt`, `dx`) to ensure sensible minima.
#'
#' If a component is not recognized (or refers to currently unsupported
#' components), no defaults are provided for that component. When `warn = TRUE`,
#' a single warning lists components without defaults and any free parameters
#' that remain unmatched. In this case, the user has to add the missing
#' parameter ranges before attempting to fit the model.
#'
#' The default ranges are **heuristics** intended to provide a reasonable
#' starting point for new users. They are not guaranteed to be
#' appropriate for every model or data set. Always review and, if needed,
#' adjust the returned values as needed.
#'
#' @examples
#' # get a model for the example
#' model <- dmc_dm(obs_data = dmc_synth_data)
#'
#' # get the parameter ranges
#' lu <- get_lower_upper(model)
#' lu$lower
#' lu$upper
#'
#' # then continue to estimate
#' # estimate_dm(model, lower = lu$lower, upper = lu$upper, optimizer = "nmkb")
#'
#' @export
get_lower_upper <- function(object, ...) {
  UseMethod("get_lower_upper")
}

#' @rdname get_lower_upper
#' @export
get_lower_upper.drift_dm <- function(object, ..., warn = TRUE) {
  drift_dm_obj <- object
  dx <- unname(prms_solve(drift_dm_obj)["dx"])
  dt <- unname(prms_solve(drift_dm_obj)["dt"])

  # input checks
  stopifnot(is.logical(warn), length(warn) == 1)

  # now create container and iterate over each component
  prms_lower <- numeric(0)
  prms_upper <- numeric(0)
  all_comp_funs <- comp_funs(drift_dm_obj)
  fails <- character(0)
  for (one_comp in names(all_comp_funs)) {
    # extract the current component of the model
    comp <- all_comp_funs[[one_comp]]

    # check for various pre-built functions (not all currently)
    if (isTRUE(identical(comp, mu_constant))) {
      prms_lower <- c(prms_lower, muc = 0.5)
      prms_upper <- c(prms_upper, muc = 9)
    } else if (isTRUE(identical(comp, x_uniform))) {
      prms_lower <- c(prms_lower, range_start = max(dx + 0.005, 0.01))
      prms_upper <- c(prms_upper, range_start = 1.5)
    } else if (isTRUE(identical(comp, b_constant))) {
      prms_lower <- c(prms_lower, b = 0.15)
      prms_upper <- c(prms_upper, b = 1.20)
    } else if (isTRUE(identical(comp, nt_constant))) {
      prms_lower <- c(prms_lower, non_dec = 0.15)
      prms_upper <- c(prms_upper, non_dec = 0.60)
    } else if (isTRUE(identical(comp, nt_uniform))) {
      prms_lower <- c(
        prms_lower,
        non_dec = 0.15,
        range_non_dec = max(dt + 0.005, 0.01)
      )
      prms_upper <- c(
        prms_upper,
        non_dec = 0.60,
        range_non_dec = max(dt + 0.005, 0.4)
      )
    } else if (isTRUE(identical(comp, mu_dmc))) {
      prms_lower <- c(prms_lower, muc = 0.5, tau = 0.015, a = 1.2, A = 0.005)
      prms_upper <- c(prms_upper, muc = 9, tau = 0.25, a = 3, A = 0.3)
    } else if (isTRUE(identical(comp, x_beta))) {
      prms_lower <- c(prms_lower, alpha = 2)
      prms_upper <- c(prms_upper, alpha = 8)
    } else if (isTRUE(identical(comp, nt_truncated_normal))) {
      prms_lower <- c(
        prms_lower,
        non_dec = 0.15,
        sd_non_dec = max(dt, 0.005)
      )
      prms_upper <- c(
        prms_upper,
        non_dec = 0.6,
        sd_non_dec = max(dt + 0.005, 0.1)
      )
    } else if (isTRUE(identical(comp, mu_ssp))) {
      prms_lower <- c(prms_lower, p = 1, sd_0 = 0.5, r = 3)
      prms_upper <- c(prms_upper, p = 7, sd_0 = 3.2, r = 30)
    } else {
      # no match with a pre-built function; check if it refers to a
      # pre-built component that is defined above, or doesn't have a parameter
      # -> if so, skip, else through a warning
      l <- list(mu_int_constant, dt_b_constant, mu_int_dmc, x_dirac_0, dummy_t)
      checks <- vapply(
        l,
        \(one_fun) {
          isTRUE(identical(one_fun, comp))
        },
        FUN.VALUE = logical(1)
      )
      if (any(checks)) {
        next
      }
      fails <- c(fails, one_comp)
    }
  }

  # get those parameters that are free
  prms_conds <- prms_cond_combo(drift_dm_obj)
  free_prms <- unique(prms_conds[1, ])

  # check if users want a variable drift rate, and add corresponding defaults
  if (
    identical(all_comp_funs$mu_fun, mu_constant) &&
      ("sd_muc" %in% free_prms)
  ) {
    prms_lower <- c(prms_lower, sd_muc = 0.01)
    prms_upper <- c(prms_upper, sd_muc = 3.00)
  }
  diff_prms <- setdiff(free_prms, names(prms_lower))
  if (length(fails) == 0 & length(diff_prms) != 0) {
    stop("There are unexpected free parameters in your model.")
  }

  # warn if there are unmatched parameters and skipped components
  if (length(fails) > 0 && warn) {
    fails <- paste("-", paste("'", fails, "'", sep = ""), "\n")
    diff_prms <- paste("-", paste("'", diff_prms, "'", sep = ""), "\n")

    warning(
      "Cannot provide default values for the model components:\n",
      fails,
      "Please specify default values for... \n",
      diff_prms,
      "...before attempting to fit the model."
    )
  }

  # finally, use those parameters that are actually considered free and return
  prms_lower <- prms_lower[intersect(free_prms, names(prms_lower))]
  prms_upper <- prms_upper[intersect(free_prms, names(prms_upper))]
  return(list(lower = prms_lower, upper = prms_upper))
}


#' TITLE
#'
#' @param object
#'
#' @param ...
#'
#' @keywords internal
get_starting_values <- function(object, ...) {
  UseMethod("get_starting_values")
}

#' @rdname get_starting_values
#' @export
get_starting_values.drift_dm <- function(
  object,
  ...,
  lower = NULL,
  upper = NULL,
  verbose = 0,
  use_ez = NULL,
  n_lhs = NULL
) {
  drift_dm_obj <- object
  prms_model <- names(coef(drift_dm_obj))
  use_ez = use_ez %||% TRUE
  n_lhs = n_lhs %||% 10L
  verbose = verbose %||% 0

  # if ez is requested, try to provide corresponding starting values
  ez_guess <- numeric()
  if (use_ez && !is.null(object$obs_data)) {
    # 1.) find component functions that can be leveraged
    comp_funs <- comp_funs(drift_dm_obj)

    mu_funs <- list(mu_constant, mu_dmc, mu_ssp)
    check_mu <- sapply(mu_funs, \(x) isTRUE(all.equal(x, comp_funs$mu_fun)))
    check_mu <- any(check_mu)

    b_funs <- list(b_constant)
    check_b <- sapply(b_funs, \(x) isTRUE(all.equal(x, comp_funs$b_fun)))
    check_b <- any(check_b)

    nt_funs <- list(nt_uniform, nt_constant, nt_truncated_normal)
    check_non_dec <- sapply(nt_funs, \(x) {
      isTRUE(all.equal(x, comp_funs$nt_fun))
    })
    check_non_dec <- any(check_non_dec)

    # 2.) find ez diffusion parmaeters
    ez_mat <- get_ez_diffusion(drift_dm_obj)
    ez_mat <- ez_mat[c("muc", "b", "non_dec"), , drop = FALSE]
    ez_mat <- ez_mat[c(check_mu, check_b, check_non_dec), , drop = FALSE]

    # 3.) map to model (or skip)
    if (nrow(ez_mat) > 0) {
      ez_vec <- as.vector(ez_mat)
      names(ez_vec) <- outer(
        rownames(ez_mat),
        colnames(ez_mat),
        paste,
        sep = "."
      )

      # small helper to map
      ez_map <- function(p, ez_vec) {
        # average over all entries
        idx <- grepl(paste0("^", p, "\\."), names(ez_vec))
        res <- mean(ez_vec[idx])
        if (is.na(res)) {
          return(NA_real_)
        } else {
          return(res)
        }
      }

      ez_guess <- vapply(
        X = prms_model,
        FUN = ez_map,
        FUN.VALUE = numeric(1),
        USE.NAMES = FALSE,
        ez_vec = ez_vec
      )
      names(ez_guess) <- prms_model
      ez_guess <- ez_guess[!is.na(ez_guess)]

      if (verbose > 0) {
        message(
          "Using EZ-Diffusion estimates for: ",
          paste(names(ez_guess), collapse = ", ")
        )
      }
    }
  }

  # check if there are parameters left and return them (or NULL)
  rem_prms <- setdiff(prms_model, names(ez_guess))
  if (length(rem_prms) == 0 || n_lhs <= 0) {
    if (length(ez_guess) == 0) {
      return(NULL)
    } else {
      return(ez_guess)
    }
  }

  # perform a rough "grid search" for the remaining parameters
  # find lower and upper ranges to try out
  l_u = tryCatch(
    {
      get_lower_upper(drift_dm_obj)
    },
    warning = function(w) {
      return(NULL)
    },
    error = function(e) {
      return(NULL)
    }
  )
  # if this fails, and lower/upper are not both provided,
  # then return NULL to not perform an initial grid search
  if ((is.null(lower) || is.null(upper)) && is.null(l_u)) {
    return(NULL)
  }
  if (is.null(lower)) {
    lower <- l_u$lower
  }
  if (is.null(upper)) {
    upper <- l_u$upper
  }
  l_u = get_parameters_smart(
    drift_dm_obj = drift_dm_obj,
    input_a = lower,
    input_b = upper,
  )
  lower = l_u$vec_a
  upper = l_u$vec_b
  rem_lower = lower[rem_prms]
  rem_upper = upper[rem_prms]

  # now the actual sampling
  if (verbose > 0) {
    message(
      "Performing latin hypercube sampling (n_lhs = ",
      n_lhs,
      ") on: ",
      paste(rem_prms, collapse = ", ")
    )
  }

  # helper for hyper-cube sampling
  lhs_unit <- function(n, d) {
    X <- matrix(NA_real_, nrow = n, ncol = d)
    for (j in seq_len(d)) {
      # Random permutation of strata {0,1,...,n-1}
      strata <- sample.int(n) - 1L
      # One uniform draw within each stratum
      u <- stats::runif(n)
      # Map to the j-th column
      X[, j] <- (strata + u) / n
    }
    return(X)
  }

  # get samples
  d <- length(rem_lower)
  n <- n_lhs * d
  U <- lhs_unit(n, d)

  # map to parameter space
  prms_lhs <- sweep(U, 2, rem_upper - rem_lower, `*`) +
    matrix(rem_lower, nrow = n, ncol = d, byrow = TRUE)
  colnames(prms_lhs) <- names(rem_lower)

  # combine to one big matrix and evaluate n times (ensuring sorting)
  ez_prms <- matrix(ez_guess, nrow = n, ncol = length(ez_guess), byrow = TRUE)
  colnames(ez_prms) <- names(ez_guess)
  prms_lhs <- cbind(ez_prms, prms_lhs)
  stopifnot(ncol(prms_lhs) == length(prms_model))
  prms_lhs <- prms_lhs[, prms_model]
  cost_vals <- apply(prms_lhs, MARGIN = 1, \(x) {
    drift_dm_obj$flex_prms_obj <- x2prms_vals(x, drift_dm_obj$flex_prms_obj)
    drift_dm_obj <- re_evaluate_model(drift_dm_obj)
    return(drift_dm_obj$cost_value)
  })

  # final parameters and ensure they are not outside the limits
  final <- prms_lhs[which.min(cost_vals), ]
  nudge <- (upper - lower) * 0.05
  final <- ifelse(final <= lower, lower + nudge, final)
  final <- ifelse(final >= upper, upper - nudge, final)

  return(final)
}


#' Compute EZ Diffusion parameters
#'
#' Internal helper that computes EZ diffusion model parameters for each
#' condition in a `drift_dm_obj`. The computation is based on the equations
#' from Wagenmakers et al. (2007) and estimates drift rate (`muc`), boundary
#' separation (`b`), and non-decision time (`non_dec`).
#'
#' @param drift_dm_obj a drift diffusion model object containing observed data
#'   in `obs_data`, including upper (`rts_u`) and lower (`rts_l`) response times
#'   per condition
#'
#' @returns a matrix with rows `muc`, `b`, and `non_dec`
#'
#' @details
#' If `Pc` equals 0, 0.5, or 1, small adjustments are applied to prevent
#' numerical issues in the logit transformation.
#'
#' @keywords internal
get_ez_diffusion <- function(drift_dm_obj) {
  # if no data provided, return NULL
  if (is.null(drift_dm_obj$obs_data)) {
    return(NULL)
  }

  # helper to calculate for one case
  ez <- function(pc, vrt, mrt, s = 1) {
    stopifnot(0 <= pc, pc <= 1)
    stopifnot(0 < vrt, 0 < mrt, 0 < s)
    s2 = s^2
    # If Pc equals 0, .5, or 1, the method will not work, so I make a slight
    # adjustements
    adj <- 0.001
    tol <- 1e-6
    if (abs(pc - 1) < tol) {
      pc <- 1.0 - adj
    }
    if (abs(pc - 0.5) < tol) {
      pc <- 0.5 + adj
    }
    if (abs(pc - 0) < tol) {
      pc <- 0.0 + adj
    }
    L = stats::qlogis(pc)
    x = L * (L * pc^2 - L * pc + pc - .5) / vrt
    v = sign(pc - .5) * s * x^(1 / 4)
    a = s2 * stats::qlogis(pc) / v
    y = -v * a / s2
    mdt = (a / (2 * v)) * (1 - exp(y)) / (1 + exp(y))
    ter = mrt - mdt
    return(c(muc = v, b = a / 2, non_dec = ter))
  }

  all_conds = conds(drift_dm_obj)
  sigma <- prms_solve(drift_dm_obj)[["sigma"]]
  prms <- sapply(all_conds, \(x) {
    rts_u <- drift_dm_obj$obs_data$rts_u[[x]]
    rts_l <- drift_dm_obj$obs_data$rts_l[[x]]
    pc = length(rts_u) / (length(rts_u) + length(rts_l))
    vrt = stats::var(c(rts_u, rts_l))
    mrt = mean(c(rts_u, rts_l))
    ez(pc = pc, vrt = vrt, mrt = mrt, s = sigma)
  })
  return(prms)
}
