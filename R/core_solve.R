
# FUNCTIONS FOR GETTING THE PDF OF A MODEL --------------------------------



#' Calculate the PDFs
#'
#' This function takes the necessary vectors to derive the first passage
#' time (for one condition; subsetted before calling this function). It only
#' calls further functions, and doesn't perform any calculations
#'
#' @param solver a single character, specifying the approach to derive
#' the first passage time (i.e., "kfe" or "im_zero")
#' @param x_vec numeric vector, the evidence space
#' @param t_vec numeric vector, the time space
#' @param prms_solve the discretization (see [dRiftDM::prms_solve])
#' @param one_set_comp_vecs a list with all vectors for each model component
#' (i.e., mu_vals, b_vals etc.)
#'
#' @returns a list of PDFs for one condition "pdf_u" and "pdf_l"
#'
#' @seealso [dRiftDM::kfe_ale()], [dRiftDM::im_zero()], [dRiftDM::add_residual]
#'
calc_pdfs <- function(solver, x_vec, t_vec, prms_solve, one_set_comp_vecs) {
  if (!is.character(solver) | length(solver) != 1) {
    stop("solver must be of type character with length 1")
  }


  if (solver == "kfe") {
    pdfs <- kfe_ale(
      x_vec = x_vec, t_vec = t_vec, prms_solve = prms_solve,
      one_set_comp_vecs = one_set_comp_vecs
    )
  } else if (solver == "im_zero") {
    pdfs <- im_zero(
      t_vec = t_vec, prms_solve = prms_solve,
      one_set_comp_vecs = one_set_comp_vecs
    )
  } else {
    stop("solver '", solver, "' not implemented yet!")
  }
  return(pdfs)
}


# im_zero -> integral method with no starting distribution
#' Derive PDFs via the Integration Method
#'
#' Currently only a dummy function, with slow R code... Will be ported to
#' C++ and will be more general
#'
#' @param t_vec the time space
#' @param prms_solve the discretization, [dRiftDM::prms_solve]
#' @param one_set_comp_vecs list of vectors containing the values for each
#' model component (i.e., "mu_vals", "b_vals", etc.)
#'
#' @returns a list of PDFs for one condition "pdf_u" and "pdf_l"
#'
im_zero <- function(t_vec, prms_solve, one_set_comp_vecs) {

  # Getting the necessary parameters
  dt <- prms_solve[["dt"]]
  nt <- prms_solve[["nt"]]
  sigma <- prms_solve[["sigma"]]

  # Initializing containers
  pdf_u <- numeric(nt + 1)
  pdf_l <- numeric(nt + 1)

  # precompute and extract
  sqrt_sigma_vals <- sqrt(2.0 * pi * sigma^2 * t_vec)
  mu_vals = one_set_comp_vecs$mu_vals
  mu_int_vals = one_set_comp_vecs$mu_int_vals
  b_vals = one_set_comp_vecs$b_vals
  dt_b_vals = one_set_comp_vecs$dt_b_vals


  for (t_i in 2:nt) {
    # @Thomas: Muss mu_int_vals[1] nicht immer logischerweise 0 sein?
    pdf_u[t_i] <- -2 * psi(b_vals[t_i], t_vec[t_i], dt_b_vals[t_i], mu_vals[t_i],
                        mu_int_vals[t_i], mu_int_vals[1], sqrt_sigma_vals[t_i])
    pdf_l[t_i] <- +2 * psi(-b_vals[t_i], t_vec[t_i], -dt_b_vals[t_i], mu_vals[t_i],
                        mu_int_vals[t_i], mu_int_vals[1], sqrt_sigma_vals[t_i])


    if (t_i > 2) {
      F11 <- ff(t_vec, b_vals, b_vals, mu_int_vals, sqrt_sigma_vals,
                2 * sigma^2, dt_b_vals[t_i], mu_vals[t_i], t_i, 1:(t_i - 2))
      F12 <- ff(t_vec, b_vals, -b_vals, mu_int_vals, sqrt_sigma_vals,
                2 * sigma^2, dt_b_vals[t_i], mu_vals[t_i], t_i, 1:(t_i - 2))
      F21 <- ff(t_vec, -b_vals, b_vals, mu_int_vals, sqrt_sigma_vals,
                2 * sigma^2, -dt_b_vals[t_i], mu_vals[t_i], t_i, 1:(t_i - 2))
      F22 <- ff(t_vec, -b_vals, -b_vals, mu_int_vals, sqrt_sigma_vals,
                2 * sigma^2, -dt_b_vals[t_i], mu_vals[t_i], t_i, 1:(t_i - 2))

      pdf_u[t_i] <- pdf_u[t_i] +
        2 * dt * (sum(pdf_u[2:(t_i-1)] * F11) + sum(pdf_l[2:(t_i-1)] * F12))
      pdf_l[t_i] <- pdf_l[t_i] -
        2 * dt * (sum(pdf_u[2:(t_i-1)] * F21) + sum(pdf_l[2:(t_i-1)] * F22))
    }
  }


   # scale appropriately
   scale <- sum(pdf_u) * dt + sum(pdf_l) * dt
   pdf_u <- pdf_u / scale
   pdf_l <- pdf_l / scale
   #  add residual time ...
   pdfs <- add_residual(
     pdf_nt = one_set_comp_vecs$nt_vals, pdf_u = pdf_u, pdf_l = pdf_l, dt = dt,
     nt = nt
   )
  return(pdfs)
}

# psi for Z = 0 and ta = 0
psi <- function(b_val_i, t_val_i, dt_b_val_i, mu_val_i, mu_int_val_i,
                mu_int_val_0, sqrt_sigma_i) {

  # f() wenn Z = 0 und ta = 0
  num1 = 1 / sqrt_sigma_i * exp(
    -(b_val_i - mu_int_val_i + mu_int_val_0)^2 / (sqrt_sigma_i^2/pi)
  )

  num2 = (dt_b_val_i - mu_val_i -
            (b_val_i - mu_int_val_i + mu_int_val_0) / t_val_i)

  return(num1*num2/2)
}


# ff of thomas
ff <- function(t_vec, b_vals_1, b_vals_2, mu_int_vals, sqrt_sigma_vals,
               sigma_sq, dt_b_val_i, mu_val_i, t_i, idxs) {


  temp = b_vals_1[t_i] - b_vals_2[idxs + 1] - mu_int_vals[t_i] + mu_int_vals[idxs + 1]

  t_temp = t_vec[t_i] - t_vec[idxs + 1]

  multipl = 1 / sqrt_sigma_vals[t_i - idxs]

  exp_val = exp(-temp^2 / (sigma_sq * t_temp))

  denom = (dt_b_val_i - mu_val_i - temp / t_temp)

  return(multipl * exp_val/2 * denom)

}


#' Derive PDFs via the KFE
#'
#' The function calls the C++ implementation of the KFE approach by
#' Richter et al. and subsequently passes the first passage times forward
#' to [dRiftDM::add_residual].
#'
#' @param x_vec the evidence space
#' @param t_vec the time space
#' @param prms_solve the discretization, [dRiftDM::prms_solve]
#' @param one_set_comp_vecs list of vectors containing the values for each
#' model component (i.e., "mu_vals", "b_vals", etc.)
#'
#' @returns a list of PDFs for one condition "pdf_u" and "pdf_l"
#'
kfe_ale <- function(x_vec, t_vec, prms_solve, one_set_comp_vecs) {
  # Getting the necessary parameters
  dt <- prms_solve[["dt"]]
  nt <- prms_solve[["nt"]]
  dx <- prms_solve[["dx"]]
  nx <- prms_solve[["nx"]]
  sigma <- prms_solve[["sigma"]]

  # Initializing containers
  pdf_u <- numeric(nt + 1)
  pdf_l <- numeric(nt + 1)

  # solve the pdfs
  errorcode <- cpp_kfe(
    pdf_u = pdf_u, pdf_l = pdf_l, xx = one_set_comp_vecs$x_vals,
    nt = nt, nx = nx, dt = dt, dx = dx, sigma = sigma,
    b_vals = one_set_comp_vecs$b_vals,
    mu_vals = one_set_comp_vecs$mu_vals,
    dt_b_vals = one_set_comp_vecs$dt_b_vals, x_vec = x_vec
  )
  if (errorcode != 1) stop("cpp-version of kfe failed")

  # Omit all states that did not reach a threshold
  scale <- sum(pdf_u) * dt + sum(pdf_l) * dt
  pdf_u <- pdf_u / scale
  pdf_l <- pdf_l / scale

  #  add residual time ...
  pdfs <- add_residual(
    pdf_nt = one_set_comp_vecs$nt_vals, pdf_u = pdf_u, pdf_l = pdf_l, dt = dt,
    nt = nt
  )

  # ... and pass back
  return(pdfs)
}



#' Convolute the First Passage Times with the Non-Decision Time Distribution
#'
#' Calls [stats::convolve] for the first passage times and the non-decision
#' time distribution to derive the full distribution of response times. Before
#' convolution, I add the robustness parameter.
#'
#' @param pdf_nt the non-decision time density values
#' @param pdf_u,pdf_l the first passage times as derived by [dRiftDM::kfe_ale]
#' or [dRiftDM::im_zero]
#' @param dt,nt step size and number of steps for the time space (for input
#' checks and scaling)
#'
#' @returns a list of PDFs for one condition "pdf_u" and "pdf_l"
#'
add_residual <- function(pdf_nt, pdf_u, pdf_l, dt, nt) {
  if (length(pdf_nt) != length(pdf_u)) {
    stop("pdf_u and pdf_nt don't have the same dimension!")
  }
  if (length(pdf_nt) != length(pdf_l)) {
    stop("pdf_l and pdf_nt don't have the same dimension!")
  }

  if (abs(sum(pdf_nt) * dt - (sum(pdf_l) * dt + sum(pdf_u) * dt)) >
    drift_dm_medium_approx_error()) {
    warning(
      "pdf of the non-dec-time and pdf_l/pdf_u don't integrate to the",
      " same value"
    )
  }


  # convolute
  pdf_u <- stats::convolve(pdf_nt, rev(pdf_u), type = "open") * dt
  pdf_l <- stats::convolve(pdf_nt, rev(pdf_l), type = "open") * dt

  pdf_u <- pdf_u[1:(nt + 1)]
  pdf_l <- pdf_l[1:(nt + 1)]

  stopifnot(length(pdf_u) == length(pdf_nt) & length(pdf_l) == length(pdf_nt))


  # add robustness prm
  pdf_u <- pdf_u + drift_dm_robust_prm()
  pdf_l <- pdf_l + drift_dm_robust_prm()


  if (min(pdf_u) < 0 | min(pdf_l) < 0) {
    warning(
      "subst. negative density values encountered when calculating",
      " the pdf"
    )
  }

  return(list(pdf_u = pdf_u, pdf_l = pdf_l))
}




# Functions for calculating the log_likelihood ----------------------------


#' Calculate the Log-Likelihood
#'
#' Wrapper function around `log_like_heart`
#'
#' @param pdfs a list of pdfs (see details)
#' @param t_vec time space
#' @param obs_data a list of obs_data
#' @param conds all conditions of a model
#' @param pdf_u,pdf_l numeric vectors of the pdfs (unpacked)
#' @param rts_u,rts_l numeric vectors of the observed RTs (unpacked)
#'
#' @details
#'
#' ## calc_log_like
#'
#' Iterates over all conditions, and passes forward the (unpacked) arguments
#' to `log_like_heart`, adding each log-likelihood of a condition.
#'
#' `pdfs` must be a list with entries named as the conditions, and then
#' each condition being a list of the two PDFs (named pdf_u and pdf_l)
#'
#' `obs_data` must be a list with entries "rts_u" and "rts_l", and then
#' each rts_* entry being a named list with the RT values for each condition
#'
#' ## log_like_heart
#'
#' Gets the density values for RTs in rts_u/rts_l via [stats::approx()],
#' takes the log of that, and then sums across both.
#' Wraps up the calculation in a tryCatch statement, throwing warnings when
#' log_like_values can not be calculated
#'
#'
#' @returns a single value of the log-likelihood
#'
calc_log_like <- function(pdfs, t_vec, obs_data, conds) {
  log_like <- 0

  for (one_cond in conds) {
    log_like <- log_like + log_like_heart(
      pdf_u = pdfs[[one_cond]]$pdf_u,
      pdf_l = pdfs[[one_cond]]$pdf_l,
      t_vec = t_vec,
      rts_u = obs_data$rts_u[[one_cond]],
      rts_l = obs_data$rts_l[[one_cond]]
    )
  }

  return(log_like)
}

#' @rdname calc_log_like
log_like_heart <- function(pdf_u, pdf_l, t_vec, rts_u, rts_l) {
  tryCatch(
    expr = {
      app_like_u <- stats::approx(
        x = t_vec, y = pdf_u,
        xout = rts_u
      )$y
      app_like_l <- stats::approx(
        x = t_vec, y = pdf_l,
        xout = rts_l
      )$y

      log_like <- sum(log(app_like_u)) + sum(log(app_like_l))
      if (is.nan(log_like)) { # log(0) gives -Inf
        if (min(app_like_u) < 0 | min(app_like_l) < 0) {
          warning(
            "negative density values encountered",
            " when calculating the log-likelihood"
          )
        }
        return(-Inf)
      }
      return(log_like)
    },
    error = function(e) {
      warning("we encountered an untreated error!", e)
      return(-Inf)
    }
  )
}
