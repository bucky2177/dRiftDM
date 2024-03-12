# === FUNCTIONS FOR GETTING THE PDF OF A MODEL

#' Derive the PDFs of a model
#'
#' @description
#' This function calculates the probability density functions of a drift
#' diffusion model for one condition.
#'
#' @param drift_dm_obj an object inheriting from [dRiftDM::drift_dm]
#'
#' @param one_cond character, providing the name of a condition
#'
#' @details
#' More in-depth information about the mathematical details can be found in
#' \insertCite{Richteretal.2023;textual}{dRiftDM}
#'
#' @references
#' \insertAllCited{}
#'
#' @export
calc_pdfs <- function(drift_dm_obj, one_cond) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }

  solver <- drift_dm_obj$solver

  if (!is.character(one_cond) | length(one_cond) != 1) {
    stop("one_cond must be of type character with length 1")
  }


  if (!is.character(solver) | length(solver) != 1) {
    stop("solver must be of type character with length 1")
  }


  if (solver == "kfe") {
    pdfs <- kfe_ale(drift_dm_obj, one_cond)
  } else {
    stop("solver '", solver, "' not implemented yet!")
  }
  return(pdfs)
}


# kfe and residual time
kfe_ale <- function(drift_dm_obj, one_cond) {
  # Getting the necessary parameters
  t_max <- drift_dm_obj$prms_solve[["t_max"]]
  dt <- drift_dm_obj$prms_solve[["dt"]]
  nt <- drift_dm_obj$prms_solve[["nt"]]
  dx <- drift_dm_obj$prms_solve[["dx"]]
  nx <- drift_dm_obj$prms_solve[["nx"]]
  sigma <- drift_dm_obj$prms_solve[["sigma"]]

  # Initializing containers
  pdf_u <- numeric(nt + 1)
  pdf_l <- numeric(nt + 1)

  # get starting condition
  x_vec <- seq(-1, 1, length.out = nx + 1)
  x_vals <- drift_dm_obj$comp_funs$x_fun(
    drift_dm_obj = drift_dm_obj,
    x_vec = x_vec, one_cond = one_cond
  )
  if (any(is.infinite(x_vals)) | any(is.na(x_vals))) {
    stop("x_fun provided infinite values or NAs")
  }
  if (min(x_vals) < 0) {
    stop("x_fun provided negative values")
  }
  if (abs(sum(x_vals) * dx - 1) > drift_dm_small_approx_error()) {
    stop("starting condition doesn't integrate to 1")
  }
  if (length(x_vals) != nx + 1) stop("unexpected length of x_vals")
  r_stepping <- ifelse(length(which(x_vals > 0)) == 1, T, F)

  # get drift rate/boundary values across t
  t_vec <- seq(0, t_max, length.out = nt + 1)
  mu_vals <- drift_dm_obj$comp_funs$mu_fun(
    drift_dm_obj = drift_dm_obj,
    t_vec = t_vec, one_cond = one_cond
  )
  b_vals <- drift_dm_obj$comp_funs$b_fun(
    drift_dm_obj = drift_dm_obj,
    t_vec = t_vec, one_cond = one_cond
  )
  dt_b_vals <- drift_dm_obj$comp_funs$dt_b_fun(
    drift_dm_obj = drift_dm_obj, t_vec = t_vec, one_cond = one_cond
  )
  if (any(is.infinite(mu_vals)) | any(is.na(mu_vals))) {
    stop("mu_vals provided infinite values or NAs")
  }
  if (any(is.infinite(b_vals)) | any(is.na(b_vals))) {
    stop("b_vals provided infinite values or NAs")
  }
  if (any(is.infinite(dt_b_vals)) | any(is.na(dt_b_vals))) {
    stop("dt_b_vals provided infinite values or NAs")
  }
  if (length(mu_vals) != length(t_vec)) stop("unexpected length of mu_vals")
  if (length(b_vals) != length(t_vec)) stop("unexpected length of b_vals")
  if (length(dt_b_vals) != length(t_vec)) stop("unexpected length of dt_b_vals")

  # solve the pdfs
  errorcode <- cpp_kfe(
    pdf_u = pdf_u, pdf_l = pdf_l, xx = x_vals, nt = nt,
    nx = nx, dt = dt, dx = dx, sigma = sigma,
    r_stepping = r_stepping, b_vals = b_vals,
    mu_vals = mu_vals,
    dt_b_vals = dt_b_vals, x_vec = x_vec
  )
  if (errorcode != 1) stop("cpp-version of kfe failed")

  # Omit all states that did not reach a threshold
  scale <- sum(pdf_u) * dt + sum(pdf_l) * dt
  pdf_u <- pdf_u / scale
  pdf_l <- pdf_l / scale

  #  add residual time ...
  pdf_nt <- drift_dm_obj$comp_funs$nt_fun(
    drift_dm_obj = drift_dm_obj,
    t_vec = t_vec, one_cond = one_cond
  )
  pdfs <- add_residual(
    pdf_nt = pdf_nt, pdf_u = pdf_u, pdf_l = pdf_l, dt = dt
  )

  # ... and pass back
  return(pdfs)
}



add_residual <- function(pdf_nt, pdf_u, pdf_l, dt) {
  if (any(is.infinite(pdf_nt)) | any(is.na(pdf_nt))) {
    stop("pdf_nt provided infinite values or NAs")
  }
  if (min(pdf_nt) < 0) {
    stop("pdf_nt provided negative values")
  }
  if (length(pdf_nt) != length(pdf_u)) {
    stop("pdf_u and pdf_nt don't have the same dimension!")
  }
  if (length(pdf_nt) != length(pdf_l)) {
    stop("pdf_l and pdf_nt don't have the same dimension!")
  }

  if (abs(sum(pdf_nt * dt) - (sum(pdf_l * dt) + sum(pdf_u * dt))) >
    drift_dm_small_approx_error()) {
    warning(
      "pdf of the non-dec-time and pdf_l/pdf_u don't integrate to the",
      " same value"
    )
  }

  pdf_u <- stats::convolve(pdf_nt, rev(pdf_u)) * dt
  pdf_l <- stats::convolve(pdf_nt, rev(pdf_l)) * dt

  stopifnot(length(pdf_u) == length(pdf_nt) & length(pdf_l) == length(pdf_nt))

  if (min(pdf_u) < -drift_dm_robust_prm() |
    min(pdf_l) < -drift_dm_robust_prm()) {
    warning(
      "subst. negative density values encountered when calculating",
      " the pdf"
    )
  }


  return(list(pdf_u = pdf_u, pdf_l = pdf_l))
}


# ==== Functions for calculating the log_likelihood

calc_log_like <- function(drift_dm_obj) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }

  log_like <- 0
  if (is.null(drift_dm_obj$obs_data)) {
    warning("calc_log_like() was called but no data is provided returning -Inf")
    return(-Inf)
  }

  if (is.null(drift_dm_obj$pdfs)) {
    warning("calc_log_like() was called but no pdfs were found. Returning -Inf")
    return(-Inf)
  }

  for (one_cond in drift_dm_obj$conds) {
    log_like <- log_like + log_like_heart(
      drift_dm_obj = drift_dm_obj,
      pdf_u = drift_dm_obj$pdfs[[one_cond]]$pdf_u,
      pdf_l = drift_dm_obj$pdfs[[one_cond]]$pdf_l,
      one_cond = one_cond
    )
  }

  return(log_like)
}

log_like_heart <- function(drift_dm_obj, pdf_u, pdf_l, one_cond) {
  tryCatch(
    expr = {
      t_max <- drift_dm_obj$prms_solve[["t_max"]]
      nt <- drift_dm_obj$prms_solve[["nt"]]
      tt <- seq(0, t_max, length.out = nt + 1)
      app_like_u <- stats::approx(
        x = tt, y = pdf_u,
        xout = drift_dm_obj$obs_data$rts_corr[[one_cond]]
      )$y
      app_like_l <- stats::approx(
        x = tt, y = pdf_l,
        xout = drift_dm_obj$obs_data$rts_err[[one_cond]]
      )$y

      app_like_u <- app_like_u + drift_dm_robust_prm()
      app_like_l <- app_like_l + drift_dm_robust_prm()
      log_like <- sum(log(app_like_u)) + sum(log(app_like_l))
      if (is.nan(log_like)) { # log(0) gives -Inf
        if (min(app_like_u) < 0 | min(app_like_l) < 0) {
          warning(
            "negative density values encountered after adding robustness",
            " parameter when calculating the log-likelihood"
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
