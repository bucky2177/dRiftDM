# === FUNCTIONS FOR GETTING THE PDF OF A MODEL

calc_pdfs <- function(solver, x_vec, t_vec, prms_solve, one_set_comp_vecs,
                      one_cond) {
  if (!is.character(solver) | length(solver) != 1) {
    stop("solver must be of type character with length 1")
  }

  if (!is.character(one_cond) | length(one_cond) != 1) {
    stop("solver must be of type character with length 1")
  }

  if (solver == "kfe") {
    pdfs <- kfe_ale(
      x_vec = x_vec, t_vec = t_vec, prms_solve = prms_solve,
      one_set_comp_vecs = one_set_comp_vecs, one_cond = one_cond
    )
  } else {
    stop("solver '", solver, "' not implemented yet!")
  }
  return(pdfs)
}


# kfe and residual time
kfe_ale <- function(x_vec, t_vec, prms_solve, one_set_comp_vecs, one_cond) {
  # Getting the necessary parameters
  dt <- prms_solve[["dt"]]
  nt <- prms_solve[["nt"]]
  dx <- prms_solve[["dx"]]
  nx <- prms_solve[["nx"]]
  sigma <- prms_solve[["sigma"]]

  # Initializing containers
  pdf_u <- numeric(nt + 1)
  pdf_l <- numeric(nt + 1)

  # decide about Rannacher time-stepping
  x_vals <- one_set_comp_vecs$x_vals
  r_stepping <- ifelse(sum(x_vals > 0) == 1, T, F)

  # solve the pdfs
  errorcode <- cpp_kfe(
    pdf_u = pdf_u, pdf_l = pdf_l, xx = x_vals,
    nt = nt, nx = nx, dt = dt, dx = dx, sigma = sigma,
    r_stepping = r_stepping,
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
    pdf_nt = one_set_comp_vecs$nt_vals, pdf_u = pdf_u, pdf_l = pdf_l, dt = dt
  )

  # ... and pass back
  return(pdfs)
}



add_residual <- function(pdf_nt, pdf_u, pdf_l, dt) {
  if (length(pdf_nt) != length(pdf_u)) {
    stop("pdf_u and pdf_nt don't have the same dimension!")
  }
  if (length(pdf_nt) != length(pdf_l)) {
    stop("pdf_l and pdf_nt don't have the same dimension!")
  }

  if (abs(sum(pdf_nt * dt) - (sum(pdf_l * dt) + sum(pdf_u * dt))) >
    drift_dm_medium_approx_error()) {
    warning(
      "pdf of the non-dec-time and pdf_l/pdf_u don't integrate to the",
      " same value"
    )
  }

  pdf_u <- stats::convolve(pdf_nt, rev(pdf_u)) * dt
  pdf_l <- stats::convolve(pdf_nt, rev(pdf_l)) * dt

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


# ==== Functions for calculating the log_likelihood

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
