# === FUNCTIONS FOR GETTING THE PDF OF A MODEL

get_pdfs <- function(drift_dm_obj, one_cond, solver) {
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
  x_vec <- seq(-1, 1, length.out = nx + 1)


  # Calculating starting vector x
  x_vals <- x(drift_dm_obj = drift_dm_obj, x_vec = x_vec, one_cond = one_cond)
  if (abs(sum(x_vals) * dx - 1) > drift_dm_small_approx_error()) {
    stop("starting condition not normalized")
  }
  if (length(x_vals) != nx + 1) stop("unexpected length of x_vals")

  # get values across t
  t_vec <- seq(0, t_max, length.out = nt + 1)
  mu_vals <- mu(drift_dm_obj = drift_dm_obj, t_vec = t_vec, one_cond = one_cond)
  b_vals <- b(drift_dm_obj = drift_dm_obj, t_vec = t_vec, one_cond = one_cond)
  dt_b_vals <- dt_b(
    drift_dm_obj = drift_dm_obj, t_vec = t_vec,
    one_cond = one_cond
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
  if (any(is.infinite(x_vals)) | any(is.na(x_vals))) {
    stop("x_vals provided infinite values or NAs")
  }
  if (length(mu_vals) != length(t_vec)) stop("unexpected length of mu_vals")
  if (length(b_vals) != length(t_vec)) stop("unexpected length of b_vals")
  if (length(dt_b_vals) != length(t_vec)) stop("unexpected length of dt_b_vals")

  # solve the pdfs
  errorcode <- cpp_kfe(pdf_u = pdf_u, pdf_l = pdf_l, xx = x_vals, nt = nt,
                       nx = nx, dt = dt, dx = dx, sigma = sigma,
                       b_vals = b_vals, mu_vals = mu_vals,
                       dt_b_vals = dt_b_vals);
  if (errorcode != 1) stop("cpp-version of kfe failed")

  # Omit all states that did not reach a threshold
  scale <- sum(pdf_u) * dt + sum(pdf_l) * dt
  pdf_u <- pdf_u / scale
  pdf_l <- pdf_l / scale

  #  add residual time ...
  dt <- drift_dm_obj$prms_solve[["dt"]]
  pdf_nt <- nt(drift_dm_obj = drift_dm_obj, t_vec = t_vec, one_cond = one_cond)

  pdfs <- add_residual(
    pdf_nt = pdf_nt, pdf_u = pdf_u, pdf_l = pdf_l, dt = dt, one_cond = one_cond
  )

  # ... and pass back
  return(pdfs)
}



add_residual <- function(pdf_nt, pdf_u, pdf_l, dt, one_cond) {
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

  list(pdf_u = pdf_u, pdf_l = pdf_l)
}


# ==== Functions for calculating the log_likelihood

get_log_like <- function(drift_dm_obj) {
  log_like <- 0
  red_drift_dm_obj <- drift_dm_obj
  if (is.null(red_drift_dm_obj$obs_data)) {
    warning("log_like() was called but no data is provided",
            " returning -Inf")
    return(-Inf)
  }
  red_drift_dm_obj$obs_data <- NULL
  for (one_cond in drift_dm_obj$conds) {
    pdfs <- get_pdfs(
      drift_dm_obj = red_drift_dm_obj,
      one_cond = one_cond,
      solver = drift_dm_obj$solver
    )

    if (min(pdfs$pdf_u) < -1e-3 || min(pdfs$pdf_l) < -1e-3) {
      warning(
        "unlikely parameter combination encountered; the model produced",
        " subst. negative pdf values"
      )
      return(-Inf)
    }

    log_like <- log_like + log_like_heart(
      drift_dm_obj = drift_dm_obj,
      pdf_u = pdfs$pdf_u,
      pdf_l = pdfs$pdf_l,
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
          warning("negative density values encountered")
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
