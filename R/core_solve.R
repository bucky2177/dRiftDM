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
  nT <- drift_dm_obj$prms_solve[["nT"]]
  dx <- drift_dm_obj$prms_solve[["dx"]]
  nX <- drift_dm_obj$prms_solve[["nX"]]
  sigma <- drift_dm_obj$prms_solve[["sigma"]]


  # Initializing containers
  f <- numeric(nX + 1)
  pdf_u <- numeric(nT + 1)
  pdf_l <- numeric(nT + 1)
  xx <- seq(-1, 1, length.out = nX + 1)


  # Calculating starting vector x
  x_vec <- x(drift_dm_obj = drift_dm_obj, one_cond = one_cond)
  if (abs(sum(x_vec) * dx - 1) > drift_dm_rough_approx_error()) {
    stop("starting condition not normalized")
  }
  if (length(x_vec) != nX + 1) stop("unexpected length of x")


  # get values across t
  t_vec <- seq(0, t_max, length.out = nT + 1)
  mu_vec <- mu(drift_dm_obj = drift_dm_obj, t_vec = t_vec, one_cond = one_cond)
  b_vec <- b(drift_dm_obj = drift_dm_obj, t_vec = t_vec, one_cond = one_cond)
  dt_b_vec <- dt_b(
    drift_dm_obj = drift_dm_obj, t_vec = t_vec,
    one_cond = one_cond
  )
  if (any(is.infinite(mu_vec)) | any(is.na(mu_vec))) {
    stop("mu_vec provided infinite values or NAs")
  }
  if (any(is.infinite(b_vec)) | any(is.na(b_vec))) {
    stop("b_vec provided infinite values or NAs")
  }
  if (any(is.infinite(dt_b_vec)) | any(is.na(dt_b_vec))) {
    stop("dt_b_vec provided infinite values or NAs")
  }
  if (any(is.infinite(x_vec)) | any(is.na(x_vec))) {
    stop("x_vec provided infinite values or NAs")
  }
  stopifnot(length(mu_vec) == length(b_vec))
  stopifnot(length(mu_vec) == length(dt_b_vec))
  stopifnot(length(mu_vec) == length(t_vec))


  # fill pdfs for each timestep
  for (n in 1:nT) {
    # Rannacher time-marching
    theta <- 0.5
    if (n <= 4) {
      theta <- 1
    }

    J_old <- b_vec[n]
    mu_old <- mu_vec[n]
    dt_b_old <- dt_b_vec[n]
    mu_old <- rep(mu_old / J_old, nX + 1) - dt_b_old / J_old * xx
    sigma_old <- sigma / J_old
    L_old <- 1.0 / dx * sigma_old * sigma_old / 2.0

    J_new <- b_vec[n + 1]
    mu_new <- mu_vec[n + 1]
    dt_b_new <- dt_b_vec[n + 1]
    mu_new <- rep(mu_new / J_new, nX + 1) - dt_b_new / J_new * xx
    sigma_new <- sigma / J_new
    L_new <- 1.0 / dx * sigma_new * sigma_new / 2.0

    f <- 2.0 / 3.0 * dx * x_vec
    f[2:nX] <- f[2:nX] + 1.0 / 6.0 * dx * x_vec[1:(nX - 1)]
    f[2:nX] <- f[2:nX] + 1.0 / 6.0 * dx * x_vec[3:(nX + 1)]

    f[2:nX] <- f[2:nX] + (theta - 1) * dt *
      (-L_old - 0.5 * mu_old[1:(nX - 1)]) * x_vec[1:(nX - 1)]
    f[2:nX] <- f[2:nX] + (theta - 1) * dt * (2.0 * L_old) * x_vec[2:nX]
    f[2:nX] <- f[2:nX] + (theta - 1) * dt *
      (-L_old + 0.5 * mu_old[3:(nX + 1)]) * x_vec[3:(nX + 1)]

    if (f[1] != 0 | f[nX + 1] != 0) stop("f nicht null?")

    x_vec <- tridiag(
      f,
      1.0 / 6.0 * dx + dt * theta * (-L_new - 0.5 * mu_new),
      2.0 / 3.0 * dx + dt * theta * (2.0 * L_new),
      1.0 / 6.0 * dx + dt * theta * (-L_new + 0.5 * mu_new)
    )

    pdf_u[n + 1] <- 0.5 * sigma_new^2.0 / dx / dx *
      (3.0 * x_vec[nX] - 1.5 * x_vec[nX - 1] + 1.0 / 3.0 * x_vec[nX - 2])
    pdf_l[n + 1] <- 0.5 * sigma_new^2.0 / dx / dx *
      (3.0 * x_vec[2] - 1.5 * x_vec[3] + 1.0 / 3.0 * x_vec[4])
  }

  # Omit all states that did not reach a threshold
  scale <- sum(pdf_u) * dt + sum(pdf_l) * dt
  pdf_u <- pdf_u / scale
  pdf_l <- pdf_l / scale

  #  add residual time ...
  dt <- drift_dm_obj$prms_solve[["dt"]]
  pdf_nt <- nt(drift_dm_obj = drift_dm_obj, t_vec = t_vec, one_cond = one_cond)

  pdfs <- add_residual(
    pdf_nt = pdf_nt, pdf_u = pdf_u, pdf_l = pdf_l, dt = dt,
    one_cond = one_cond
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

  if (abs(sum(pdf_nt*dt) - (sum(pdf_l*dt) + sum(pdf_u*dt))) >
    drift_dm_small_approx_error()) {
    warning(
      "pdf of the non-dec-time and pdf_l/pdf_u don't integrate to the",
      " same value"
    )
  }

  pdf_u <- stats::convolve(pdf_nt, rev(pdf_u)) * dt
  pdf_l <- stats::convolve(pdf_nt, rev(pdf_l)) * dt

  stopifnot(length(pdf_u) == length(pdf_nt) & length(pdf_l) == length(pdf_nt))

  if (min(pdf_u) < -1e-3 || min(pdf_l) < -1e-3) {
    warning(
      "unlikely parameter combination encountered; the model produced",
      " subst. negative pdf values"
    )
  }

  list(pdf_u = pdf_u, pdf_l = pdf_l)
}


# ==== Functions for calculating the log_likelihood

log_like <- function(drift_dm_obj) {
  log_like <- 0
  red_drift_dm_obj <- drift_dm_obj
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
      nT <- drift_dm_obj$prms_solve[["nT"]]
      tt <- seq(0, t_max, length.out = nT + 1)
      app_like_u <- stats::approx(
        x = tt, y = pdf_u,
        xout = drift_dm_obj$obs_data$rts_corr[[one_cond]]
      )$y
      app_like_l <- stats::approx(
        x = tt, y = pdf_l,
        xout = drift_dm_obj$obs_data$rts_err[[one_cond]]
      )$y

      app_like_u = app_like_u + drift_dm_robust_prm()
      app_like_l = app_like_l + drift_dm_robust_prm()
      log_like <- sum(log(app_like_u)) + sum(log(app_like_l))
      if (is.nan(log_like)) {
        if (min(app_like_u) < 0 | min(app_like_l) < 0)
          warning("negative density values encountered") # log(0) gives -Inf
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