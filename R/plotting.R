# === FUNCTION FOR PLOTTING A DM

plot_model <- function(drift_dm_obj, add_x = TRUE, sigma = 0, k = 1,
                       x_lab = NULL, y_lab = NULL, x_lim = NULL, y_lim = NULL,
                       line_cols_ev = NULL, line_cols_b = NULL, seed = NULL) {
  # prepare some variables
  drift_dm_obj$prms_solve["sigma"] <- sigma
  t_max <- drift_dm_obj$prms_solve["t_max"]
  nT <- drift_dm_obj$prms_solve["nT"]
  t_vec <- seq(0, t_max, length.out = nT + 1)
  unique_conds <- unique(drift_dm_obj$conds)


  # set default arguments
  if (is.null(x_lab)) {
    x_lab <- "Time"
  }

  if (is.null(y_lab)) {
    y_lab <- "Evidence"
  }

  if (is.null(y_lim)) {
    bs <- sapply(unique_conds, function(x) {
      max(b(drift_dm_obj = drift_dm_obj, t_vec = t_vec, one_cond = x))
    })
    max_b <- max(bs)
    y_lim <- c(-max_b, max_b)
  }

  if (is.null(x_lim)) {
    x_lim <- c(0, t_max / 3)
  }

  if (!is.null(line_cols_ev)) {
    if (length(line_cols_ev) == 1) {
      rep(line_cols_ev, length(unique_conds))
    } else {
      if (length(line_cols_ev) != length(unique_conds)) {
        stop("number of line_cols_ev must match the number of conditions")
      }
    }
  } else {
    line_cols_ev <- grDevices::rainbow(n = length(unique_conds))
  }

  if (!is.null(line_cols_b)) {
    if (length(line_cols_b) == 1) {
      rep(line_cols_b, length(unique_conds))
    } else {
      if (length(line_cols_b) != length(unique_conds)) {
        stop("number of line_cols_b must match the number of conditions")
      }
    }
  } else {
    line_cols_b <- rep("black", length(unique_conds))
  }

  if (!is.null(seed)) {
    withr::local_seed(seed)
  }



  # prepare plot
  plot(c(1, 2) ~ c(1, 1),
    col = "white", xlab = x_lab, ylab = y_lab, xlim = x_lim,
    ylim = y_lim
  )
  graphics::abline(h = 0, col = "gray", cex = 0.5)

  for (idx in seq_along(unique_conds)) {
    one_cond <- unique_conds[idx]
    exp_processes <- simulate_trace(drift_dm_obj,
      k = k, one_cond = one_cond,
      add_x = add_x
    )
    for (i in 1:k) {
      exp_process <- exp_processes[i, ]
      stopifnot(length(exp_process) == length(t_vec))
      graphics::points(exp_process ~ t_vec, ty = "l", col = line_cols_ev[idx])
    }

    b_vec <- b(drift_dm_obj = drift_dm_obj, t_vec = t_vec, one_cond = one_cond)
    stopifnot(length(b_vec) == length(t_vec))
    graphics::points(b_vec ~ t_vec, ty = "l", col = line_cols_b[idx])
    graphics::points(-b_vec ~ t_vec, ty = "l", col = line_cols_b[idx])
  }

  graphics::legend("topright",
    legend = unique_conds,
    col = line_cols_ev, lty = 1
  )
}


# === FUNCTION FOR PLOTTING the Model Predictions

plot_cafs <- function(drift_dm_obj, type = "both", n_bins = 5, x_lab = NULL,
                      y_lab = NULL, x_lim = NULL, y_lim = NULL,
                      line_cols = NULL) {
  type <- match.arg(type, c("both", "pred"))

  unique_conds <- drift_dm_obj$conds

  # set default arguments
  if (is.null(x_lab)) {
    x_lab <- "Bins"
  }

  if (is.null(y_lab)) {
    y_lab <- "Accuracy [%]"
  }

  if (is.null(y_lim)) {
    y_lim <- c(0, 1)
  }

  if (is.null(x_lim)) {
    x_lim <- c(1, n_bins)
  }

  if (!is.null(line_cols)) {
    if (length(line_cols) == 1) {
      rep(line_cols, length(unique_conds))
    } else {
      if (length(line_cols) != length(unique_conds)) {
        stop("number of line_cols must match the number of conditions")
      }
    }
  } else {
    line_cols <- grDevices::rainbow(n = length(unique_conds))
  }

  cafs <- calc_cafs(drift_dm_obj = drift_dm_obj, type = type, n_bins = n_bins)

  # prepare plot
  plot(c(1, 2) ~ c(1, 1),
    col = "white", xlab = x_lab, ylab = y_lab, xlim = x_lim,
    ylim = y_lim
  )

  for (idx in seq_along(unique_conds)) {
    sub_dat <- cafs[cafs$Cond == unique_conds[idx], ]
    if (type == "both") {
      sub_dat_obs <- sub_dat[sub_dat$Source == "obs", ]
      if (nrow(sub_dat_obs) == 0) {
        warning("no data found; not plotting them")
      } else {
        graphics::points(sub_dat_obs$P_Corr ~ sub_dat_obs$Bin, col = line_cols[idx])
      }
    }
    sub_dat_pred <- sub_dat[sub_dat$Source == "pred", ]
    graphics::points(sub_dat_pred$P_Corr ~ sub_dat_pred$Bin,
      ty = "l",
      col = line_cols[idx]
    )
  }

  graphics::legend("bottomright",
    legend = unique_conds,
    col = line_cols, lty = 1
  )
}




plot_quantiles <- function(drift_dm_obj, type = "both",
                           probs = seq(0.1, 0.9, 0.1), to_plot = "Quant_Corr",
                           x_lab = NULL, y_lab = NULL, x_lim = NULL,
                           y_lim = NULL, line_cols = NULL) {
  unique_conds <- drift_dm_obj$conds
  t_max <- drift_dm_obj$prms_solve[["t_max"]]


  # set default arguments
  if (is.null(x_lab)) {
    x_lab <- "RT"
  }

  if (is.null(y_lab)) {
    y_lab <- "F(RT)"
  }

  if (is.null(y_lim)) {
    y_lim <- c(0, 1)
  }

  if (is.null(x_lim)) {
    x_lim <- c(0, t_max / 2)
  }

  if (!is.null(line_cols)) {
    if (length(line_cols) == 1) {
      rep(line_cols, length(unique_conds))
    } else {
      if (length(line_cols) != length(unique_conds)) {
        stop("number of line_cols must match the number of conditions")
      }
    }
  } else {
    line_cols <- grDevices::rainbow(n = length(unique_conds))
  }

  quantiles <- calc_quantiles(
    drift_dm_obj = drift_dm_obj, type = type,
    probs = probs
  )

  # prepare plot
  plot(c(1, 2) ~ c(1, 1),
    col = "white", xlab = x_lab, ylab = y_lab, xlim = x_lim,
    ylim = y_lim
  )


  for (idx in seq_along(unique_conds)) {
    sub_dat <- quantiles[quantiles$Cond == unique_conds[idx], ]
    if (type == "both") {
      sub_dat_obs <- sub_dat[sub_dat$Source == "obs", ]
      sub_dat_obs <- sub_dat_obs[c("Prob", to_plot)]
      sub_dat_obs <- stats::na.omit(sub_dat_obs)
      if (nrow(sub_dat_obs) == 0) {
        warning("no data found; not plotting them")
      } else {
        graphics::points(sub_dat_obs$Prob ~ sub_dat_obs[[to_plot]], col = line_cols[idx])
      }
    }
    sub_dat_pred <- sub_dat[sub_dat$Source == "pred", ]
    sub_dat_pred <- sub_dat_pred[c("Prob", to_plot)]
    sub_dat_pred <- stats::na.omit(sub_dat_pred)
    graphics::points(sub_dat_pred$Prob ~ sub_dat_pred[[to_plot]],
      ty = "l",
      col = line_cols[idx]
    )
  }


  graphics::legend("bottomright",
    legend = unique_conds,
    col = line_cols, lty = 1
  )
}
