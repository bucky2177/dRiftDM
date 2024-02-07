# === FUNCTION FOR PLOTTING A DM
#' Plot trace(s) of a model
#'
#' @description
#' This functions creates a basic plot that depicts a user-defined number
#' of traces (see [dRiftDM::simulate_trace]).
#' May come in handy when developing/testing/exploring a model.
#'
#' @param drift_dm_obj an object inheriting from [dRiftDM::drift_dm]
#' @param add_x logical, indicating whether a starting point should be
#'  added. Default is TRUE.
#' @param sigma numerical, for controlling the diffusion parameter when
#'  simulating traces. Set 0 to plot the expected time-course of the diffusion
#'  model. The default (NULL) leaves the diffusion constant of `drift_dm_obj`
#'  unchanged.
#' @param k numerical, number of traces to plot. Default is 1.
#' @param x_lab,y_lab character, providing a label for the x-axis and y-axis,
#'  respectively. Default is "Time" and "Evidence", respectively.
#' @param x_lim,y_lim numeric vectors of length 2, providing the limits of the
#'  x-axis and y-axis. The default for `x_lim` is `c(0, t_max / 4)`, with
#'  `t_max` defined within `drift_dm_obj`. The default for `y_lim` is the
#'  maximum of the boundary function provided in `drift_dm_obj`.
#' @param line_cols_ev character vector, indicating the color-coding of the
#'  conditions. Default colors are based on the `grDevices::rainbow` palette.
#' @param line_cols_b character, providing a color for the boundary. Default
#'  is black.
#' @param seed a seed for making the simulated traces reproducable. Default
#'  (NULL) uses no seed.
#'
#' @export
plot_trace <- function(drift_dm_obj, add_x = TRUE, sigma = NULL, k = 1,
                       x_lab = NULL, y_lab = NULL, x_lim = NULL, y_lim = NULL,
                       line_cols_ev = NULL, line_cols_b = NULL, seed = NULL) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }

  # prepare some variables
  if (!is.null(sigma)) {
    drift_dm_obj$prms_solve["sigma"] <- sigma
  }
  t_max <- drift_dm_obj$prms_solve["t_max"]
  nt <- drift_dm_obj$prms_solve["nt"]
  t_vec <- seq(0, t_max, length.out = nt + 1)
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
      max(drift_dm_obj$comp_funs$b_fun(
        drift_dm_obj = drift_dm_obj,
        t_vec = t_vec, one_cond = x
      ))
    })
    max_b <- max(bs)
    y_lim <- c(-max_b, max_b)
  }

  if (is.null(x_lim)) {
    x_lim <- c(0, t_max / 4)
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
    if (length(line_cols_b) != 1) {
      stop("line_cols_b must a character vector of length 1")
    }
  } else {
    line_cols_b <- "black"
  }

  if (!is.null(seed)) {
    if (!is.numeric(seed) | length(seed) != 1) {
      stop("seed must be a single numeric")
    }
    withr::local_preserve_seed()
    set.seed(seed)
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
      if (k == 1) {
        exp_process <- exp_processes
      } else {
        exp_process <- exp_processes[i, ]
      }
      stopifnot(length(exp_process) == length(t_vec))
      graphics::points(exp_process ~ t_vec, ty = "l", col = line_cols_ev[idx])
    }

    b_vec <- drift_dm_obj$comp_funs$b_fun(
      drift_dm_obj = drift_dm_obj,
      t_vec = t_vec, one_cond = one_cond
    )
    stopifnot(length(b_vec) == length(t_vec))
    graphics::points(b_vec ~ t_vec, ty = "l", col = line_cols_b)
    graphics::points(-b_vec ~ t_vec, ty = "l", col = line_cols_b)
  }

  graphics::legend("topright",
    legend = unique_conds,
    col = line_cols_ev, lty = 1
  )
}


# === FUNCTION FOR PLOTTING the Model Predictions

#' Plot conditional accuracy functions
#'
#' This function provides a basic plot of the model's and/or the observed data's
#' conditional accuracy functions (CAFs; see [dRiftDM::calc_stats]).
#'
#' @param obj an object inheriting from [dRiftDM::drift_dm] or
#' `dm_fits_subjects` (see [dRiftDM::load_fits_subjects]). If `obj` is of the
#' latter type, cafs are calculated via
#' [dRiftDM::gather_stats] and then aggregated across subjects.
#'
#' @param source character, indicating whether CAFs of the model ("pred"), the
#'  observed data ("obs"), or both ("both") should be plotted. Default is
#'  "both".
#' @param n_bins numeric, indicating how many bins the CAF should contain.
#'  Default is 5.
#' @param x_lab,y_lab character, providing a label for the x-axis and y-axis,
#'  respectively. Default is "Bins" and "Accuracy %", respectively.
#' @param x_lim,y_lim numeric vectors of length 2, providing the limits of the
#'  x-axis and y-axis. The default for `x_lim` is `c(1, n_bins)`. The default
#'  for `y_lim` is `c(0, 1)`.
#' @param line_cols character vector, indicating the color-coding of the
#'  conditions. Default colors are based on the `grDevices::rainbow` palette.
#'
#' @export
plot_cafs <- function(obj, source = "both", n_bins = 5, x_lab = NULL,
                      y_lab = NULL, x_lim = NULL, y_lim = NULL,
                      line_cols = NULL) {
  if (!inherits(obj, "drift_dm") &
    !inherits(obj, "dm_fits_subjects")) {
    stop("obj is not of type drift_dm or dm_fits_subjects")
  }

  # get values
  if (inherits(obj, "drift_dm")) {
    cafs <- calc_stats(
      drift_dm_obj = obj, type = "cafs", source = source,
      n_bins = n_bins
    )
  } else {
    cafs <- gather_stats(
      fits_subjects = obj, type = "cafs", source = source,
      n_bins = n_bins, verbose = 1
    )
  }

  agg_factors <- c("Source", "Cond", "Bin")
  stopifnot(agg_factors %in% colnames(cafs))
  cafs <- stats::aggregate(cafs["P_Corr"], by = cafs[agg_factors], FUN = mean)

  unique_conds <- unique(cafs$Cond)

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

  # prepare plot
  plot(c(1, 2) ~ c(1, 1),
    col = "white", xlab = x_lab, ylab = y_lab, xlim = x_lim,
    ylim = y_lim
  )

  for (idx in seq_along(unique_conds)) {
    sub_dat <- cafs[cafs$Cond == unique_conds[idx], ]
    sub_dat_obs <- sub_dat[sub_dat$Source == "obs", ]
    if (nrow(sub_dat_obs) > 0) {
      graphics::points(sub_dat_obs$P_Corr ~ sub_dat_obs$Bin, col = line_cols[idx])
    }

    sub_dat_pred <- sub_dat[sub_dat$Source == "pred", ]
    if (nrow(sub_dat_pred) > 0) {
      graphics::points(sub_dat_pred$P_Corr ~ sub_dat_pred$Bin,
        ty = "l",
        col = line_cols[idx]
      )
    }
  }

  graphics::legend("bottomright",
    legend = unique_conds,
    col = line_cols, lty = 1
  )
}



#' Plot the quantiles
#'
#' This function provides a basic plot of the model's and/or the observed data's
#' quantiles (see [dRiftDM::calc_stats]).
#'
#' @param obj an object inheriting from [dRiftDM::drift_dm] or
#' `dm_fits_subjects` (see [dRiftDM::load_fits_subjects]). If `obj` is of the
#' latter type, quantiles are calculated via
#' [dRiftDM::gather_stats] and then aggregated across subjects.
#'
#' @param source character, indicating whether CAFs of the model ("pred"), the
#'  observed data ("obs"), or both ("both") should be plotted. Default is
#'  "both".
#' @param probs numeric vector, providing the quantiles to plot. Default is
#'  `seq(0.1, 0.9, 0.1)`
#' @param to_plot character, providing the column of the data.frame returned by
#'  [dRiftDM::calc_stats] that should be used for plotting. Default is
#'  "Quant_Corr", indicating that the quantiles of correct responses will be
#'  plotted.
#' @param x_lab,y_lab character, providing a label for the x-axis and y-axis,
#'  respectively. Default is "RT" and "F(RT)", respectively.
#' @param x_lim,y_lim numeric vectors of length 2, providing the limits of the
#'  x-axis and y-axis. The default for `x_lim` is `c(0, t_max / 2)`, with
#'  `t_max` defined within `obj`. The default for `y_lim` is
#'  `c(0, 1)`.
#' @param line_cols character vector, indicating the color-coding of the
#'  conditions. Default colors are based on the `grDevices::rainbow` palette.
#'
#'
#'
#' @export
plot_quantiles <- function(obj, source = "both",
                           probs = seq(0.1, 0.9, 0.1), to_plot = "Quant_Corr",
                           x_lab = NULL, y_lab = NULL, x_lim = NULL,
                           y_lim = NULL, line_cols = NULL) {
  if (!inherits(obj, "drift_dm") &
    !inherits(obj, "dm_fits_subjects")) {
    stop("obj is not of type drift_dm or dm_fits_subjects")
  }

  # get values
  if (inherits(obj, "drift_dm")) {
    quantiles <- calc_stats(
      drift_dm_obj = obj, type = "quantiles", source = source,
      probs = probs
    )
  } else {
    quantiles <- gather_stats(
      fits_subjects = obj, type = "quantiles", source = source,
      probs = probs, verbose = 1
    )
  }

  agg_factors <- c("Source", "Cond", "Prob")
  stopifnot(agg_factors %in% colnames(quantiles))
  quantiles <- stats::aggregate(quantiles[to_plot],
    by = quantiles[agg_factors],
    FUN = mean
  )


  unique_conds <- unique(quantiles$Cond)
  if (inherits(obj, "drift_dm")) {
    t_max <- obj$prms_solve[["t_max"]]
  } else {
    t_max <- obj$drift_dm_fit_info$drift_dm_obj$prms_solve[["t_max"]]
  }


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

  # prepare plot
  plot(c(1, 2) ~ c(1, 1),
    col = "white", xlab = x_lab, ylab = y_lab, xlim = x_lim,
    ylim = y_lim
  )


  for (idx in seq_along(unique_conds)) {
    sub_dat <- quantiles[quantiles$Cond == unique_conds[idx], ]
    sub_dat_obs <- sub_dat[sub_dat$Source == "obs", ]
    if (nrow(sub_dat_obs) > 0) {
      graphics::points(sub_dat_obs$Prob ~ sub_dat_obs[[to_plot]],
        col = line_cols[idx]
      )
    }
    sub_dat_pred <- sub_dat[sub_dat$Source == "pred", ]
    if (nrow(sub_dat_pred) > 0) {
      graphics::points(sub_dat_pred$Prob ~ sub_dat_pred[[to_plot]],
        ty = "l", col = line_cols[idx]
      )
    }
  }


  graphics::legend("bottomright",
    legend = unique_conds,
    col = line_cols, lty = 1
  )
}



#' Plot Parameter Distribution(s)
#'
#' This function creates a histogram for each parameter in `fits_subjects`
#'
#' @param fits_subjects an object inheriting from `dm_fits_subjects` (see
#' [dRiftDM::load_fits_subjects])
#' @param include_fit_values logical, indicating if a histogram for
#' `log_like`, `AIC`, and `BIC` should be created as well
#' @param col the color of the histrogram bars
#'
#' @details
#' This function uses [dRiftDM::gather_parameters] to gather parameters across
#' subjects for a single object of type `dm_fits_subjects`. Subsequently it
#' creates a histogram for each parameter. The final plot has multiple panels,
#' each for one parameter.
#'
#'
#' @export
plot_prms <- function(fits_subjects, include_fit_values = F, col = "skyblue") {
  if (!inherits(fits_subjects, "dm_fits_subjects")) {
    stop("argument fits_subjects is not not of type dm_fits_subjects")
  }

  prms <- gather_parameters(fits_subjects)
  to_plot_names <- setdiff(colnames(prms), c("Subject"))
  if (!include_fit_values) {
    to_plot_names <- setdiff(to_plot_names, c("AIC", "BIC", "log_like"))
  }

  n_plots <- length(to_plot_names)
  n_rows <- ceiling(sqrt(n_plots))
  n_cols <- ceiling(n_plots / n_rows)

  withr::local_par(mfrow = c(n_rows, n_cols))
  for (one_prm_to_plot in to_plot_names) {
    graphics::hist(prms[[one_prm_to_plot]],
      col = col, xlab = "values",
      main = one_prm_to_plot
    )
  }
}
