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

  b_vecs <- sapply(unique_conds, function(x) {
    drift_dm_obj$comp_funs$b_fun(prms_model = drift_dm_obj$prms_model,
                                 prms_solve = drift_dm_obj$prms_solve,
                                 t_vec = t_vec, one_cond = x,
                                 ddm_opts = drift_dm_obj$ddm_opts)
  })

  if (is.null(y_lim)) {
    max_b <- max(b_vecs)
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

    b_vec <- b_vecs[, "comp"]
    stopifnot(length(b_vec) == length(t_vec))
    graphics::points(b_vec ~ t_vec, ty = "l", col = line_cols_b)
    graphics::points(-b_vec ~ t_vec, ty = "l", col = line_cols_b)
  }

  graphics::legend("topright",
    legend = unique_conds,
    col = line_cols_ev, lty = 1
  )
}


# === FUNCTION FOR PLOTTING STATISTICS

# Plots the CAFs
plot_cafs <- function(obj, source = "both", n_bins_cafs = NULL,
                      x_lab_cafs = NULL, y_lab_cafs = NULL, x_lim_cafs = NULL,
                      y_lim_cafs = NULL, line_cols_cafs = NULL) {
  if (!inherits(obj, "drift_dm") & !inherits(obj, "dm_fits_subjects")) {
    stop("obj is not of type drift_dm or dm_fits_subjects")
  }

  # get values
  if (inherits(obj, "drift_dm")) {
    cafs <- calc_stats(
      drift_dm_obj = obj, type = "cafs", source = source,
      n_bins = n_bins_cafs
    )
  } else {
    cafs <- gather_stats(
      fits_subjects = obj, type = "cafs", source = source,
      n_bins = n_bins_cafs, verbose = 1
    )
  }

  agg_factors <- c("Source", "Cond", "Bin")
  stopifnot(agg_factors %in% colnames(cafs))
  cafs <- stats::aggregate(cafs["P_Corr"], by = cafs[agg_factors], FUN = mean)

  unique_conds <- unique(cafs$Cond)

  # set default arguments
  if (is.null(x_lab_cafs)) {
    x_lab_cafs <- "Bins"
  }

  if (is.null(y_lab_cafs)) {
    y_lab_cafs <- "Accuracy [%]"
  }

  if (is.null(y_lim_cafs)) {
    y_lim_cafs <- c(0, 1)
  }

  if (is.null(x_lim_cafs)) {
    x_lim_cafs <- as.numeric(c(min(cafs$Bin), max(cafs$Bin)))
  }

  if (!is.null(line_cols_cafs)) {
    if (length(line_cols_cafs) == 1) {
      line_cols_cafs = rep(line_cols_cafs, length(unique_conds))
    } else {
      if (length(line_cols_cafs) != length(unique_conds)) {
        stop("number of line_cols must match the number of conditions")
      }
    }
  } else {
    line_cols_cafs <- grDevices::rainbow(n = length(unique_conds))
  }

  # prepare plot
  plot(c(1, 2) ~ c(1, 1),
    col = "white", xlab = x_lab_cafs, ylab = y_lab_cafs, xlim = x_lim_cafs,
    ylim = y_lim_cafs
  )

  for (idx in seq_along(unique_conds)) {
    sub_dat <- cafs[cafs$Cond == unique_conds[idx], ]
    sub_dat_obs <- sub_dat[sub_dat$Source == "obs", ]
    if (nrow(sub_dat_obs) > 0) {
      graphics::points(sub_dat_obs$P_Corr ~ sub_dat_obs$Bin,
                       col = line_cols_cafs[idx])
    }

    sub_dat_pred <- sub_dat[sub_dat$Source == "pred", ]
    if (nrow(sub_dat_pred) > 0) {
      graphics::points(sub_dat_pred$P_Corr ~ sub_dat_pred$Bin,
        ty = "l",
        col = line_cols_cafs[idx]
      )
    }
  }

  graphics::legend("bottomright",
    legend = unique_conds,
    col = line_cols_cafs, lty = 1
  )
}

# Plots the Quantiles
plot_quantiles <- function(obj, source = "both",
                           probs_quantiles = NULL, dv_quantiles = NULL,
                           x_lab_quantiles = NULL, y_lab_quantiles = NULL,
                           x_lim_quantiles = NULL, y_lim_quantiles = NULL,
                           line_cols_quantiles = NULL) {
  if (!inherits(obj, "drift_dm") & !inherits(obj, "dm_fits_subjects")) {
    stop("obj is not of type drift_dm or dm_fits_subjects")
  }

  # get values
  if (inherits(obj, "drift_dm")) {
    quantiles <- calc_stats(
      drift_dm_obj = obj, type = "quantiles", source = source,
      probs = probs_quantiles
    )
  } else {
    quantiles <- gather_stats(
      fits_subjects = obj, type = "quantiles", source = source,
      probs = probs_quantiles, verbose = 1
    )
  }

  if (is.null(dv_quantiles)) {
    dv_quantiles <- "Quant_Corr"
  }
  dv_quantiles = match.arg(dv_quantiles, c("Quant_Err", "Quant_Corr"))

  agg_factors <- c("Source", "Cond", "Prob")
  stopifnot(agg_factors %in% colnames(quantiles))
  quantiles <- stats::aggregate(quantiles[dv_quantiles],
    by = quantiles[agg_factors],
    FUN = mean
  )


  unique_conds <- unique(quantiles$Cond)
  if (inherits(obj, "drift_dm")) {
    t_max <- obj$prms_solve[["t_max"]]
  } else {
    t_max <- obj$drift_dm_fit_info$drift_dm_obj$prms_solve[["t_max"]]
  }


  # set default plot arguments
  if (is.null(x_lab_quantiles)) {
    x_lab_quantiles <- "RT"
  }

  if (is.null(y_lab_quantiles)) {
    y_lab_quantiles <- "F(RT)"
  }

  if (is.null(y_lim_quantiles)) {
    y_lim_quantiles <- c(0, 1)
  }

  if (is.null(x_lim_quantiles)) {
    x_lim_quantiles <- c(0, t_max / 2)
  }

  if (!is.null(line_cols_quantiles)) {
    if (length(line_cols_quantiles) == 1) {
      line_cols_quantiles = rep(line_cols_quantiles, length(unique_conds))
    } else {
      if (length(line_cols_quantiles) != length(unique_conds)) {
        stop("number of line_cols_quantiles must match the number of conditions")
      }
    }
  } else {
    line_cols_quantiles <- grDevices::rainbow(n = length(unique_conds))
  }

  # prepare plot
  plot(c(1, 2) ~ c(1, 1),
    col = "white", xlab = x_lab_quantiles, ylab = y_lab_quantiles,
    xlim = x_lim_quantiles, ylim = y_lim_quantiles
  )


  for (idx in seq_along(unique_conds)) {
    sub_dat <- quantiles[quantiles$Cond == unique_conds[idx], ]
    sub_dat_obs <- sub_dat[sub_dat$Source == "obs", ]
    if (nrow(sub_dat_obs) > 0) {
      graphics::points(sub_dat_obs$Prob ~ sub_dat_obs[[dv_quantiles]],
        col = line_cols_quantiles[idx]
      )
    }
    sub_dat_pred <- sub_dat[sub_dat$Source == "pred", ]
    if (nrow(sub_dat_pred) > 0) {
      graphics::points(sub_dat_pred$Prob ~ sub_dat_pred[[dv_quantiles]],
        ty = "l", col = line_cols_quantiles[idx]
      )
    }
  }

  graphics::legend("bottomright",
    legend = unique_conds,
    col = line_cols_quantiles, lty = 1
  )
}


# Plots the Delta Function(s)
plot_delta_fun <- function(obj, source = "both", minuend_delta,
                           subtrahend_delta, probs_delta = NULL, dv_delta = NULL,
                           x_lab_delta = NULL, y_lab_delta = NULL,
                           x_lim_delta = NULL, y_lim_delta = NULL,
                           line_cols_delta = NULL) {

  if (!inherits(obj, "drift_dm") & !inherits(obj, "dm_fits_subjects")) {
    stop("obj is not of type drift_dm or dm_fits_subjects")
  }

  # get values
  if (inherits(obj, "drift_dm")) {
    delta_fun <- calc_stats(
      drift_dm_obj = obj, type = "delta_fun", source = source,
      probs = probs_delta, minuend = minuend_delta,
      subtrahend = subtrahend_delta, dv = dv_delta
    )
  } else {
    delta_fun <- gather_stats(
      fits_subjects = obj, type = "delta_fun", source = source,
      probs = probs_delta, minuend = minuend_delta,
      subtrahend = subtrahend_delta, dv = dv_delta, verbose = 1
    )
  }

  delta_columns =
    names(delta_fun)[grepl(pattern = "^Delta", names(delta_fun))]
  avg_columns =
    names(delta_fun)[grepl(pattern = "^Avg", names(delta_fun))]

  agg_factors <- c("Source", "Prob")
  stopifnot(agg_factors %in% colnames(delta_fun))
  delta_fun <- stats::aggregate(delta_fun[c(delta_columns, avg_columns)],
                                by = delta_fun[agg_factors],
                                FUN = mean
  )

  if (inherits(obj, "drift_dm")) {
    t_max <- obj$prms_solve[["t_max"]]
  } else {
    t_max <- obj$drift_dm_fit_info$drift_dm_obj$prms_solve[["t_max"]]
  }

  # set default plot arguments
  if (is.null(x_lab_delta)) {
    x_lab_delta <- "Avg"
  }

  if (is.null(y_lab_delta)) {
    y_lab_delta <- expression(Delta)
  }

  if (is.null(y_lim_delta)) {
    y_lim_delta <- c(-0.1, 0.2)
  }

  if (is.null(x_lim_delta)) {
    x_lim_delta <- c(0, t_max / 2)
  }

  if (!is.null(line_cols_delta)) {
    if (length(line_cols_delta) == 1) {
      line_cols_delta = rep(line_cols_delta, length(delta_columns))
    } else {
      if (length(line_cols_delta) != length(delta_columns)) {
        stop("number of line_cols_delta must match the number of delta columns")
      }
    }
  } else {
    line_cols_delta <- grDevices::rainbow(n = length(delta_columns))
  }


  # prepare plot
  plot(c(1, 2) ~ c(1, 1),
       col = "white", xlab = x_lab_delta, ylab = y_lab_delta,
       xlim = x_lim_delta, ylim = y_lim_delta
  )


  for (idx in seq_along(delta_columns)) {
    sub_dat_obs <- delta_fun[delta_fun$Source == "obs", ]
    sub_dat_obs <- sub_dat_obs[c(delta_columns[idx], avg_columns[idx])]
    if (nrow(sub_dat_obs) > 0) {
      graphics::points(sub_dat_obs[[1]] ~ sub_dat_obs[[2]],
                       col = line_cols_delta[idx]
      )
    }
    sub_dat_pred <- delta_fun[delta_fun$Source == "pred", ]
    sub_dat_pred <- sub_dat_pred[c(delta_columns[idx], avg_columns[idx])]
    if (nrow(sub_dat_pred) > 0) {
      graphics::points(sub_dat_pred[[1]] ~ sub_dat_pred[[2]],
                       ty = "l", col = line_cols_delta[idx]
      )
    }
  }

  graphics::legend("bottomright",
                   legend = gsub("Delta_", "", delta_columns),
                   col = line_cols_delta, lty = 1
  )
}




#' Plot Statistics
#'
#' This function provides basic plots of statistics derived from a model.
#' Internally, it calls [dRiftDM::gather_stats] or [dRiftDM::calc_stats] and
#' subsequently creates basic plots.
#'
#' @param obj an object inheriting from [dRiftDM::drift_dm] or
#' `dm_fits_subjects` (see [dRiftDM::load_fits_subjects]). If `obj` is of the
#' latter type, statistics may be calculated via
#' [dRiftDM::gather_stats], depending on the requested statistics. For
#' quantiles, delta functions, and CAfs, observed data and model predictions
#' are averaged across subjects
#'
#' @param type character vector, indicating which statistics should be
#'  plotted (see [dRiftDM::calc_stats] for more info).
#' @param source character, indicating whether the model prediction ("pred"),
#' the observed data ("obs"), or both ("both") should be considered. Default is
#'  "both".
#' @param mfrow optional numeric vector, controls the number of rows and columns
#' of the final plot in case `type` is of length > 1. Default is NULL, meaning
#' that plots will not be grouped.
#' @param ... optional and mandatory arguments passed down to internal functions
#' creating the plots. For a full list of arguments see the details.
#'
#' @details
#'
#' For more information on each statistics, see [dRiftDM::calc_stats]
#'
#' # Conditional Accuracy Functions (CAFs)
#'
#' Optional arguments
#' - `n_bins_cafs`: the number of bins for which to calculate accuracy
#' - `x_lab_cafs`, `y_lab_cafs`, `x_lim_cafs`, `y_lim_cafs`: axes labels and limits
#' - `line_cols_cafs`: a character vector defining colors for each condition
#'
#' # Quantiles
#'
#' Optional arguments
#' - `probs_quantiles`: the probabilities for which quantiles to calculate
#' - `dv_quantiles`: The dependent variable to plot; options are `Quant_Corr` or
#'  `Quant_Err` for correct or incorrect responses
#' - `x_lab_quantiles`, `y_lab_quantiles`, `x_lim_quantiles`,
#'   `y_lim_quantiles`: axes labels and limits
#' - `line_cols_quantiles`: a character vector defining colors for each condition
#'
#' # Delta Functions
#'
#' Mandatory arguments
#' - `minuend_delta`, `subtrahend_delta`: character vectors specifying how to
#' calculate delta functions
#'
#' Optional arguments
#' - `probs_delta`: the probabilities for which quantiles to calculate
#' - `dv_delta`: The dependent variable to plot; options are `Quant_Corr` or
#'  `Quant_Err` for correct or incorrect responses
#' - `x_lab_delta`, `y_lab_delta`, `x_lim_delta`, `y_lim_delta`: axes labels
#' and limits
#' - `line_cols_delta`: a character vector defining colors for each line
#'
#'
#' @export
plot_stats <- function(obj, type, source = "both", mfrow = NULL, ...) {

  if (!inherits(obj, "drift_dm") & !inherits(obj, "dm_fits_subjects")) {
    stop("obj is not of type drift_dm or dm_fits_subjects")
  }

  if (!is.character(type) | length(type) == 0) {
    stop("type must be character vector of length >= 1")
  }

  type <- sapply(type, function(x) {
    match.arg(x, c("cafs", "quantiles", "delta_fun"))
  })
  type <- unname(type)

  if (!is.null(mfrow)) {
    withr::local_par(mfrow)
  }

  # plot the requested plots sequentially
  for (one_type in type) {

    if (one_type == "cafs")
      plot_cafs(obj = obj, source = source, ...)
    if (one_type == "quantiles")
      plot_quantiles(obj = obj, source = source, ...)
    if (one_type == "delta_fun")
      plot_delta_fun(obj = obj, source = source, ...)
  }
}


# HISTOGRAM of Parameters

#' Plot Parameter Distribution(s)
#'
#' This function creates a histogram for each parameter in `fits_subjects`
#'
#' @param fits_subjects an object inheriting from `dm_fits_subjects` (see
#' [dRiftDM::load_fits_subjects])
#' @param include_fit_values logical, indicating if a histogram for
#' `log_like`, `AIC`, and `BIC` should be created as well
#' @param col the color of the histrogram bars
#' @param breaks argument controlling the number of break points
#' (see the arguments of [graphics::hist] for more information)
#'
#' @details
#' This function uses [dRiftDM::gather_parameters] to gather parameters across
#' subjects for a single object of type `dm_fits_subjects`. Subsequently it
#' creates a histogram for each parameter. The final plot has multiple panels,
#' each for one parameter.
#'
#'
#' @export
plot_prms <- function(fits_subjects, include_fit_values = F, col = "skyblue",
                      breaks = "Sturges") {
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
      main = one_prm_to_plot, breaks = breaks
    )
  }
}
