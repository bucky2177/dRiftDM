# === FUNCTION FOR PLOTTING A DM
#' Plot trace(s) of a model
#'
#' @description
#' This functions creates a basic plot that shows a user-defined number
#' of traces (see [dRiftDM::simulate_trace]).
#' May come in handy when developing/testing/exploring a model.
#'
#' @param drift_dm_obj an object inheriting from [dRiftDM::drift_dm]
#' @param k numeric, the number of traces to simulate per condition
#' @param conds character vector, conditions for which traces shall be simulated
#' @param add_x logical, indicating whether a starting point should be
#'  added. Default is FALSE
#' @param seed numerical, an optional seed for reproducible sampling
#' @param sigma numerical, for controlling the diffusion parameter when
#'  simulating traces. The default (NULL) leaves the diffusion constant of
#'  `drift_dm_obj` unchanged.
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
#'
#' @export
plot_traces <- function(drift_dm_obj, k, conds = NULL, add_x = FALSE, seed = NULL,
                        sigma = NULL, x_lab = NULL, y_lab = NULL, x_lim = NULL,
                        y_lim = NULL, line_cols_ev = NULL, line_cols_b = NULL) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }

  # prepare some variables
  if (!is.null(sigma)) {
    stopifnot(sigma >= 0)
    drift_dm_obj$prms_solve["sigma"] <- sigma
  }
  t_max <- drift_dm_obj$prms_solve["t_max"]
  nt <- drift_dm_obj$prms_solve["nt"]
  t_vec <- seq(0, t_max, length.out = nt + 1)
  if (!is.null(conds)) {
    unique_conds <- conds
  } else {
    unique_conds <- unique(drift_dm_obj$conds)
  }


  # set default arguments
  if (is.null(x_lab)) {
    x_lab <- "Time"
  }

  if (is.null(y_lab)) {
    y_lab <- "Evidence"
  }

  b_vecs <- sapply(unique_conds, function(x) {
    drift_dm_obj$comp_funs$b_fun(
      prms_model = drift_dm_obj$prms_model,
      prms_solve = drift_dm_obj$prms_solve,
      t_vec = t_vec, one_cond = x,
      ddm_opts = drift_dm_obj$ddm_opts
    )
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
    exp_processes <- simulate_traces(drift_dm_obj,
      k = k, conds = one_cond,
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

    b_vec <- b_vecs[, one_cond]
    stopifnot(length(b_vec) == length(t_vec))
    graphics::points(b_vec ~ t_vec, ty = "l", col = line_cols_b)
    graphics::points(-b_vec ~ t_vec, ty = "l", col = line_cols_b)
  }

  graphics::legend("topright",
    legend = unique_conds,
    col = line_cols_ev, lty = 1, bg = "white"
  )
}


# === FUNCTION FOR PLOTTING STATISTICS

# Plots the CAFs
plot_cafs <- function(obj, source = "both", n_bins_cafs = NULL,
                      x_lab_cafs = NULL, y_lab_cafs = NULL, x_lim_cafs = NULL,
                      y_lim_cafs = NULL, line_cols_cafs = NULL, ...) {
  if (!inherits(obj, "drift_dm") & !inherits(obj, "dm_fits_ids")) {
    stop("obj is not of type drift_dm or dm_fits_ids")
  }

  # get values
  if (inherits(obj, "drift_dm")) {
    cafs <- calc_stats(
      drift_dm_obj = obj, type = "cafs", source = source,
      n_bins = n_bins_cafs
    )
    b_encoding <- attr(obj, "b_encoding")
  } else {
    cafs <- gather_stats(
      fits_ids = obj, type = "cafs", source = source,
      n_bins = n_bins_cafs, verbose = 1
    )
    b_encoding <- attr(obj$drift_dm_fit_info$drift_dm_obj, "b_encoding")
  }

  agg_factors <- c("Source", "Cond", "Bin")
  stopifnot(agg_factors %in% colnames(cafs))

  u_name <- names(b_encoding$u_name_value)
  caf_name <- paste0("P_", u_name)
  cafs <- stats::aggregate(cafs[caf_name], by = cafs[agg_factors], FUN = mean)

  unique_conds <- unique(cafs$Cond)

  # set default arguments
  if (is.null(x_lab_cafs)) {
    x_lab_cafs <- "Bins"
  }

  if (is.null(y_lab_cafs)) {
    y_lab_cafs <- paste("%", u_name)
  }

  if (is.null(y_lim_cafs)) {
    y_lim_cafs <- c(0, 1)
  }

  if (is.null(x_lim_cafs)) {
    x_lim_cafs <- as.numeric(c(min(cafs$Bin), max(cafs$Bin)))
  }

  if (!is.null(line_cols_cafs)) {
    if (length(line_cols_cafs) == 1) {
      line_cols_cafs <- rep(line_cols_cafs, length(unique_conds))
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
      graphics::points(sub_dat_obs[[caf_name]] ~ sub_dat_obs$Bin,
        col = line_cols_cafs[idx]
      )
    }

    sub_dat_pred <- sub_dat[sub_dat$Source == "pred", ]
    if (nrow(sub_dat_pred) > 0) {
      graphics::points(sub_dat_pred[[caf_name]] ~ sub_dat_pred$Bin,
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
                           line_cols_quantiles = NULL, ...) {
  if (!inherits(obj, "drift_dm") & !inherits(obj, "dm_fits_ids")) {
    stop("obj is not of type drift_dm or dm_fits_ids")
  }

  # get values
  if (inherits(obj, "drift_dm")) {
    quantiles <- calc_stats(
      drift_dm_obj = obj, type = "quantiles", source = source,
      probs = probs_quantiles
    )
    b_encoding <- attr(obj, "b_encoding")
  } else {
    quantiles <- gather_stats(
      fits_ids = obj, type = "quantiles", source = source,
      probs = probs_quantiles, verbose = 1
    )
    b_encoding <- attr(obj$drift_dm_fit_info$drift_dm_obj, "b_encoding")
  }



  u_name <- names(b_encoding$u_name_value)

  if (is.null(dv_quantiles)) {
    dv_quantiles <- paste0("Quant_", u_name)
  }

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
      line_cols_quantiles <- rep(line_cols_quantiles, length(unique_conds))
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
plot_delta_fun <- function(obj, source = "both", minuends_deltas,
                           subtrahends_deltas, probs_deltas = NULL, dvs_deltas = NULL,
                           x_lab_deltas = NULL, y_lab_deltas = NULL,
                           x_lim_deltas = NULL, y_lim_deltas = NULL,
                           line_cols_deltas = NULL, ...) {
  if (!inherits(obj, "drift_dm") & !inherits(obj, "dm_fits_ids")) {
    stop("obj is not of type drift_dm or dm_fits_ids")
  }

  # get values
  if (inherits(obj, "drift_dm")) {
    delta_fun <- calc_stats(
      drift_dm_obj = obj, type = "delta_funs", source = source,
      probs = probs_deltas, minuends = minuends_deltas,
      subtrahends = subtrahends_deltas, dv = dvs_deltas
    )
  } else {
    delta_fun <- gather_stats(
      fits_ids = obj, type = "delta_funs", source = source,
      probs = probs_deltas, minuends = minuends_deltas,
      subtrahends = subtrahends_deltas, dv = dvs_deltas, verbose = 1
    )
  }

  delta_columns <-
    names(delta_fun)[grepl(pattern = "^Delta", names(delta_fun))]
  avg_columns <-
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
  if (is.null(x_lab_deltas)) {
    x_lab_deltas <- "Avg"
  }

  if (is.null(y_lab_deltas)) {
    y_lab_deltas <- expression(Delta)
  }

  if (is.null(y_lim_deltas)) {
    y_lim_deltas <- c(-0.1, 0.2)
  }

  if (is.null(x_lim_deltas)) {
    x_lim_deltas <- c(0, t_max / 2)
  }

  if (!is.null(line_cols_deltas)) {
    if (length(line_cols_deltas) == 1) {
      line_cols_deltas <- rep(line_cols_deltas, length(delta_columns))
    } else {
      if (length(line_cols_deltas) != length(delta_columns)) {
        stop("number of line_cols_deltas must match the number of delta columns")
      }
    }
  } else {
    line_cols_deltas <- grDevices::rainbow(n = length(delta_columns))
  }


  # prepare plot
  plot(c(1, 2) ~ c(1, 1),
    col = "white", xlab = x_lab_deltas, ylab = y_lab_deltas,
    xlim = x_lim_deltas, ylim = y_lim_deltas
  )


  for (idx in seq_along(delta_columns)) {
    sub_dat_obs <- delta_fun[delta_fun$Source == "obs", ]
    sub_dat_obs <- sub_dat_obs[c(delta_columns[idx], avg_columns[idx])]
    if (nrow(sub_dat_obs) > 0) {
      graphics::points(sub_dat_obs[[1]] ~ sub_dat_obs[[2]],
        col = line_cols_deltas[idx]
      )
    }
    sub_dat_pred <- delta_fun[delta_fun$Source == "pred", ]
    sub_dat_pred <- sub_dat_pred[c(delta_columns[idx], avg_columns[idx])]
    if (nrow(sub_dat_pred) > 0) {
      graphics::points(sub_dat_pred[[1]] ~ sub_dat_pred[[2]],
        ty = "l", col = line_cols_deltas[idx]
      )
    }
  }

  graphics::legend("bottomright",
    legend = gsub("Delta_", "", delta_columns),
    col = line_cols_deltas, lty = 1
  )
}




#' Plot Statistics
#'
#' This function provides basic plots of statistics derived from a model.
#' Internally, it calls [dRiftDM::gather_stats] or [dRiftDM::calc_stats] and
#' subsequently creates basic plots.
#'
#' @param obj an object inheriting from [dRiftDM::drift_dm] or
#' `dm_fits_ids` (see [dRiftDM::load_fits_ids]). If `obj` is of the
#' latter type, statistics may be calculated via
#' [dRiftDM::gather_stats], depending on the requested statistics. For
#' quantiles, delta functions, and CAfs, observed data and model predictions
#' are averaged across individuals.
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
#' - `probs_quantiles`: the probabilities for which quantiles to compute
#' - `dv_quantiles`: The dependent variable to plot; default are quantiles
#' associated with the upper boundary
#' - `x_lab_quantiles`, `y_lab_quantiles`, `x_lim_quantiles`,
#'   `y_lim_quantiles`: axes labels and limits
#' - `line_cols_quantiles`: a character vector defining colors for each condition
#'
#' # Delta Functions
#'
#' Mandatory arguments
#' - `minuends_deltas`, `subtrahends_deltas`: character vectors specifying how to
#' calculate delta functions
#'
#' Optional arguments
#' - `probs_deltas`: the probabilities for which quantiles to calculate
#' - `dvs_deltas`: The dependent variable to plot; default are quantiles
#' associated with the upper boundary
#' - `x_lab_deltas`, `y_lab_deltas`, `x_lim_deltas`, `y_lim_deltas`: axes labels
#' and limits
#' - `line_cols_deltas`: a character vector defining colors for each line
#'
#'
#' @export
plot_stats <- function(obj, type, source = "both", mfrow = NULL, ...) {
  if (!inherits(obj, "drift_dm") & !inherits(obj, "dm_fits_ids")) {
    stop("obj is not of type drift_dm or dm_fits_ids")
  }

  if (!is.character(type) | length(type) == 0) {
    stop("type must be character vector of length >= 1")
  }

  type <- sapply(type, function(x) {
    match.arg(x, c("cafs", "quantiles", "delta_funs"))
  })
  type <- unname(type)

  if (!is.null(mfrow)) {
    withr::local_par(mfrow = mfrow)
  }

  # plot the requested plots sequentially
  for (one_type in type) {
    if (one_type == "cafs") {
      plot_cafs(obj = obj, source = source, ...)
    }
    if (one_type == "quantiles") {
      plot_quantiles(obj = obj, source = source, ...)
    }
    if (one_type == "delta_funs") {
      plot_delta_fun(obj = obj, source = source, ...)
    }
  }
}


# HISTOGRAM of Parameters

#' Plot Parameter Distribution(s)
#'
#' This function creates a histogram for each parameter in `fits_ids`
#'
#' @param fits_ids an object inheriting from `dm_fits_ids` (see
#' [dRiftDM::load_fits_ids])
#' @param include_fit_values logical, indicating if a histogram for
#' `log_like`, `AIC`, and `BIC` should be created as well
#' @param col the color of the histrogram bars
#' @param breaks argument controlling the number of break points
#' (see the arguments of [graphics::hist] for more information)
#'
#' @details
#' This function uses [dRiftDM::gather_parameters] to gather parameters across
#' individuals for a single object of type `dm_fits_ids`. Subsequently it
#' creates a histogram for each parameter. The final plot has multiple panels,
#' each for one parameter.
#'
#'
#' @export
plot_prms <- function(fits_ids, include_fit_values = F, col = "skyblue",
                      breaks = "Sturges") {
  if (!inherits(fits_ids, "dm_fits_ids")) {
    stop("argument fits_ids is not not of type dm_fits_ids")
  }

  prms <- gather_parameters(fits_ids)
  to_plot_names <- setdiff(colnames(prms), c("ID"))
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




#' Visualize Model Components
#'
#' This function takes an object of type [dRiftDM::drift_dm] and plots the
#' output of each component function in `drift_dm_obj$comp_funs`
#'
#' @param drift_dm_obj an object inheriting from [dRiftDM::drift_dm]
#' @param line_cols character vector, providing colors for each condition in
#' `drift_dm_obj$conds`. Default is `NULL`, meaning that colors are determined by
#' [grDevices::rainbow]
#' @param xlim_time numeric vector of length 2, providing the limits of the
#' x-axis of all plots related to the time space. Default is `NULL` which
#' refers to \eqn{[0, t_{max}/2]}
#'
#' @export
plot_model_comps <- function(drift_dm_obj, line_cols = NULL,
                             xlim_time = NULL) {
  # unpack parameters and conduct input checks
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("argument drift_dm_obj is not not of type drift_dm")
  }


  nx <- drift_dm_obj$prms_solve[["nx"]]
  nt <- drift_dm_obj$prms_solve[["nt"]]
  t_max <- drift_dm_obj$prms_solve[["t_max"]]
  x_vec <- seq(-1, 1, length.out = nx + 1)
  t_vec <- seq(0, t_max, length.out = nt + 1)
  conds <- drift_dm_obj$conds

  if (is.null(line_cols)) {
    line_cols <- grDevices::rainbow(n = length(conds))
  }
  if (length(line_cols) == 1) {
    line_cols <- rep(line_cols, length(conds))
  } else {
    if (length(line_cols) != length(conds)) {
      stop("number of line_cols must match the number of delta columns")
    }
  }



  if (is.null(xlim_time)) {
    xlim_time <- c(0, t_max / 2)
  }
  if (!is.numeric(xlim_time) | length(xlim_time) != 2) {
    stop("xlim_time must be a numeric vector of length 2")
  }


  withr::local_par(mfrow = c(3, 2))

  try(
    {
      # plot the drift rate
      drift_rates <- sapply(conds, function(one_cond) {
        drift_dm_obj$comp_funs$mu_fun(
          prms_model = drift_dm_obj$prms_model,
          prms_solve = drift_dm_obj$prms_solve,
          t_vec = t_vec, one_cond = one_cond,
          ddm_opts = drift_dm_obj$ddm_opts
        )
      }, simplify = F, USE.NAMES = T)
      y_lim_drift_rate <- range(unlist(drift_rates))

      plot(c(1, 2) ~ c(1, 1),
        col = "white", xlim = xlim_time,
        ylab = "Drift Rate", xlab = "Time [s]", ylim = y_lim_drift_rate,
        main = "mu_fun"
      )

      for (i in seq_along(conds)) {
        points(drift_rates[[conds[i]]] ~ t_vec, ty = "l", col = line_cols[i])
      }
    },
    silent = T
  )


  try(
    {
      # plot the integral of the drift rate
      drift_rates_int <- sapply(conds, function(one_cond) {
        drift_dm_obj$comp_funs$mu_int_fun(
          prms_model = drift_dm_obj$prms_model,
          prms_solve = drift_dm_obj$prms_solve,
          t_vec = t_vec, one_cond = one_cond,
          ddm_opts = drift_dm_obj$ddm_opts
        )
      }, simplify = F, USE.NAMES = T)
      y_lim_drift_rate_in <- range(unlist(drift_rates_int))

      plot(c(1, 2) ~ c(1, 1),
        col = "white", xlim = xlim_time,
        ylab = "Drift", xlab = "Time [s]", ylim = y_lim_drift_rate_in,
        main = "mu_int_fun"
      )

      for (i in seq_along(conds)) {
        points(drift_rates_int[[conds[i]]] ~ t_vec, ty = "l", col = line_cols[i])
      }
    },
    silent = T
  )

  try(
    {
      # plot the starting condition
      x_vals <- sapply(conds, function(one_cond) {
        drift_dm_obj$comp_funs$x_fun(
          prms_model = drift_dm_obj$prms_model,
          prms_solve = drift_dm_obj$prms_solve,
          x_vec = x_vec, one_cond = one_cond,
          ddm_opts = drift_dm_obj$ddm_opts
        )
      }, simplify = F, USE.NAMES = T)
      y_lim_x_vals <- range(unlist(x_vals))

      plot(c(1, 2) ~ c(1, 1),
        col = "white", xlim = c(-1, 1),
        ylab = "Density", xlab = "Evidence Value", ylim = y_lim_x_vals,
        main = "x_fun"
      )

      for (i in seq_along(conds)) {
        points(x_vals[[conds[i]]] ~ x_vec, ty = "l", col = line_cols[i])
      }
    },
    silent = T
  )

  try(
    {
      # plot the boundary
      bs <- sapply(conds, function(one_cond) {
        drift_dm_obj$comp_funs$b_fun(
          prms_model = drift_dm_obj$prms_model,
          prms_solve = drift_dm_obj$prms_solve,
          t_vec = t_vec, one_cond = one_cond,
          ddm_opts = drift_dm_obj$ddm_opts
        )
      }, simplify = F, USE.NAMES = T)
      y_lim_bs <- range(unlist(bs))

      plot(c(1, 2) ~ c(1, 1),
        col = "white", xlim = xlim_time,
        ylab = "Boundary", xlab = "Time [s]", ylim = y_lim_bs,
        main = "b_fun"
      )

      for (i in seq_along(conds)) {
        points(bs[[conds[i]]] ~ t_vec, ty = "l", col = line_cols[i])
      }
    },
    silent = T
  )

  try(
    {
      # plot the derivative of the boundary
      dt_bs <- sapply(conds, function(one_cond) {
        drift_dm_obj$comp_funs$dt_b_fun(
          prms_model = drift_dm_obj$prms_model,
          prms_solve = drift_dm_obj$prms_solve,
          t_vec = t_vec, one_cond = one_cond,
          ddm_opts = drift_dm_obj$ddm_opts
        )
      }, simplify = F, USE.NAMES = T)
      y_lim_dt_bs <- range(unlist(dt_bs))

      plot(c(1, 2) ~ c(1, 1),
        col = "white", xlim = xlim_time,
        ylab = "Derivative Boundary", xlab = "Time [s]", ylim = y_lim_dt_bs,
        main = "dt_b_fun"
      )

      for (i in seq_along(conds)) {
        points(dt_bs[[conds[i]]] ~ t_vec, ty = "l", col = line_cols[i])
      }
    },
    silent = T
  )


  try(
    {
      # plot the nn-decision time
      non_dec_vecs <- sapply(conds, function(one_cond) {
        drift_dm_obj$comp_funs$nt_fun(
          prms_model = drift_dm_obj$prms_model,
          prms_solve = drift_dm_obj$prms_solve,
          t_vec = t_vec, one_cond = one_cond,
          ddm_opts = drift_dm_obj$ddm_opts
        )
      }, simplify = F, USE.NAMES = T)
      y_lim_non_dec <- range(unlist(non_dec_vecs))

      plot(c(1, 2) ~ c(1, 1),
        col = "white", xlim = xlim_time,
        ylab = "Density", xlab = "Time [s]", ylim = y_lim_non_dec,
        main = "nt_fun"
      )

      for (i in seq_along(conds)) {
        points(non_dec_vecs[[conds[i]]] ~ t_vec, ty = "l", col = line_cols[i])
      }
    },
    silent = T
  )

  legend("topright", legend = conds, col = line_cols, lty = 1)
}
