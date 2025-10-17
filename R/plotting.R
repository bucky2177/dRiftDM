# FUNCTION FOR PLOTTING TRACES ---------------------------------------------

# Core plotting function to handle trace and boundary plotting
plot_one_traces <- function(
  traces_obj,
  col,
  col_b,
  xlab,
  ylab,
  xlim,
  ylim,
  new_plot,
  ...
) {
  # set_default_arguments() was already called from plot.traces_dm_list
  dots = list(...)

  # Initialize plot
  if (new_plot) {
    plot(
      c(1, 2) ~ c(1, 1),
      col = "white",
      xlab = xlab,
      ylab = ylab,
      xlim = xlim,
      ylim = ylim,
      cex.axis = dots$cex.axis,
      cex.lab = dots$cex.lab,
      cex.main = dots$cex.main,
      main = dots$main
    )
    graphics::abline(h = 0, col = dots$horiz.col, lwd = dots$horiz.lwd)
  }

  # Plot each condition's traces
  e_samples <- unpack_obj(traces_obj)
  t_vec <- attr(traces_obj, "t_vec")

  for (i in 1:nrow(e_samples)) {
    one_trace <- e_samples[i, ]
    graphics::points(
      one_trace ~ t_vec,
      col = col,
      type = "l",
      lty = dots$pred.lty,
      lwd = dots$pred.lwd
    )
  }

  b_vals <- attr(traces_obj, "b_vals")
  graphics::points(
    b_vals ~ t_vec,
    type = "l",
    col = col_b,
    lwd = dots$b.lwd,
    lty = dots$b.lty
  )
  graphics::points(
    -b_vals ~ t_vec,
    type = "l",
    col = col_b,
    lwd = dots$b.lwd,
    lty = dots$b.lty
  )
}


#' Plot Traces of a Drift Diffusion Model
#'
#'
#' @description
#' Creates a plot of simulated traces (i.e., simulated evidence
#' accumulation processes) from a drift diffusion model. Such plots are useful
#' for exploring and testing model behavior.
#'
#' @param x an object of type `traces_dm_list` or `traces_dm`, containing the
#' traces to be plotted, resulting from a call to [dRiftDM::simulate_traces()].
#' @inheritParams plot.cafs
#' @param col_b a character vector, specifying the color of the boundary for
#' each condition. If a single color is provided, it is repeated for all
#' conditions. Default is `"black"`.
#'
#' @details
#'
#' `plot.traces_dm_list()` iterates over all conditions and plots the traces.
#'  It includes a legend with condition labels.
#'
#' `plot.traces_dm` plots a single set of traces. Because
#'  [dRiftDM::simulate_traces()] returns an object of type `traces_dm_list` per
#'  default, users will likely call `plot.traces_dm_list()` in most cases; and
#'  not `plot.traces_dm`. `plot.traces_dm` is only relevant if users explicitly
#'  extract and provide an object of type `traces_dm`.
#'
#' The function automatically generates the upper and lower boundaries based on
#' the information stored within `x`.
#'
#' @returns
#' `NULL` invisibly
#'
#'
#' @examples
#' # get a couple of traces for demonstration purpose
#' a_model <- dmc_dm()
#' some_traces <- simulate_traces(a_model, k = 3)
#'
#' # Plots for traces_dm_list objects ----------------------------------------
#' # basic plot
#' plot(some_traces)
#'
#' # a slightly more beautiful plot :)
#' plot(some_traces,
#'   col = c("green", "red"),
#'   xlim = c(0, 0.35),
#'   xlab = "Time [s]",
#'   ylab = bquote(Realizations ~ of ~ X[t]),
#'   legend_pos = "bottomright"
#' )
#'
#' # Plots for traces_dm objects ---------------------------------------------
#' # we can also extract a single set of traces and plot them
#' one_set_traces <- some_traces$comp
#' plot(one_set_traces)
#'
#' # modifications to the plot work in the same way
#' plot(one_set_traces,
#'   col = "green",
#'   xlim = c(0, 0.35),
#'   xlab = "Time [s]",
#'   ylab = bquote(Realizations ~ of ~ X[t]),
#'   legend = "just comp"
#' )
#'
#' @seealso [dRiftDM::simulate_traces]
#'
#' @export
plot.traces_dm_list <- function(
  x,
  ...,
  conds = NULL,
  col = NULL,
  col_b = NULL,
  xlim = NULL,
  ylim = NULL,
  xlab = "Time",
  ylab = "Evidence"
) {
  # input handling
  all_conds <- names(x)
  conds <- conds %||% all_conds
  conds <- match.arg(arg = conds, choices = all_conds, several.ok = TRUE)

  t_vec <- attr(x, "t_vec")
  dots <- list(...)

  dots <- set_default_arguments(dots = dots, conds, NULL)

  # for backward compatibility (remove in the future)
  if (!is.null(dots$lwd)) {
    dots$b.lwd = dots$lwd
    dots$pred.lwd = dots$lwd
  }
  if (!is.null(dots$lty)) {
    dots$b.lty = dots$lty
    dots$pred.lty = dots$lty
    lifecycle::deprecate_warn(
      "0.3.0",
      "dRiftDM::plot.traces_dm_list(lty = )",
      details = "Please use the optional arguments `b.lty` and `pred.lty`"
    )
  }
  if (!is.null(dots$type)) {
    lifecycle::deprecate_warn(
      "0.3.0",
      "dRiftDM::plot.traces_dm_list(type = )",
      details = "the argument is no longer supported"
    )
  }
  if (!is.null(dots$legend_pos)) {
    dots$legend.pos = dots$legend_pos
    lifecycle::deprecate_warn(
      "0.3.0",
      "dRiftDM::plot.traces_dm_list(legend_pos = )",
      details = "Please use the optional `legend.pos` argument instead"
    )
  }

  # Set defaults for colors, x/y limits, and legend
  col <- set_default_colors(colors = col, n = length(all_conds))
  col <- col[which(all_conds %in% conds)]

  col_b <- col_b %||% "black"
  col_b <- set_default_colors(colors = col_b, n = length(all_conds))
  col_b <- col_b[which(all_conds %in% conds)]

  xlim <- xlim %||% c(0, max(t_vec) / 4)
  all_vals <- unlist(x[conds])
  ylim <- ylim %||% c(-max(all_vals, na.rm = TRUE), max(all_vals, na.rm = TRUE))

  # iterate over all traces
  for (i in seq_along(x[conds])) {
    new_plot = if (i == 1) TRUE else FALSE

    requ_args <- list(
      traces_obj = x[[i]],
      col = col[i],
      col_b = col_b[i],
      xlab = xlab,
      ylab = ylab,
      xlim = xlim,
      ylim = ylim,
      new_plot = new_plot
    )

    do.call(plot_one_traces, args = c(requ_args, dots))
  }

  # plot the legend
  if (is.character(dots$legend.pos)) {
    x_leg <- dots$legend.pos
    y_leg <- NULL
  } else {
    x_leg <- dots$legend.pos[1]
    y_leg <- dots$legend.pos[2]
  }

  if (all(!is.na(dots$legend)) && length(dots$legend) >= 1) {
    graphics::legend(
      x = x_leg,
      y = y_leg,
      legend = dots$legend,
      col = col,
      lwd = dots$pred.lwd,
      lty = dots$pred.lty,
      bg = dots$legend.bg,
      bty = dots$bty,
      box.lwd = dots$box.lwd,
      box.col = dots$box.col,
      cex = dots$cex.legend,
      box.lty = dots$box.lty
    )
  }
  invisible()
}

#' @rdname plot.traces_dm_list
#' @export
plot.traces_dm <- function(x, ...) {
  # create a pseudo traces_dm_list object
  tmp <- list(dummy_cond = x)
  attr(tmp, "t_vec") <- attr(x, "t_vec")

  # suppress a legend per default
  dots <- list(...)
  dots$legend <- dots$legend %||% NA

  # and plot the traces
  do.call(plot.traces_dm_list, c(list(x = tmp), dots))
  invisible()
}


# FUNCTIONS FOR PLOTTING STATISTICS ---------------------------------------

#' Plot Conditional Accuracy Functions (CAFs)
#'
#' @description
#' Visualizes conditional accuracy functions (CAFs) for observed and/or
#' predicted data. This is useful for assessing model fit or exploring
#' response patterns across conditions or participants.
#'
#' @param x an object of `type = "cafs"`, typically returned by
#'   [dRiftDM::calc_stats()].
#' @param id a numeric or character, specifying the ID of a single participant
#'   to plot. If `length(id) > 1`, `plot.cafs()` is called recursively for
#'   each entry. Each `id` must match an entry in the `ID` column of `x`.
#' @param conds a character vector specifying the conditions to plot.
#'   Defaults to all available conditions.
#' @param col a character vector specifying colors for each condition. If a
#'   single color is provided, it is repeated for all conditions.
#' @param xlim a numeric vector of length 2, specifying the x-axis limits.
#' @param ylim a numeric vector of length 2, specifying the y-axis limits.
#' @param xlab,ylab character strings for the x- and y-axis labels.
#' @param interval_obs,interval_pred logicals; if `TRUE` and `x` contains a
#' column named `Estimate`, error bars for observed data and shaded contours
#' for predicted data are drawn, respectively.
#' @param ... additional graphical arguments passed to plotting functions.
#'  See [dRiftDM::set_default_arguments()] for the full list of supported
#'  options.
#'
#' @details
#' If `x` contains multiple `ID`s and no specific `id` is provided, the
#' function aggregates across participants before plotting.
#'
#' Observed CAFs are shown as points, and predicted CAFs as lines. When
#' `interval = TRUE` and the input includes interval estimates (i.e., the
#' column `Estimate` exists), the plot includes error bars for observed data
#' and shaded contours for model predictions.
#'
#' Colors, symbols, and line styles can be customized via `...`.
#'
#' @returns
#' Returns `NULL` invisibly. The function is called for its side effect of
#' generating a plot.
#'
#' @examples
#' # Example 1: Model predictions only ---------------------------------------
#' a_model <- dmc_dm(t_max = 1.5, dx = .01, dt = .005)
#' cafs <- calc_stats(a_model, type = "cafs")
#' plot(cafs)
#' plot(cafs, col = c("green", "red"), ylim = c(0.5, 1))
#'
#' # Example 2: Observed and predicted data ----------------------------------
#' obs_data(a_model) <- dmc_synth_data
#' cafs <- calc_stats(a_model, type = "cafs")
#' plot(cafs)
#'
#' # Example 3: Observed data only -------------------------------------------
#' cafs <- calc_stats(dmc_synth_data, type = "cafs")
#' plot(cafs)
#'
#' # Example 4: Observed data with interval ----------------------------------
#' cafs <- calc_stats(dmc_synth_data, type = "cafs", resample = TRUE)
#' plot(cafs)
#'
#' @export
plot.cafs <- function(
  x,
  ...,
  id = NULL,
  conds = NULL,
  col = NULL,
  xlim = NULL,
  ylim = c(0, 1),
  xlab = "Bins",
  ylab = NULL,
  interval_obs = TRUE,
  interval_pred = TRUE
) {
  cafs <- x
  caf_name <- grep("^P_", colnames(cafs), value = TRUE)

  # get the data to plot
  if (!is.null(cafs$ID) && length(unique(cafs$ID)) > 1) {
    if (!is.null(id)) {
      if (!all(id %in% unique(cafs$ID))) {
        mis_ids <- setdiff(id, unique(cafs$ID))
        stop(
          "The following IDs were not found: ",
          paste(mis_ids, collapse = ", ")
        )
      }

      if (length(id) == 1) {
        cafs <- cafs[cafs$ID == id, ]
      } else {
        lapply(id, function(one_id) {
          plot.cafs(
            x = x,
            ...,
            id = one_id,
            conds = conds,
            col = col,
            xlim = xlim,
            ylim = ylim,
            xlab = xlab,
            ylab = ylab,
            interval_obs = interval_obs,
            interval_pred = interval_pred
          )
        })
        return(invisible())
      }
    } else {
      message("Aggregating across ID")
      cafs <- aggregate_stats(stat_df = cafs)
    }
  }

  # set default arguments
  all_conds <- unique(cafs$Cond)
  if (is.null(conds)) {
    conds <- all_conds
  }
  conds <- match.arg(arg = conds, choices = all_conds, several.ok = TRUE)

  if (is.null(ylab)) {
    # f(upper_boundary_name)
    u_name <- substr(caf_name, 3, nchar(caf_name))
    ylab <- paste("f(", u_name, ")", sep = "")
  }
  xlim <- xlim %||% c(min(cafs$Bin), max(cafs$Bin))

  # Set colors
  col <- set_default_colors(colors = col, n = length(all_conds))
  col <- col[which(all_conds %in% conds)]

  # further optional arguments related to plot, points, and legend
  dots = list(...)
  dots = set_default_arguments(dots, conds, id)
  dots$err.col <- set_default_colors(
    colors = dots$err.col,
    n = length(all_conds)
  )
  dots$err.col <- dots$err.col[which(all_conds %in% conds)]

  # prepare plot
  withr::local_par(family = dots$family)
  plot(
    c(1, 2) ~ c(1, 1),
    col = "white",
    xlab = xlab,
    ylab = ylab,
    xlim = xlim,
    ylim = ylim,
    cex.axis = dots$cex.axis,
    cex.lab = dots$cex.lab,
    cex.main = dots$cex.main,
    main = dots$main
  )

  # figure out if an Estimate column exists
  if ("Estimate" %in% names(cafs)) {
    orig = cafs[cafs$Estimate == "orig", ]
    l_u = cafs[cafs$Estimate != "orig", ]
  } else {
    orig = cafs
    l_u = NULL
  }

  # first plot the observed data
  for (idx in seq_along(conds)) {
    sub_dat_l_u <- l_u[l_u$Cond == conds[idx] & l_u$Source == "obs", ]
    sub_dat_orig <- orig[orig$Cond == conds[idx] & orig$Source == "obs", ]

    if (nrow(sub_dat_orig) == 0) {
      next # if there is no observed data skip this part
    }

    if ((!is.null(sub_dat_l_u) && nrow(sub_dat_l_u) > 0) && interval_obs) {
      borders <- sort(unique(sub_dat_l_u$Estimate))
      stopifnot(length(borders) == 2)
      df_lower <- sub_dat_l_u[sub_dat_l_u$Estimate == borders[1], ]
      df_upper <- sub_dat_l_u[sub_dat_l_u$Estimate == borders[2], ]

      err_y0 <- df_lower[[caf_name]]
      err_y1 <- df_upper[[caf_name]]
      err_x <- sub_dat_orig$Bin
      keep <- abs(err_y1 - err_y0) > diff(ylim) * dots$err.eps

      graphics::arrows(
        x0 = err_x[keep],
        y0 = err_y0[keep],
        x1 = err_x[keep],
        y1 = err_y1[keep],
        angle = 90,
        code = 3,
        length = dots$err.width,
        col = dots$err.col[idx]
      )
    }

    graphics::points(
      sub_dat_orig[[caf_name]] ~ sub_dat_orig$Bin,
      col = col[idx],
      type = "p",
      pch = dots$obs.pch,
      bg = dots$obs.pt.bg,
      cex = dots$cex.pt,
    )
  }

  # then plot the predicted data
  for (idx in seq_along(conds)) {
    sub_dat_l_u <- l_u[l_u$Cond == conds[idx] & l_u$Source == "pred", ]
    sub_dat_orig <- orig[orig$Cond == conds[idx] & orig$Source == "pred", ]

    if (nrow(sub_dat_orig) == 0) {
      next # if there is no observed data skip this part
    }

    if ((!is.null(sub_dat_l_u) && nrow(sub_dat_l_u) > 0) && interval_pred) {
      borders <- sort(unique(sub_dat_l_u$Estimate))
      stopifnot(length(borders) == 2)
      df_lower <- sub_dat_l_u[sub_dat_l_u$Estimate == borders[1], ]
      df_upper <- sub_dat_l_u[sub_dat_l_u$Estimate == borders[2], ]

      x_poly <- c(df_lower$Bin, rev(df_upper$Bin))
      y_poly <- c(df_lower[[caf_name]], rev(df_upper[[caf_name]]))

      graphics::polygon(
        x = x_poly,
        y = y_poly,
        col = grDevices::adjustcolor(col[idx], alpha.f = dots$alpha),
        border = NA
      )
    }

    graphics::points(
      sub_dat_orig[[caf_name]] ~ sub_dat_orig$Bin,
      col = col[idx],
      type = "l",
      lty = dots$pred.lty,
      lwd = dots$pred.lwd
    )
  }

  # plot the legend
  dots$pred.lty <- if (!any(cafs$Source == "pred")) NULL else dots$pred.lty
  dots$obs.pch <- if (!any(cafs$Source == "obs")) NULL else dots$obs.pch
  if (is.character(dots$legend.pos)) {
    x_leg <- dots$legend.pos
    y_leg <- NULL
  } else {
    x_leg <- dots$legend.pos[1]
    y_leg <- dots$legend.pos[2]
  }

  if (all(!is.na(dots$legend)) && length(dots$legend) >= 1) {
    graphics::legend(
      x = x_leg,
      y = y_leg,
      legend = dots$legend,
      col = col,
      lwd = dots$pred.lwd,
      lty = dots$pred.lty,
      pch = dots$obs.pch,
      pt.bg = dots$obs.pt.bg,
      pt.cex = dots$cex.pt,
      bg = dots$legend.bg,
      bty = dots$bty,
      box.lwd = dots$box.lwd,
      box.col = dots$box.col,
      cex = dots$cex.legend,
      box.lty = dots$box.lty
    )
  }
  invisible()
}


#' Plot Response Time Quantiles
#'
#' @description
#' Visualizes response time quantiles for observed and/or predicted data across
#' experimental conditions. This is useful for assessing model fit or exploring
#' response patterns across conditions or participants.
#'
#' @param x an object of `type = "quantiles"`, typically returned by
#'   [dRiftDM::calc_stats()].
#' @inheritParams plot.cafs
#' @param dv a character string indicating the dependent variable to plot.
#'   Defaults to the quantiles for the upper boundary.
#'
#' @details
#' If `x` contains multiple `ID`s and no specific `id` is provided, the
#' function aggregates across participants before plotting.
#'
#' Observed quantiles are shown as points, and predicted quantiles as lines.
#' When `interval = TRUE` and the input includes interval estimates (i.e., the
#' column `Estimate` exists), the plot includes error bars for observed data
#' and shaded contours for model predictions.
#'
#' Colors, symbols, and line styles can be customized via `...`.
#'
#' @returns
#' Returns `NULL` invisibly. The function is called for its side effect of
#' generating a plot.
#'
#' @examples
#' # Example 1: Model predictions only ---------------------------------------
#' a_model <- dmc_dm(t_max = 1.5, dx = .01, dt = .005)
#' quantiles <- calc_stats(a_model, type = "quantiles")
#' plot(quantiles)
#' plot(quantiles, col = c("green", "red"), xlim = c(0.2, 0.6))
#'
#' # Example 2: Observed and predicted data ----------------------------------
#' obs_data(a_model) <- dmc_synth_data
#' quantiles <- calc_stats(a_model, type = "quantiles")
#' plot(quantiles)
#'
#' # Example 3: Observed data only -------------------------------------------
#' quantiles <- calc_stats(dmc_synth_data, type = "quantiles")
#' plot(quantiles)
#'
#' # Example 4: Observed data with interval ----------------------------------
#' cafs <- calc_stats(dmc_synth_data, type = "quantiles", resample = TRUE)
#' plot(cafs)
#'
#' @export
plot.quantiles <- function(
  x,
  ...,
  id = NULL,
  conds = NULL,
  dv = NULL,
  col = NULL,
  xlim = NULL,
  ylim = c(0, 1),
  xlab = "RT [s]",
  ylab = "F(RT)",
  interval_obs = TRUE,
  interval_pred = TRUE
) {
  quantiles <- x

  # Handle multiple IDs
  if (!is.null(quantiles$ID) && length(unique(quantiles$ID)) > 1) {
    if (!is.null(id)) {
      if (!all(id %in% unique(quantiles$ID))) {
        mis_ids <- setdiff(id, unique(quantiles$ID))
        stop(
          "The following IDs were not found: ",
          paste(mis_ids, collapse = ", ")
        )
      }

      if (length(id) == 1) {
        quantiles <- quantiles[quantiles$ID == id, ]
      } else {
        lapply(id, function(one_id) {
          plot.quantiles(
            x = x,
            ...,
            id = one_id,
            conds = conds,
            dv = dv,
            col = col,
            xlim = xlim,
            ylim = ylim,
            xlab = xlab,
            ylab = ylab,
            interval_obs = interval_obs,
            interval_pred = interval_pred
          )
        })
        return(invisible())
      }
    } else {
      message("Aggregating across ID")
      quantiles <- aggregate_stats(stat_df = quantiles)
    }
  }

  # Determine conditions
  all_conds <- unique(quantiles$Cond)
  if (is.null(conds)) {
    conds <- all_conds
  }
  conds <- match.arg(arg = conds, choices = all_conds, several.ok = TRUE)

  # Determine dependent variable
  quant_columns <- grep("^Quant_", names(quantiles), value = TRUE)
  if (is.null(dv)) {
    u_name <- names(attr(quantiles, "b_coding")$u_name_value)
    dv <- paste("Quant", u_name, sep = "_")
  }
  dv <- match.arg(dv, quant_columns)

  # Set axis limits
  xlim <- xlim %||% c(min(quantiles[[dv]]) * 0.75, max(quantiles[[dv]]) * 1.25)

  # Set colors
  col <- set_default_colors(colors = col, n = length(all_conds))
  col <- col[which(all_conds %in% conds)]

  # Collect additional plotting options
  dots <- list(...)
  dots <- set_default_arguments(dots, conds, id)
  dots$err.col <- set_default_colors(
    colors = dots$err.col,
    n = length(all_conds)
  )
  dots$err.col <- dots$err.col[which(all_conds %in% conds)]

  # Initialize plot
  withr::local_par(family = dots$family)
  plot(
    c(1, 2) ~ c(1, 1),
    type = "n",
    xlab = xlab,
    ylab = ylab,
    xlim = xlim,
    ylim = ylim,
    cex.axis = dots$cex.axis,
    cex.lab = dots$cex.lab,
    cex.main = dots$cex.main,
    main = dots$main
  )

  # figure out if an Estimate column exists
  if ("Estimate" %in% names(quantiles)) {
    orig = quantiles[quantiles$Estimate == "orig", ]
    l_u = quantiles[quantiles$Estimate != "orig", ]
  } else {
    orig = quantiles
    l_u = NULL
  }

  # first plot the observed data
  for (idx in seq_along(conds)) {
    sub_dat_l_u <- l_u[l_u$Cond == conds[idx] & l_u$Source == "obs", ]
    sub_dat_orig <- orig[orig$Cond == conds[idx] & orig$Source == "obs", ]

    if (nrow(sub_dat_orig) == 0) {
      next # if there is no observed data skip this part
    }

    if ((!is.null(sub_dat_l_u) && nrow(sub_dat_l_u) > 0) && interval_obs) {
      borders <- sort(unique(sub_dat_l_u$Estimate))
      stopifnot(length(borders) == 2)
      df_lower <- sub_dat_l_u[sub_dat_l_u$Estimate == borders[1], ]
      df_upper <- sub_dat_l_u[sub_dat_l_u$Estimate == borders[2], ]

      err_x0 <- df_lower[[dv]]
      err_x1 <- df_upper[[dv]]
      err_y <- sub_dat_orig$Prob
      keep <- abs(err_x1 - err_x0) > diff(ylim) * dots$err.eps

      graphics::arrows(
        y0 = err_y[keep],
        x0 = err_x0[keep],
        y1 = err_y[keep],
        x1 = err_x1[keep],
        angle = 90,
        code = 3,
        length = dots$err.width,
        col = dots$err.col[idx]
      )
    }

    graphics::points(
      sub_dat_orig$Prob ~ sub_dat_orig[[dv]],
      col = col[idx],
      type = "p",
      pch = dots$obs.pch,
      bg = dots$obs.pt.bg,
      cex = dots$cex.pt,
    )
  }

  # then plot the predicted data
  for (idx in seq_along(conds)) {
    sub_dat_l_u <- l_u[l_u$Cond == conds[idx] & l_u$Source == "pred", ]
    sub_dat_orig <- orig[orig$Cond == conds[idx] & orig$Source == "pred", ]

    if (nrow(sub_dat_orig) == 0) {
      next # if there is no observed data skip this part
    }

    if ((!is.null(sub_dat_l_u) && nrow(sub_dat_l_u) > 0) && interval_pred) {
      borders <- sort(unique(sub_dat_l_u$Estimate))
      stopifnot(length(borders) == 2)
      df_lower <- sub_dat_l_u[sub_dat_l_u$Estimate == borders[1], ]
      df_upper <- sub_dat_l_u[sub_dat_l_u$Estimate == borders[2], ]

      y_poly <- c(df_lower$Prob, rev(df_upper$Prob))
      x_poly <- c(df_lower[[dv]], rev(df_upper[[dv]]))

      graphics::polygon(
        x = x_poly,
        y = y_poly,
        col = grDevices::adjustcolor(col[idx], alpha.f = dots$alpha),
        border = NA
      )
    }

    graphics::points(
      sub_dat_orig$Prob ~ sub_dat_orig[[dv]],
      col = col[idx],
      type = "l",
      lty = dots$pred.lty,
      lwd = dots$pred.lwd
    )
  }

  # Legend setup
  dots$pred.lty <- if (!any(quantiles$Source == "pred")) NULL else dots$pred.lty
  dots$obs.pch <- if (!any(quantiles$Source == "obs")) NULL else dots$obs.pch

  if (is.character(dots$legend.pos)) {
    x_leg <- dots$legend.pos
    y_leg <- NULL
  } else {
    x_leg <- dots$legend.pos[1]
    y_leg <- dots$legend.pos[2]
  }

  if (all(!is.na(dots$legend)) && length(dots$legend) >= 1) {
    graphics::legend(
      x = x_leg,
      y = y_leg,
      legend = dots$legend,
      col = col,
      lwd = dots$pred.lwd,
      lty = dots$pred.lty,
      pch = dots$obs.pch,
      pt.bg = dots$obs.pt.bg,
      pt.cex = dots$cex.pt,
      bg = dots$legend.bg,
      bty = dots$bty,
      box.lwd = dots$box.lwd,
      box.col = dots$box.col,
      cex = dots$cex.legend,
      box.lty = dots$box.lty
    )
  }

  invisible()
}


#' Plot Delta Functions
#'
#' @description
#' Visualizes delta functions for observed and/or predicted data. This is useful
#' for assessing model fit or exploring the model behavior
#'
#' @param x an object of `type = "delta_funs"`, typically returned by
#'   [dRiftDM::calc_stats()].
#' @inheritParams plot.cafs
#' @param dv a character vector indicating the delta function(s) to plot.
#'   Defaults to all columns in `x` that begin with `"Delta_"`.
#'
#' @details
#' If `x` contains multiple `ID`s and no specific `id` is provided, the
#' function aggregates across participants before plotting.
#'
#' Observed delta functions are shown as points, and predicted delta functions
#' as lines. When `interval_obs = TRUE` or `interval_pred = TRUE` and the input
#' includes interval estimates (i.e., the column `Estimate` exists), the plot
#' includes error bars for observed data and shaded contours for model
#' predictions.
#'
#' Colors, symbols, and line styles can be customized via `...`.
#'
#' @returns
#' Returns `NULL` invisibly. The function is called for its side effect of
#' generating a plot.
#'
#' @examples
#' # Example 1: Model predictions only ---------------------------------------
#' a_model <- dmc_dm(t_max = 1.5, dx = .01, dt = .005)
#' deltas <- calc_stats(
#'   a_model,
#'   type = "delta_funs",
#'   minuends = "incomp",
#'   subtrahends = "comp"
#' )
#' plot(deltas)
#' plot(deltas, col = "black", lty = 2, xlim = c(0.2, 0.65))
#'
#' # Example 2: Observed and predicted data ----------------------------------
#' obs_data(a_model) <- dmc_synth_data
#' deltas <- calc_stats(
#'   a_model,
#'   type = "delta_funs",
#'   minuends = "incomp",
#'   subtrahends = "comp"
#' )
#' plot(deltas)
#'
#' # Example 3: Observed data only -------------------------------------------
#' deltas <- calc_stats(
#'   dmc_synth_data,
#'   type = "delta_funs",
#'   minuends = "incomp",
#'   subtrahends = "comp"
#' )
#' plot(deltas)
#'
#' # Example 4: Observed data with intervals ---------------------------------
#' deltas <- calc_stats(
#'   dmc_synth_data,
#'   type = "delta_funs",
#'   minuends = "incomp",
#'   subtrahends = "comp",
#'   resample = TRUE
#' )
#' plot(deltas)
#'
#' @export
plot.delta_funs <- function(
  x,
  ...,
  id = NULL,
  conds = NULL,
  dv = NULL,
  col = NULL,
  xlim = NULL,
  ylim = NULL,
  xlab = "RT [s]",
  ylab = expression(Delta),
  interval_obs = TRUE,
  interval_pred = TRUE
) {
  delta_fun <- x

  # Handle multiple IDs
  if (!is.null(delta_fun$ID) && length(unique(delta_fun$ID)) > 1) {
    if (!is.null(id)) {
      if (!all(id %in% unique(delta_fun$ID))) {
        mis_ids <- setdiff(id, unique(delta_fun$ID))
        stop(
          "The following IDs were not found: ",
          paste(mis_ids, collapse = ", ")
        )
      }

      if (length(id) == 1) {
        delta_fun <- delta_fun[delta_fun$ID == id, ]
      } else {
        lapply(id, function(one_id) {
          plot.delta_funs(
            x = x,
            ...,
            id = one_id,
            conds = conds,
            dv = dv,
            col = col,
            xlim = xlim,
            ylim = ylim,
            xlab = xlab,
            ylab = ylab,
            interval_obs = interval_obs,
            interval_pred = interval_pred
          )
        })
        return(invisible())
      }
    } else {
      message("Aggregating across ID")
      delta_fun <- aggregate_stats(stat_df = delta_fun)
    }
  }

  # Get the delta columns to plot
  all_delta_columns <- grep("^Delta_", names(delta_fun), value = TRUE)
  if (is.null(dv)) {
    dv <- all_delta_columns
  }
  dv <- match.arg(dv, choices = all_delta_columns, several.ok = TRUE)

  # Determine x-variable names
  uv <- gsub("^Delta_", "Avg_", dv)

  # Set axis limits
  all_y_vals <- unlist(delta_fun[dv])
  y_r <- range(all_y_vals)
  y_r <- c(y_r[1] - diff(y_r) / 2, y_r[2] + diff(y_r) / 2)
  ylim <- ylim %||% y_r

  all_x_vals <- unlist(delta_fun[uv])
  xlim <- xlim %||% c(min(all_x_vals) * 0.75, max(all_x_vals) * 1.25)

  # Set colors
  if (length(all_delta_columns) == 1) {
    col <- "black"
  } else {
    col <- set_default_colors(colors = col, n = length(all_delta_columns))
    col <- col[which(all_delta_columns %in% dv)]
  }

  # Collect graphical options
  dots <- list(...)
  dots <- set_default_arguments(dots, dv, id)
  if (is.null(dots$err.col)) {
    dots$err.col <- "gray"
  }
  dots$err.col <- set_default_colors(
    colors = dots$err.col,
    n = length(all_delta_columns)
  )
  dots$err.col <- dots$err.col[which(all_delta_columns %in% dv)]

  # Prepare plot
  withr::local_par(family = dots$family)
  plot(
    c(1, 2) ~ c(1, 1),
    type = "n",
    xlab = xlab,
    ylab = ylab,
    xlim = xlim,
    ylim = ylim,
    cex.axis = dots$cex.axis,
    cex.lab = dots$cex.lab,
    cex.main = dots$cex.main,
    main = dots$main
  )

  # Determine original vs interval data
  if ("Estimate" %in% names(delta_fun)) {
    orig <- delta_fun[delta_fun$Estimate == "orig", ]
    l_u <- delta_fun[delta_fun$Estimate != "orig", ]
  } else {
    orig <- delta_fun
    l_u <- NULL
  }

  # Plot observed
  for (idx in seq_along(dv)) {
    sub_obs_orig <- orig[orig$Source == "obs", c(dv[idx], uv[idx])]
    sub_obs_lu <- l_u[l_u$Source == "obs", ]

    if (nrow(sub_obs_orig) == 0) {
      next # if there is no observed data skip this part
    }

    if (!is.null(sub_obs_lu) && nrow(sub_obs_lu) > 0 && interval_obs) {
      borders <- sort(unique(sub_obs_lu$Estimate))
      stopifnot(length(borders) == 2)
      df_lower <-
        sub_obs_lu[sub_obs_lu$Estimate == borders[1], c(dv[idx], uv[idx])]
      df_upper <-
        sub_obs_lu[sub_obs_lu$Estimate == borders[2], c(dv[idx], uv[idx])]

      err_x0 <- df_lower[[uv[idx]]]
      err_x1 <- df_upper[[uv[idx]]]
      err_y <- sub_obs_orig[[dv[idx]]]
      keep <- abs(err_x1 - err_x0) > diff(ylim) * dots$err.eps

      graphics::arrows(
        y0 = err_y[keep],
        x0 = err_x0[keep],
        y1 = err_y[keep],
        x1 = err_x1[keep],
        angle = 90,
        code = 3,
        length = dots$err.width,
        col = dots$err.col[idx]
      )

      err_y0 <- df_lower[[dv[idx]]]
      err_y1 <- df_upper[[dv[idx]]]
      err_x <- sub_obs_orig[[uv[idx]]]
      keep <- abs(err_y1 - err_y0) > diff(ylim) * dots$err.eps

      graphics::arrows(
        y0 = err_y0[keep],
        x0 = err_x[keep],
        y1 = err_y1[keep],
        x1 = err_x[keep],
        angle = 90,
        code = 3,
        length = dots$err.width,
        col = dots$err.col[idx]
      )
    }
    graphics::points(
      sub_obs_orig[[1]] ~ sub_obs_orig[[2]],
      col = col[idx],
      pch = dots$obs.pch,
      bg = dots$obs.pt.bg,
      cex = dots$cex.pt
    )
  }

  # Plot predicted
  for (idx in seq_along(dv)) {
    sub_pred_orig <- orig[orig$Source == "pred", c(dv[idx], uv[idx])]
    sub_pred_lu <- l_u[l_u$Source == "pred", ]

    if (nrow(sub_pred_orig) == 0) {
      next # if there is no observed data skip this part
    }

    if (!is.null(sub_pred_lu) && nrow(sub_pred_lu) > 0 && interval_pred) {
      borders <- sort(unique(sub_pred_lu$Estimate))
      stopifnot(length(borders) == 2)
      df_lower <-
        sub_pred_lu[sub_pred_lu$Estimate == borders[1], c(dv[idx], uv[idx])]
      df_upper <-
        sub_pred_lu[sub_pred_lu$Estimate == borders[2], c(dv[idx], uv[idx])]

      uv_lower = df_lower[[uv[idx]]]
      dv_lower = df_lower[[dv[idx]]]
      uv_upper = df_upper[[uv[idx]]]
      dv_upper = df_upper[[dv[idx]]]
      uv_orig = sub_pred_orig[[uv[idx]]]
      dv_orig = sub_pred_orig[[dv[idx]]]
      n = length(uv_lower)

      x_poly <- c(uv_lower[1], uv_orig, uv_upper[n], rev(uv_orig))
      y_poly <- c(dv_orig[1], dv_lower, dv_orig[n], rev(dv_upper))

      graphics::polygon(
        x = x_poly,
        y = y_poly,
        col = grDevices::adjustcolor(col[idx], alpha.f = dots$alpha),
        border = NA
      )
    }

    graphics::points(
      sub_pred_orig[[1]] ~ sub_pred_orig[[2]],
      type = "l",
      col = col[idx],
      lty = dots$pred.lty,
      lwd = dots$pred.lwd
    )
  }

  # Legend
  dots$pred.lty <- if (!any(delta_fun$Source == "pred")) NULL else dots$pred.lty
  dots$obs.pch <- if (!any(delta_fun$Source == "obs")) NULL else dots$obs.pch

  if (is.character(dots$legend.pos)) {
    x_leg <- dots$legend.pos
    y_leg <- NULL
  } else {
    x_leg <- dots$legend.pos[1]
    y_leg <- dots$legend.pos[2]
  }

  legend_labels <- gsub("^Delta_", "", dots$legend)
  if (all(!is.na(legend_labels)) && length(legend_labels) >= 1) {
    graphics::legend(
      x = x_leg,
      y = y_leg,
      legend = legend_labels,
      col = col,
      lwd = dots$pred.lwd,
      lty = dots$pred.lty,
      pch = dots$obs.pch,
      pt.bg = dots$obs.pt.bg,
      pt.cex = dots$cex.pt,
      bg = dots$legend.bg,
      bty = dots$bty,
      box.lwd = dots$box.lwd,
      box.col = dots$box.col,
      box.lty = dots$box.lty,
      cex = dots$cex.legend
    )
  }

  invisible()
}


#' Plot Distributions of Predicted and Observed Data
#'
#' Visualizes observed and/or predicted response time distributions. Useful for
#' assessing model fit or exploring model behavior.
#'
#' @param x an object of `type = "densities"`, typically returned by
#'   [dRiftDM::calc_stats()].
#' @inheritParams plot.cafs
#' @param obs_stats a character vector specifying which observed statistics to
#'   plot. Options include `"hist"` for histograms and `"kde"` for kernel
#'   density estimates. Defaults to `"hist"`.
#'
#' @details
#' If `x` contains multiple `ID`s and no specific `id` is provided, the function
#' aggregates across participants before plotting. You can provide a vector of
#' `id`s to produce separate plots for each participant.
#'
#' Observed densities are shown as histograms (default: gray shaded areas), or
#' KDE lines (default: black, dotted). Predicted densities are shown as lines
#' (default: colorized). Distributions associated with the upper boundary are
#' shown with values > 0 (i.e., the upper part of the plot), distributions
#' associated with the lower boundary are shown with values < 0 (i.e., the lower
#' part of the plot).
#'
#' Axis limits, colors, and styling options can be customized via `...`. If
#' interval information is provided (i.e., the column `Estimate` exists in `x`),
#' error bars or shading will be added, depending on the type of
#' statistic.
#'
#' A legend is only displayed if there is predicted data.
#'
#' @returns
#' Returns `NULL` invisibly. The function is called for its side effect of
#' generating a plot.
#'
#' @examples
#' # Example 1: Predicted densities only -------------------------------------
#' a_model <- dmc_dm()
#' dens <- calc_stats(a_model, type = "densities")
#' plot(dens, xlim = c(0, 1))
#' plot(dens, xlim = c(0, 1), conds = "comp")
#'
#'
#' # Example 2: Observed and predicted densities -----------------------------
#' obs_data(a_model) <- dmc_synth_data
#' dens <- calc_stats(a_model, type = "densities")
#' plot(dens, xlim = c(0, 1), conds = "comp", col = "blue")
#'
#' # Example 3: Observed densities only --------------------------------------
#' dens <- calc_stats(dmc_synth_data, type = "densities")
#' plot(dens, conds = "comp", obs.hist.col = "green", alpha = 1)
#'
#' # Example 4: With interval estimates --------------------------------------
#' dens <- calc_stats(dmc_synth_data, type = "densities", resample = TRUE)
#' plot(dens, interval_obs = TRUE, conds = "comp")
#'
#' @export
plot.densities <- function(
  x,
  ...,
  id = NULL,
  conds = NULL,
  col = NULL,
  xlim = NULL,
  ylim = NULL,
  xlab = "RT [s]",
  ylab = "Density",
  obs_stats = "hist",
  interval_obs = FALSE,
  interval_pred = TRUE
) {
  densities <- x

  # Handle multiple IDs
  if (!is.null(densities$ID) && length(unique(densities$ID)) > 1) {
    if (!is.null(id)) {
      if (!all(id %in% unique(densities$ID))) {
        mis_ids <- setdiff(id, unique(densities$ID))
        stop(
          "The following IDs were not found: ",
          paste(mis_ids, collapse = ", ")
        )
      }

      if (length(id) == 1) {
        densities <- densities[densities$ID == id, ]
      } else {
        lapply(id, function(one_id) {
          plot.densities(
            x = x,
            ...,
            id = one_id,
            conds = conds,
            col = col,
            xlim = xlim,
            ylim = ylim,
            xlab = xlab,
            ylab = ylab,
            obs_stats = obs_stats,
            interval_obs = interval_obs,
            interval_pred = interval_pred
          )
        })
        return(invisible())
      }
    } else {
      message("Aggregating across ID")
      densities <- aggregate_stats(stat_df = densities)
    }
  }

  # Determine conditions
  all_conds = unique(densities$Cond)
  if (is.null(conds)) {
    conds <- all_conds
  }
  conds <- match.arg(arg = conds, choices = all_conds, several.ok = TRUE)

  # Get the columns to plot
  dv <- grep("^Dens_", names(densities), value = TRUE)
  stopifnot(length(dv) == 2)

  # Set axis limits
  max_dens <- sort(sapply(densities[dv], max, na.rm = TRUE))
  y_r <- c(
    -max_dens[1] - diff(max_dens) / 20,
    max_dens[2] + diff(max_dens) / 20
  )
  ylim <- ylim %||% y_r

  xlim <- xlim %||% range(densities$Time)

  # Set colors
  col <- set_default_colors(colors = col, n = length(all_conds))
  col <- col[which(all_conds %in% conds)]

  # Collect graphical options
  dots <- list(...)
  dots <- set_default_arguments(dots, conds, id)

  # collect additional default coloring
  if (is.null(dots$err.col)) {
    dots$err.col <- "black"
  }
  dots$err.col <- set_default_colors(
    colors = dots$err.col,
    n = length(all_conds)
  )
  dots$err.col <- dots$err.col[which(all_conds %in% conds)]

  dots$obs.hist.col <- set_default_colors(
    colors = dots$obs.hist.col,
    n = length(all_conds)
  )
  dots$obs.hist.col <- dots$obs.hist.col[which(all_conds %in% conds)]
  dots$obs.kde.col <- set_default_colors(
    colors = dots$obs.kde.col,
    n = length(all_conds)
  )
  dots$obs.kde.col <- dots$obs.kde.col[which(all_conds %in% conds)]

  # Prepare plot
  withr::local_par(family = dots$family)
  plot(
    c(1, 2) ~ c(1, 1),
    type = "n",
    xlab = xlab,
    ylab = ylab,
    xlim = xlim,
    ylim = ylim,
    cex.axis = dots$cex.axis,
    cex.lab = dots$cex.lab,
    cex.main = dots$cex.main,
    main = dots$main
  )
  graphics::abline(h = 0, col = dots$horiz.col, lwd = dots$horiz.lwd)

  # Determine original vs interval data
  if ("Estimate" %in% names(densities)) {
    orig <- densities[densities$Estimate == "orig", ]
    l_u <- densities[densities$Estimate != "orig", ]
  } else {
    orig <- densities
    l_u <- NULL
  }

  # Plot observed
  for (idx in seq_along(conds)) {
    sub_obs_orig <- orig[orig$Source == "obs" & orig$Cond == conds[idx], ]
    sub_obs_lu <- l_u[l_u$Source == "obs" & l_u$Cond == conds[idx], ]

    if (nrow(sub_obs_orig) == 0) {
      next # if there is no observed data skip this part
    }

    for (one_stat in obs_stats) {
      for (i in seq_along(dv)) {
        sign = if (i == 1) 1 else -1

        if (!is.null(sub_obs_lu) && nrow(sub_obs_lu) > 0 && interval_obs) {
          borders <- sort(unique(sub_obs_lu$Estimate))
          stopifnot(length(borders) == 2)

          df_lower <- sub_obs_lu[
            sub_obs_lu$Estimate == borders[1] &
              sub_obs_lu$Stat == one_stat,
          ]
          df_upper <- sub_obs_lu[
            sub_obs_lu$Estimate == borders[2] &
              sub_obs_lu$Stat == one_stat,
          ]

          err_y0 <- sign * df_lower[[dv[i]]]
          err_y1 <- sign * df_upper[[dv[i]]]
          err_x <- df_lower[["Time"]]
          keep <- abs(err_y1 - err_y0) > diff(ylim) * dots$err.eps

          graphics::arrows(
            y0 = err_y0[keep],
            x0 = err_x[keep],
            y1 = err_y1[keep],
            x1 = err_x[keep],
            angle = 90,
            code = 3,
            length = dots$err.width,
            col = dots$err.col[idx]
          )
        }

        df_orig <- sub_obs_orig[sub_obs_orig$Stat == one_stat, ]
        y_orig = sign * df_orig[[dv[i]]]
        x_orig = df_orig[["Time"]]
        if (one_stat == "hist") {
          x_poly <- c(x_orig, rev(x_orig))
          y_poly <- c(y_orig, numeric(nrow(df_orig)))

          graphics::polygon(
            x = x_poly,
            y = y_poly,
            col = grDevices::adjustcolor(
              dots$obs.hist.col[idx],
              alpha.f = dots$alpha
            ),
            border = NA
          )
        } else if (one_stat == "kde") {
          graphics::points(
            y_orig ~ x_orig,
            col = dots$obs.kde.col[idx],
            type = "l",
            lty = dots$obs.kde.lty,
            lwd = dots$obs.kde.lwd
          )
        }
      }
    }
  }

  # Plot predicted
  for (idx in seq_along(conds)) {
    sub_pred_orig <- orig[orig$Source == "pred" & orig$Cond == conds[idx], ]
    sub_pred_lu <- l_u[l_u$Source == "pred" & l_u$Cond == conds[idx], ]

    if (nrow(sub_pred_orig) == 0) {
      next # if there is no observed data skip this part
    }

    for (i in seq_along(dv)) {
      sign = if (i == 1) 1 else -1
      if (!is.null(sub_pred_lu) && nrow(sub_pred_lu) > 0 && interval_pred) {
        borders <- sort(unique(sub_pred_lu$Estimate))
        stopifnot(length(borders) == 2)

        df_lower <- sub_pred_lu[
          sub_pred_lu$Estimate == borders[1] &
            sub_pred_lu$Stat == "kde",
        ]
        df_upper <- sub_pred_lu[
          sub_pred_lu$Estimate == borders[2] &
            sub_pred_lu$Stat == "kde",
        ]

        err_y0 <- sign * df_lower[[dv[i]]]
        err_y1 <- sign * df_upper[[dv[i]]]
        err_x <- df_lower[["Time"]]

        x_poly <- c(err_x, rev(err_x))
        y_poly <- c(err_y0, rev(err_y1))

        graphics::polygon(
          x = x_poly,
          y = y_poly,
          col = grDevices::adjustcolor(col[idx], alpha.f = dots$alpha),
          border = NA
        )
      }
      df_orig <- sub_pred_orig[sub_pred_orig$Stat == "pdf", ]
      y_orig = sign * df_orig[[dv[i]]]
      x_orig = df_orig[["Time"]]
      graphics::points(
        y_orig ~ x_orig,
        col = col[idx],
        type = "l",
        lty = dots$pred.lty,
        lwd = dots$pred.lwd
      )
    }
  }

  # Legend (condition)
  dots$pred.lty <- if (!any(densities$Source == "pred")) NULL else dots$pred.lty

  if (is.character(dots$legend.pos)) {
    x_leg <- dots$legend.pos
    y_leg <- NULL
  } else {
    x_leg <- dots$legend.pos[1]
    y_leg <- dots$legend.pos[2]
  }

  if (
    !is.null(dots$pred.lty) && !anyNA(dots$legend) && length(dots$legend) >= 1
  ) {
    graphics::legend(
      x = x_leg,
      y = y_leg,
      legend = dots$legend,
      col = col,
      lwd = dots$pred.lwd,
      lty = dots$pred.lty,
      bg = dots$legend.bg,
      bty = dots$bty,
      box.lwd = dots$box.lwd,
      box.col = dots$box.col,
      box.lty = dots$box.lty,
      cex = dots$cex.legend
    )
  }

  # Legend (lines, if obs_data is represented as a line and both
  # predicted and observed densities are present)
  if (is.character(dots$lines.legend.pos)) {
    x_leg <- dots$lines.legend.pos
    y_leg <- NULL
  } else {
    x_leg <- dots$lines.legend.pos[1]
    y_leg <- dots$lines.legend.pos[2]
  }

  if (all(c("pred", "obs") %in% unique(densities$Source))) {
    dots[["lines.legend"]] = dots[["lines.legend"]] %||% c("pred", "obs")
    if ("kde" %in% obs_stats && !all(is.na(dots[["lines.legend"]]))) {
      graphics::legend(
        x = x_leg,
        y = y_leg,
        legend = dots$lines.legend,
        col = "black",
        lwd = c(dots$pred.lwd, dots$obs.kde.lwd),
        lty = c(dots$pred.lty, dots$obs.kde.lty),
        bg = dots$legend.bg,
        bty = dots$bty,
        box.lwd = dots$box.lwd,
        box.col = dots$box.col,
        box.lty = dots$box.lty,
        cex = dots$cex.legend
      )
    }
  }

  invisible()
}


#' Plot Multiple Statistics
#'
#' @description
#' This function iterates over a list of statistics data, resulting from a call
#' to [dRiftDM::calc_stats()], and subsequently plots each statistic. It allows
#' for a simple arrangement of multiple plots on a single graphics device.
#'
#'
#' @param x an object of type `stats_dm_list`, which is essentially a list
#'  of multiple statistics, resulting from a call to [dRiftDM::calc_stats()].
#' @param mfrow an optional numeric vector of length 2, specifying the number of
#'  rows and columns for arranging multiple panels in a single plot
#'  (e.g., `c(1, 3)`). Plots are provided sequentially if `NULL` (default),
#'  using the current graphics layout of a user.
#' @param ... additional arguments passed to the [plot] function for each
#'  individual `stats_dm` object in `x`.
#'
#' @details
#' The `plot.stats_dm_list()` function "merely" iterates over each entry of `x`
#' and calls the respective `plot()` method. If `dRiftDM` doesn't provide a
#' `plot()` method for an object stored in `x`, the respective entry is
#' skipped and a message is displayed.
#'
#' When users want more control over each plot, it is best to call the
#' `plot()` function separately for each statistic in the list
#' (e.g., `plot(x$cafs)`; `plot(x$quantiles)`)
#'
#'
#' @returns
#' Nothing (`NULL`; invisibly)
#'
#' @examples
#' # get a list of statistics for demonstration purpose
#' all_fits <- get_example_fits("fits_ids_dm")
#' stats <- calc_stats(all_fits, type = c("cafs", "quantiles"))
#'
#' # then call the plot function.
#' plot(stats, mfrow = c(1, 2))
#'
#' @seealso [dRiftDM::plot.cafs()], [dRiftDM::plot.quantiles()],
#' [dRiftDM::plot.delta_funs()], [dRiftDM::plot.densities()]
#'
#' @export
plot.stats_dm_list <- function(x, ..., mfrow = NULL) {
  if (!is.null(mfrow)) {
    withr::local_par(mfrow = mfrow)
  }

  for (obj in x) {
    class_obj <- class(obj)[1]
    if (!(class_obj %in% drift_dm_stats_types("sum_dist"))) {
      message(
        "dRiftDM doesn't provide a plot method for objects of type ",
        class_obj,
        ", skipping this entry."
      )
      next
    }
    plot(obj, ...)
  }
  invisible()
}


# HISTOGRAM of Parameters -------------------------------------------------

#' Plot Parameter Distribution(s)
#'
#' This function creates a histogram for each parameter in a `coefs_dm` object,
#' resulting from a call to [dRiftDM::coef.fits_ids_dm].
#'
#' @param x an object of class `coefs_dm` (see [dRiftDM::coef.fits_ids_dm])
#' @param bundle_plots logical, indicating whether to display separate panels
#'  in a single plot layout (`FALSE`), or to plot them separately (`TRUE`).
#' @param col character vector, specifying colors for each condition, if
#'  conditions are present.
#' @param ... additional graphical arguments passed to [graphics::hist()].
#' Not supported are the `plot` and `probability` arguments
#' (the latter can be controlled via the supported `freq` argument).
#' For further plotting arguments, see also [dRiftDM::set_default_arguments()].
#' @inheritParams plot.cafs
#'
#' @details
#' The `hist.coefs_dm` function is designed for visualizing parameter
#' distributions.
#'
#' If multiple conditions are present, it overlays histograms for each condition
#' with adjustable transparency.
#'
#' When `bundle_plots` is set to `TRUE`, histograms for each parameter are
#' displayed in a grid layout within a single graphics device.
#'
#' This function has some customization options, but they are limited. If you
#' want to have a highly customized histogram, it is best to create it on your
#' own using R's [graphics::hist()] function (see the examples below).
#'
#' @returns
#' Nothing (`NULL`; invisibly)
#'
#' @examples
#' # get an auxiliary fit procedure result (see the function load_fits_ids)
#' all_fits <- get_example_fits("fits_ids")
#' coefs <- coef(all_fits)
#' print(coefs)
#' hist(coefs, bundle_plots = FALSE) # calls hist.coefs_dm method of dRiftDM
#'
#' # how to fall back to R's hist() function for heavy customization
#' coefs <- unpack_obj(coefs) # provides the plain data.frame
#' hist(coefs$muc, main = expression(mu[c])) # calls graphics::hist()
#'
#' @export
hist.coefs_dm <- function(
  x,
  ...,
  conds = NULL,
  col = NULL,
  xlim = NULL,
  ylim = NULL,
  xlab = "value",
  ylab = NULL,
  bundle_plots = TRUE
) {
  coefs_obj <- x
  dots <- list(...)

  # ensure backward compatibility (delete in the future)
  if (!is.null(dots$colors)) {
    col = dots$colors
    lifecycle::deprecate_warn(
      "0.3.0",
      "dRiftDM::hist.coefs_dm(colors = )",
      "dRiftDM::hist.coefs_dm(col = )"
    )
  }
  if (!is.null(dots$separate_plots)) {
    bundle_plots = dots$separate_plots
    lifecycle::deprecate_warn(
      "0.3.0",
      "dRiftDM::hist.coefs_dm(separate_plots = )",
      "dRiftDM::hist.coefs_dm(bundle_plots = )"
    )
  }

  # get the parameter and condition names (if existant)
  prm_names <- setdiff(colnames(coefs_obj), c("ID", "Cond"))
  conds_present <- "Cond" %in% colnames(coefs_obj)

  # all_conds/conds will be NULL if no Cond column is present
  all_conds <- unique(coefs_obj$Cond)
  if (is.null(conds)) {
    conds <- all_conds
  }
  conds <- match.arg(arg = conds, choices = all_conds, several.ok = TRUE)

  # create colors
  if (conds_present) {
    # Set colors
    col <- set_default_colors(colors = col, n = length(all_conds))
    col <- col[which(all_conds %in% conds)]
  } else {
    col <- set_default_colors(col, n = 1)
  }

  # figure out the plot outline if separate panels shall be plotted in one plot
  n_plots <- length(prm_names)
  n_rows <- ceiling(sqrt(n_plots))
  n_cols <- ceiling(n_plots / n_rows)
  if (bundle_plots) {
    withr::local_par(mfrow = c(n_rows, n_cols))
  }

  # figure out the arguments
  dots <- set_default_arguments(dots, leg = conds, id = NULL)
  if (is.null(dots$breaks)) {
    dots$breaks = "Sturges"
  }
  if (is.null(dots$include.lowest)) {
    dots$include.lowest = TRUE
  }
  if (is.null(dots$right)) {
    dots$right = TRUE
  }
  if (is.null(dots$fuzz)) {
    dots$fuzz = 1e-7
  }
  if (is.null(dots$angle)) {
    dots$angle = 45
  }
  if (is.null(dots$axes)) {
    dots$axes = TRUE
  }
  if (is.null(dots$labels)) {
    dots$labels = FALSE
  }
  if (is.null(dots$ann)) {
    dots$ann = TRUE
  }

  # figure out the main titles
  if (is.null(dots$main)) {
    main <- prm_names
  } else {
    main <- rep(dots$main, length.out = length(prm_names))
  }

  # temporary plot function to reuse
  my_hist <- function(x_vals, col, main, add = FALSE, xlim = NULL) {
    hist_vals <- graphics::hist(
      x_vals,
      breaks = dots$breaks,
      include.lowest = dots$include.lowest,
      right = dots$right,
      fuzz = dots$fuzz,
      plot = FALSE
    )
    if (is.null(dots$freq)) {
      freq = hist_vals$equidist
    }
    if (is.null(ylab)) {
      if (isTRUE(freq)) {
        ylab = "Frequency"
      } else {
        ylab = "Density"
      }
    }
    if (is.null(xlim)) {
      xlim <- range(hist_vals$breaks)
    }

    col = grDevices::adjustcolor(col, alpha.f = dots$alpha)
    plot(
      hist_vals,
      freq = freq,
      density = dots$density,
      angle = dots$angle,
      col = col,
      border = dots$border,
      lty = dots$lty,
      main = main,
      sub = dots$sub,
      xlab = xlab,
      ylab = ylab,
      xlim = xlim,
      ylim = ylim,
      axes = dots$axes,
      labels = dots$labels,
      ann = dots$ann,
      cex.axis = dots$cex.axis,
      cex.lab = dots$cex.lab,
      cex.main = dots$cex.main,
      family = dots$family,
      add = add
    )
    invisible()
  }

  # iterate through all paramters
  for (prm_idx in seq_along(prm_names)) {
    # if no condition present, then just call hist
    if (!conds_present) {
      my_hist(
        x_vals = coefs_obj[[prm_names[prm_idx]]],
        col = col,
        main = main[prm_idx]
      )
      next()
    }

    # if conditions are present, then iterate through the conditions ...
    x_r = sapply(conds, \(x) {
      sub <- coefs_obj[[prm_names[prm_idx]]][coefs_obj$Cond == x]
      hist_vals <- graphics::hist(
        sub,
        breaks = dots$breaks,
        include.lowest = dots$include.lowest,
        right = dots$right,
        fuzz = dots$fuzz,
        plot = FALSE
      )
      return(range(hist_vals$breaks))
    })
    if (is.null(xlim)) {
      x_r = range(x_r)
    } else {
      x_r = xlim
    }

    for (cond_idx in seq_along(conds)) {
      # get subset for one prm and one conditon
      sub <- coefs_obj[[prm_names[prm_idx]]][coefs_obj$Cond == conds[cond_idx]]

      # plot or add histogram
      my_hist(
        x_vals = sub,
        col = col[cond_idx],
        main = main[prm_idx],
        add = cond_idx != 1,
        xlim = x_r
      )
    }
  }
  invisible()
}


#' Plot Components of a Drift Diffusion Model
#'
#' @description
#' This function generates plots for all components of a drift diffusion model
#' (DDM), such as drift rate, boundary, and starting condition. Each component
#' is plotted against the time or evidence space, allowing for visual inspection
#' of the model's behavior across different conditions.
#'
#' @param x an object of class [dRiftDM::drift_dm]
#' @inheritParams plot.cafs
#' @inheritParams hist.coefs_dm
#'
#' @details
#' The `plot.drift_dm` function provides an overview of key DDM components,
#' which include:
#' - `mu_fun`: Drift rate over time.
#' - `mu_int_fun`: Integrated drift rate over time (if required by the
#'   specified `solver` of the model).
#' - `x_fun`: Starting condition as a density across evidence values.
#' - `b_fun`: Boundary values over time.
#' - `dt_b_fun`: Derivative of the boundary function over time.
#' - `nt_fun`: Non-decision time as a density over time.
#'
#'
#' @returns
#' Nothing (`NULL`; invisibly)
#'
#' @examples
#' # plot the component functions of the Ratcliff DDM
#' plot(ratcliff_dm())
#' plot(ratcliff_dm(var_non_dec = TRUE))
#' # Note: the variability in the drift rate for the Ratcliff DDM
#' # is not plotted! This is because it is not actually stored as a component
#' # function.
#'
#' # plot the component functions of the DMC model
#' plot(dmc_dm(), col = c("green", "red"))
#'
#' @export
plot.drift_dm <- function(
  x,
  ...,
  conds = NULL,
  col = NULL,
  xlim = NULL,
  bundle_plots = TRUE
) {
  drift_dm_obj <- x
  dots <- list(...)

  # ensure backward compatibility (delete in the future)
  if (!is.null(dots$legend_pos)) {
    dots$legend.pos = dots$legend_pos
    lifecycle::deprecate_warn(
      "0.3.0",
      "dRiftDM::plot.drift_dm(legend_pos = )",
      details = "Please use the optional `legend.pos` argument instead"
    )
  }

  # get conditions
  all_conds = conds(drift_dm_obj)
  if (is.null(conds)) {
    conds <- all_conds
  }
  conds <- match.arg(arg = conds, choices = all_conds, several.ok = TRUE)

  # get default parameters
  dots <- set_default_arguments(dots, conds, NULL)

  col <- set_default_colors(colors = col, n = length(all_conds))
  col <- col[which(all_conds %in% conds)]

  t_max <- drift_dm_obj$prms_solve[["t_max"]]
  if (is.null(xlim)) {
    xlim <- c(0, t_max / 4)
  }

  # get the time and evidence space
  nx <- drift_dm_obj$prms_solve[["nx"]]
  nt <- drift_dm_obj$prms_solve[["nt"]]
  dx <- drift_dm_obj$prms_solve[["dx"]]
  dt <- drift_dm_obj$prms_solve[["dt"]]

  x_vec <- seq(-1, 1, length.out = nx + 1)
  t_vec <- seq(0, t_max, length.out = nt + 1)

  # get all components
  all_vals <- comp_vals(drift_dm_obj)
  mu_vals <- lapply(all_vals, \(x) x$mu_vals)
  mu_int_vals <- lapply(all_vals, \(x) x$mu_int_vals)
  x_vals <- lapply(all_vals, \(x) x$x_vals)
  b_vals <- lapply(all_vals, \(x) x$b_vals)
  dt_b_vals <- lapply(all_vals, \(x) x$dt_b_vals)
  nt_vals <- lapply(all_vals, \(x) x$nt_vals)

  # some temp functions for easier data handling
  temp_is_not_null <- function(x) {
    return(!all(sapply(x, is.null)))
  }

  range_vals <- function(x, reduce_t = FALSE, select_indices_t = NULL) {
    as_arr <- sapply(x, \(y) {
      if (reduce_t) {
        return(range(y[select_indices_t]))
      }
      return(range(y))
    })
    return(c(min(as_arr), max(as_arr)))
  }

  plot_empty <- function(main, ylab, xlab, xlim, ylim) {
    plot(
      c(1, 2) ~ c(1, 1),
      col = "white",
      xlim = xlim,
      ylab = ylab,
      xlab = xlab,
      ylim = ylim,
      main = main,
      cex.axis = dots$cex.axis,
      cex.lab = dots$cex.lab,
      cex.main = dots$cex.main,
      family = dots$family
    )
  }
  temp_points <- function(formula, col) {
    graphics::points(
      formula,
      ty = "l",
      col = col,
      lty = dots$pred.lty,
      lwd = dots$pred.lwd
    )
  }

  # plot everything
  if (bundle_plots) {
    withr::local_par(mfrow = c(3, 2))
  }
  # get the relevant time steps (for y-axis scaling)
  select_indices_t <- which(t_vec >= xlim[1] & t_vec <= xlim[2])

  # plot the drift rate
  if (temp_is_not_null(mu_vals)) {
    ylim <- range_vals(mu_vals, reduce_t = TRUE, select_indices_t)
    plot_empty("mu_fun", "Drift Rate", "Time [s]", xlim, ylim)
    for (i in seq_along(conds)) {
      temp_points(mu_vals[[conds[i]]] ~ t_vec, col[i])
    }
  }

  # plot the integral of the drift rate
  if (temp_is_not_null(mu_int_vals)) {
    ylim <- range_vals(mu_int_vals, reduce_t = TRUE, select_indices_t)
    plot_empty("mu_int_fun", "Drift", "Time [s]", xlim, ylim)

    for (i in seq_along(conds)) {
      temp_points(mu_int_vals[[conds[i]]] ~ t_vec, col[i])
    }
  }

  # plot the starting condition
  if (temp_is_not_null(x_vals)) {
    ylim <- range_vals(x_vals)
    plot_empty("x_fun", "Density", "Evidence Value", c(-1, 1), ylim)

    for (i in seq_along(conds)) {
      temp_points(x_vals[[conds[i]]] ~ x_vec, col[i])
    }
  }

  # plot the boundary
  if (temp_is_not_null(b_vals)) {
    ylim <- range_vals(b_vals, reduce_t = TRUE, select_indices_t)
    plot_empty("b_fun", "Boundary", "Time [s]", xlim, ylim)

    for (i in seq_along(conds)) {
      temp_points(b_vals[[conds[i]]] ~ t_vec, col[i])
    }
  }

  # plot the derivative of the boundary
  if (temp_is_not_null(dt_b_vals)) {
    ylim <- range_vals(dt_b_vals, reduce_t = TRUE, select_indices_t)
    plot_empty("dt_b_fun", "Derivative Boundary", "Time [s]", xlim, ylim)

    for (i in seq_along(conds)) {
      temp_points(dt_b_vals[[conds[i]]] ~ t_vec, col[i])
    }
  }

  # plot the non-decision time
  if (temp_is_not_null(nt_vals)) {
    ylim <- range_vals(nt_vals, reduce_t = TRUE, select_indices_t)
    plot_empty("nt_fun", "Density", "Time [s]", xlim, ylim)

    for (i in seq_along(conds)) {
      temp_points(nt_vals[[conds[i]]] ~ t_vec, col[i])
    }
  }

  # plot the legend
  if (is.character(dots$legend.pos)) {
    x_leg <- dots$legend.pos
    y_leg <- NULL
  } else {
    x_leg <- dots$legend.pos[1]
    y_leg <- dots$legend.pos[2]
  }

  if (all(!is.na(dots$legend)) && length(dots$legend) >= 1) {
    graphics::legend(
      x = x_leg,
      y = y_leg,
      legend = dots$legend,
      col = col,
      lwd = dots$pred.lwd,
      lty = dots$pred.lty,
      bg = dots$legend.bg,
      bty = dots$bty,
      box.lwd = dots$box.lwd,
      box.col = dots$box.col,
      cex = dots$cex.legend,
      box.lty = dots$box.lty
    )
  }
  invisible()
}


# PLOT MCMC RESULTS  --------------------------------------------

#' Plot MCMC Results and Diagnostics `mcmc_dm` Objects
#'
#' Visualize MCMC results and diagnostics for `mcmc_dm` objects.
#' The function `plot.mcmc()` is typically called when users supply an
#' objects returned by [dRiftDM::estimate_bayesian()] to the generic
#' [base::plot()] function.
#'
#' @param x an object of class `mcmc_dm`, as returned by
#' [dRiftDM::estimate_bayesian()].
#' @param ... optional arguments passed on to the underlying plotting functions
#'   [dRiftDM::plot_mcmc_trace()], [dRiftDM::plot_mcmc_marginal()], and
#'   [dRiftDM::plot_mcmc_auto()]. See the respective documentations for a list
#'   of optional arguments and the examples below. Probably the most relevant
#'   optional argument is `which_prms` that allows users to select a specific
#'   subset of parameters.
#' @param id optional character vector, specifying the id(s) of participants to
#'  plot. If `length(id) > 1`, `plot.mcmc_dm()` is called recursively,
#'  iterating over each entry in `id`. Each `id` must match with the relevant
#'  dimension names of the used chains array stored in `x`.
#' @param what a character string indicating the type of plot to produce. Must
#'   be either `"trace"`, `"density"`, or `"auto"`. See the Details below.
#'   Default is `"trace"`.
#' @inheritParams hist.coefs_dm
#'
#' @details
#' This function provides diagnostic and summary visualizations of MCMC samples.
#' It handles results from both hierarchical and non-hierarchical MCMC runs:
#'
#'  * If `id` is provided, the plot refers to the requested participant, with
#'    MCMC results extracted at the individual level.
#'  * If `id` is omitted, plots refer to group-level parameters (i.e., the
#'    hyperparameters)
#'
#' The following plot types are supported:
#'
#' * Trace plots (`what = "trace"`): These plots show sampled parameter values
#' across MCMC iterations for each
#' chain. They are primarily used to inspect convergence and mixing behavior.
#' Ideally, all chains should appear well-mixed (i.e., they should overlap and
#' sample in a similar range). Lack of convergence is indicated by chains that
#' remain in separate regions or exhibit trends over time.
#'
#' * Density plots (`what = "density"`):  These plots display smoothed marginal
#' posterior distributions for each
#' parameter, collapsed over chains and iterations. They are useful for
#' understanding the central tendency, variance, and shape of the posterior
#' distributions.
#'
#' * Autocorrelation plots (`what = "auto"`):  These plots display the
#' autocorrelation at different lags, averaged across chains.
#' They are useful to judge how quickly the chains produced independent samples.
#'
#' @return Returns `NULL` invisibly.
#'
#' @examples
#' # get an examplary `mcmc_dm` object
#' chains_obj <- get_example_fits("mcmc")
#' plot(chains_obj)
#' plot(chains_obj, what = "density")
#' plot(chains_obj, what = "density", which_prm = "b", bundle_plots = FALSE)
#'
#' @seealso [dRiftDM::plot_mcmc_trace()], [dRiftDM::plot_mcmc_marginal()],
#' [dRiftDM::plot_mcmc_auto()]
#' @export
plot.mcmc_dm <- function(
  x,
  ...,
  id = NULL,
  what = "trace",
  bundle_plots = TRUE
) {
  chains_obj <- x

  # input checks

  # ids only make sense in the hierarchical case
  hierarchical <- attr(chains_obj, "hierarchical")
  if (!hierarchical & !is.null(id)) {
    id = NULL
  }
  what = match.arg(what, choices = c("trace", "density", "auto"))

  # optional: use local mfrow
  if (bundle_plots) {
    withr::local_par(mfrow = c(3, 2))
  }

  # optional/additional arguments
  dots = list(...)

  # call recursively the plot function if id has multiple entries
  if (length(id) > 1) {
    lapply(id, \(one_id) {
      dots$id = one_id
      do.call(plot.mcmc_dm, c(list(x = x, what = what), dots))
    })
    return(invisible())
  }

  # get the chains to plot
  chains <- get_subset_chains(chains_obj, id = id)

  # now dispatch to the specific function
  if (what == "trace") {
    plot_mcmc_trace(chains = chains, ...)
  }
  if (what == "density") {
    plot_mcmc_marginal(chains = chains, ...)
  }
  if (what == "auto") {
    plot_mcmc_auto(chains = chains, ...)
  }

  invisible()
}


#' Plot MCMC Chains for Drift Diffusion Model Parameters
#'
#' The functions provide visualizations of MCMC results obtained for
#' [dRiftDM::drift_dm()] objects. Users won't call the functions directly.
#' Instead, they are called via [dRiftDM::plot.mcmc_dm()], and the
#' following documentation helps to define optional arguments that can be
#' passed on via  `...`.
#'
#'
#' @param chains an array of MCMC samples with three dimensions
#'   (parameters × chains × iterations; see also [dRiftDM::plot.mcmc_dm()]).
#'   This argument is not optional and will be provided by
#'   [dRiftDM::plot.mcmc_dm()].
#' @param col_palette a function to generate a color palette for chains
#'   (default is [grDevices::rainbow]). Must be callable like this
#'   `col_palette(n)`.
#' @param col_chains a character vector, defining colors(s) to override
#'   `col_palette` for chain lines. Can be a single value or a vector matching
#'   the number of chains. Recycled if necessary to match the number of chains.
#' @param col_line a character vector, defining color(s) for the line outlines
#'   in the density plots. Can be a single value or a vector matching the number
#'   of parameters to plot. Recycled if necessary to match the number of
#'   parameters being plotted.
#' @param col_shade a character vector, defining color(s) for the shaded
#'   areas in the density plots. Can be a single value or a vector matching
#'   the number of parameters to plot. Recycled if necessary to match the number
#'   of parameters being plotted.
#' @param which_prms a regular expression (string), used to select a subset
#'   of parameters to plot. For example, `"^v"` would match all parameters
#'   starting with `v`. See also the examples in [dRiftDM::plot.mcmc_dm()].
#' @param xlab,ylab character vector(s), specifying axis labels for each panel.
#'   Recycled if necessary to match the number of plots being generated.
#' @param xlim,ylim list(s), containing length-2 numeric vectors specifying
#'   x- and y-axis limits, respectively. Recycled if necessary to match the
#'   number of plots being generated.
#' @param lags a numeric vector, giving the lags at which to calculate
#'  autocorrelation (see also [coda::autocorr.diag()] and [coda::autocorr()]).
#' @param type a character, specifying how autocorrelations shall be displayed.
#' Defaults to "h" (see the `type` argument of [base::plot()]). Recycled if
#' necessary to match the number of parameters being plotted.
#' @param main character vector, specifying the labels above each
#' autocorrelation plot. Defaults to parameter labels. Recycled if
#' necessary to match the number of parameters being plotted.
#'
#' @return These functions are called for their side effects (producing plots).
#'   They return `NULL` invisibly.
#'
#' @details
#' - `plot_mcmc_trace()` plots one panel per parameter, with lines for each
#'   chain showing how values evolve over iterations. This is useful for
#'   diagnosing convergence and mixing.
#' - `plot_mcmc_marginal()` plots smoothed marginal posterior densities
#'   collapsed over chains and iterations for each parameter, useful for
#'   inspecting posterior distributions.
#' - `plot_mcmc_auto()` plots the autocorrelation among samples as function
#'   of `lags`.
#'
#'
#' @seealso [dRiftDM::plot.mcmc_dm()]
#' @keywords internal
plot_mcmc_trace = function(
  chains,
  col_palette = grDevices::rainbow,
  col_chains = NULL,
  which_prms = NULL,
  xlab = NULL,
  ylab = NULL,
  xlim = NULL,
  ylim = NULL
) {
  stopifnot(length(dim(chains)) == 3)

  # get the dimension and prm labels
  n_chains <- dim(chains)[2]
  iterations <- as.numeric(dimnames(chains)[[3]])

  all_prm_names <- dimnames(chains)[[1]]
  prms_to_plot <- all_prm_names
  if (!is.null(which_prms)) {
    prms_to_plot <- grep(pattern = which_prms, x = prms_to_plot, value = TRUE)
  }
  if (length(prms_to_plot) == 0) {
    warning(
      "The argument `which_prms` doesn't match with any parameter. ",
      "No plot is provided."
    )
    return(invisible())
  }

  # get the colors for the plot
  all_colors = col_palette(n_chains)
  if (!is.null(col_chains)) {
    stopifnot(is.character(col_chains))
    all_colors = rep(col_chains, length.out = n_chains)
  }

  # get default values for xlab etc.
  n_plot <- length(prms_to_plot)
  xlab <- rep(if (is.null(xlab)) "Iterations" else xlab, length.out = n_plot)
  ylab <- rep(if (is.null(ylab)) prms_to_plot else ylab, length.out = n_plot)
  xlim <- rep(if (is.null(xlim)) list(NA) else xlim, length.out = n_plot)
  ylim <- rep(if (is.null(ylim)) list(NA) else ylim, length.out = n_plot)

  # now iterate over every parameter and plot the traces
  for (i in seq_along(prms_to_plot)) {
    one_prm <- prms_to_plot[i]
    # which idx in the chains array
    prm_idx = which(one_prm == all_prm_names)
    sub_chains <- chains[prm_idx, , ]

    # prepare x and y limits
    ylim_one_prm = ylim[[i]]
    if (is.na(ylim_one_prm)) {
      range_sub <- range(sub_chains)
      padding <- 0.05 * diff(range_sub) # scale by 5%
      ylim_one_prm <- c(range_sub[1] - padding, range_sub[2] + padding)
    }
    xlim_one_prm = xlim[[i]]
    if (is.na(xlim_one_prm)) {
      xlim_one_prm = range(iterations)
    }

    # now the raw plot outline
    plot(
      NA,
      ylim = ylim_one_prm,
      xlab = xlab[i],
      ylab = ylab[i],
      xlim = xlim_one_prm
    )
    for (j in seq_len(n_chains)) {
      col <- all_colors[j]
      graphics::points(sub_chains[j, ] ~ iterations, ty = "l", col = col)
    }

    invisible()
  }
}


#' @rdname plot_mcmc_trace
plot_mcmc_marginal <- function(
  chains,
  col_line = NULL,
  col_shade = NULL,
  which_prms = NULL,
  xlab = NULL,
  ylab = NULL,
  xlim = NULL,
  ylim = NULL
) {
  stopifnot(length(dim(chains)) == 3)

  # get the prm labels
  all_prm_names <- dimnames(chains)[[1]]
  prms_to_plot <- all_prm_names
  if (!is.null(which_prms)) {
    prms_to_plot <- grep(pattern = which_prms, x = prms_to_plot, value = TRUE)
  }
  if (length(prms_to_plot) == 0) {
    warning(
      "The argument `which_prms` doesn't match with any parameter. ",
      "No plot is provided."
    )
    return(invisible())
  }

  # get default values for xlab etc.
  n_plot <- length(prms_to_plot)
  col_line <- rep(
    if (is.null(col_line)) "black" else col_line,
    length.out = n_plot
  )
  col_shade <- rep(
    if (is.null(col_shade)) grDevices::rgb(0, 0, 0.5, 0.5) else col_shade,
    length.out = n_plot
  )
  xlab <- rep(if (is.null(xlab)) prms_to_plot else xlab, length.out = n_plot)
  ylab <- rep(if (is.null(ylab)) "Density" else ylab, length.out = n_plot)
  xlim <- rep(if (is.null(xlim)) list(NA) else xlim, length.out = n_plot)
  ylim <- rep(if (is.null(ylim)) list(NA) else ylim, length.out = n_plot)

  # iterate over each parameter and plot the posterior density
  for (i in seq_along(prms_to_plot)) {
    one_prm <- prms_to_plot[i]
    prm_idx <- which(one_prm == all_prm_names)
    sub_chains <- chains[prm_idx, , ]
    dens_one_prm <- stats::density(as.numeric(sub_chains))

    # prepare x and y limits
    xlim_one_prm = xlim[[i]]
    if (is.na(xlim_one_prm)) {
      range_sub <- range(sub_chains)
      padding <- 0.05 * diff(range_sub) # scale by 5%
      xlim_one_prm <- c(range_sub[1] - padding, range_sub[2] + padding)
    }
    ylim_one_prm = ylim[[i]]
    if (is.na(ylim_one_prm)) {
      max_sub <- max(dens_one_prm$y)
      ylim_one_prm <- c(0, 1.05 * max_sub) # scale max by 5%
    }

    plot(
      NA,
      ylim = ylim_one_prm,
      xlab = xlab[i],
      ylab = ylab[i],
      xlim = xlim_one_prm
    )
    graphics::polygon(
      x = c(dens_one_prm$x, rev(dens_one_prm$x)),
      y = c(dens_one_prm$y, rep(0, length(dens_one_prm$y))),
      col = col_shade[i],
      border = col_line[i]
    )
  }
  invisible()
}


#' @rdname plot_mcmc_trace
#' @keywords internal
plot_mcmc_auto <- function(
  chains,
  lags = 1:30,
  col_line = NULL,
  which_prms = NULL,
  xlab = NULL,
  ylab = NULL,
  xlim = NULL,
  ylim = NULL,
  type = NULL,
  main = NULL
) {
  stopifnot(length(dim(chains)) == 3)

  # get the prm labels
  all_prm_names <- dimnames(chains)[[1]]
  prms_to_plot <- all_prm_names
  if (!is.null(which_prms)) {
    prms_to_plot <- grep(pattern = which_prms, x = prms_to_plot, value = TRUE)
  }
  if (length(prms_to_plot) == 0) {
    warning(
      "The argument `which_prms` doesn't match with any parameter. ",
      "No plot is provided."
    )
    return(invisible())
  }

  # transform to coda mcmc_list and get the autocorrelation values
  mcmc_list = mcmc_dm_to_coda_mcmc(chains)
  auto_cors = coda::autocorr.diag(mcmc_list, lags = lags)

  # get default values for xlab etc.
  n_plot <- length(prms_to_plot)
  col_line <- rep(
    if (is.null(col_line)) "black" else col_line,
    length.out = n_plot
  )
  xlab <- rep(if (is.null(xlab)) "Lag" else xlab, length.out = n_plot)
  ylab <- rep(
    if (is.null(ylab)) "Autocorrelation" else ylab,
    length.out = n_plot
  )
  xlim <- rep(
    if (is.null(xlim)) list(range(lags)) else xlim,
    length.out = n_plot
  )
  ylim <- rep(if (is.null(ylim)) list(c(-1, 1)) else ylim, length.out = n_plot)
  type <- rep(if (is.null(type)) "h" else type, length.out = n_plot)
  main <- rep(if (is.null(main)) prms_to_plot else main, length.out = n_plot)

  # iterate over each parameter and plot the posterior density
  for (i in seq_along(prms_to_plot)) {
    one_prm <- prms_to_plot[i]
    prm_idx <- which(one_prm == all_prm_names)
    subset_auto_corrs = auto_cors[, one_prm]

    # prepare x and y limits
    xlim_one_prm = xlim[[i]]
    ylim_one_prm = ylim[[i]]

    plot(
      subset_auto_corrs ~ lags,
      ylim = ylim_one_prm,
      xlab = xlab[i],
      ylab = ylab[i],
      xlim = xlim_one_prm,
      ty = type[i],
      col = col_line[i],
      main = main[i]
    )
  }
  invisible()
}


# HELPER FUNCTIONS --------------------------------------------------------

#' Set Default Colors
#'
#' @description
#' This function assigns default colors if none are provided or adjusts the
#' color vector to match the number `n`
#'
#' @param colors character vector, specifying colors for conditions. If `NULL`,
#' default colors are used.
#' @param n
#'
#' @return A character vector of colors of length `n`.
#'
#' @keywords internal
set_default_colors <- function(colors, n) {
  color_blind_palette = grDevices::palette.colors(palette = "Okabe-Ito")
  color_blind_palette = color_blind_palette[c(4, 7, 1, 2, 3, 5, 6, 8)] # just ordering
  if (n <= 8) {
    def_cols = color_blind_palette[1:n]
  } else {
    def_cols = c(color_blind_palette, grDevices::rainbow(n = n - 8))
  }

  if (is.null(colors)) {
    colors <- def_cols
  } else if (length(colors) == 1) {
    colors <- rep(colors, n)
  } else if (length(colors) != n) {
    stop("The number of colors must match the number of conditions")
  }
  return(colors)
}


#' Set default graphical parameters for plotting in dRiftDM
#'
#' This function sets/updates graphical parameters (passed through `...` by
#' `plot.*()` methods of `dRiftDM`) and handles default settings.
#' It supports arguments relevant to
#' [graphics::plot()], [graphics::points()], [graphics::legend()], and
#' [graphics::arrows()], allowing users fine-grained control over plotting
#' while avoiding argument clashes. This is an internal function, so users
#' won't call it directly. However, the Details below list all optional
#' arguments that users can provide when calling the `plot.*` methods
#'
#' @param dots a named list of graphical parameters (created from
#'   `...` when passing additional arguments to `plot.*()` methods of
#'   `dRiftDM`).
#' @param leg a character vector, used to set default legend entries.
#' @param id a character or numeric participant value.
#'
#' @details
#'
#' The following list shows possible arguments and default values for...
#'
#' shades, lines, points:
#'
#' - `obs.pch`: plotting symbol for points of observed data (default: `19`)
#' - `obs.pt.bg`: background color for points (default: `NA`)
#' - `pred.lty`: line type for lines of predicted data (default: `1`)
#' - `pred.lwd`: line width for lines (default: `1`)
#' - `alpha`: alpha transparency for shaded prediction intervals
#'    (default: `0.2`)
#' - `b.lty`: line type for decision boundaries (default: `1`)
#' - `b.lwd`: line width for decision boundaries (default: `1`)
#'
#' text and scaling:
#'
#' - `cex.axis`, `cex.lab`, `cex.main`, `cex.pt`, `cex.legend`: text and point
#'   scaling factors (default: `1`)
#' - `main`: plot title (default: NULL, with `id` appended if applicable)
#' - `family`: font family (default: empty string)
#'
#' legend:
#'
#' - `legend`: legend labels (default: `leg`)
#' - `legend.pos`: position of the primary legend (default: `"bottomright"`,
#'   it is also possible to specify a numeric vector for `legend.pos`, providing
#'   the exact x- and y-position)
#' - `legend.bg`: background color of the legend box (default: `"white"`)
#' - `bty`: box type for the legend (default: `"o"`)
#' - `box.lwd`: border line width of the legend box (default: `1`)
#' - `box.col`: border color of the legend box (default: `"black"`)
#' - `box.lty`: border line type of the legend box (default: `1`)
#'
#' - `lines.legend`: optional labels for a separate legend for line types
#'   (only relevant for `plot.densities()`, default: `NULL`)
#' - `lines.legend.pos`: position of that separate legend (default:
#'   `"topright"`, passing a vector with exact x- and y-positions for
#'   `lines.legend` is possible)
#'
#' error bars:
#'
#' - `err.width`: width of caps on error bars (default: `0.05`)
#' - `err.col`: color of error bars (default: `NULL`; resolved to
#'   line/point colors, black or gray, depending on the plot type; recycled
#'   across conditions)
#' - `err.eps`: threshold under which an error bar is treated as too small to
#'   display (default: `0.1%` of the y-axis range)
#'
#' observed histogram and KDE (for plot of RT distributions/densities):
#'
#' - `obs.hist.col`: fill color for histograms (default: `"gray20"`)
#' - `obs.kde.col`: color of KDE lines (default: `"black"`)
#' - `obs.kde.lty`: line type of KDE lines (default: `2`)
#' - `obs.kde.lwd`: line width of KDE lines (default: `1`)
#'
#' ETC.:
#'
#' - `horiz.col`: color of a horizontal axis line at y = 0 (default: `"gray"`)
#' - `horiz.lwd`: color of a horizontal axis line at y = 0 (default: `1`)
#'
#' @returns A list with all updated graphical parameters, ready to be passed
#' to plotting functions. (done internally, not by the user)
#'
#' @examples
#' # This is not an example of calling set_default_arguments() directly
#' # (because it is an internal function), but it illustrates how user-supplied
#' # plotting arguments are processed via this helper.
#'
#' some_data <- ulrich_flanker_data
#' some_stats <- calc_stats(some_data, type = "quantiles")
#'
#' # See also ?plot.quantiles for more detail
#' plot(
#'   some_stats,
#'   obs.pch = 21,        # optional argument 1: point type
#'   obs.pt.bg = "black", # optional argument 2: point background
#'   legend = c("foo", "bar") # optional argument 3: custom legend labels
#' )
#' # for a full list of optional arguments, see the Details above
#'
#' @keywords internal
set_default_arguments <- function(dots, leg, id) {
  # standard line types and point types
  dots$obs.pch = if (is.null(dots$obs.pch)) 19 else dots$obs.pch
  dots$pred.lty = if (is.null(dots$pred.lty)) 1 else dots$pred.lty
  dots$obs.pt.bg = if (is.null(dots$obs.pt.bg)) NA else dots$obs.pt.bg
  dots$pred.lwd = if (is.null(dots$pred.lwd)) 1 else dots$pred.lwd
  dots$b.lty = if (is.null(dots$b.lty)) 1 else dots$b.lty
  dots$b.lwd = if (is.null(dots$b.lwd)) 1 else dots$b.lwd

  # shading settings
  dots$alpha = if (is.null(dots$alpha)) 0.2 else dots$alpha

  # general cex settings
  dots$cex.axis = if (is.null(dots$cex.axis)) 1 else dots$cex.axis
  dots$cex.lab = if (is.null(dots$cex.lab)) 1 else dots$cex.lab
  dots$cex.main = if (is.null(dots$cex.main)) 1 else dots$cex.main
  dots$cex.pt = if (is.null(dots$cex.pt)) 1 else dots$cex.pt
  dots$cex.legend = if (is.null(dots$cex.legend)) 1 else dots$cex.legend

  # main and family
  if (!is.null(id)) {
    if (is.null(dots$main)) {
      dots$main = id
    } else {
      dots$main = paste(dots$main, id, sep = " - ")
    }
  }
  dots$family = if (is.null(dots$family)) "" else dots$family

  # the primary legend settings
  dots[["legend"]] = if (is.null(dots[["legend"]])) leg else dots[["legend"]]
  dots$legend.pos = if (is.null(dots$legend.pos)) {
    "bottomright"
  } else {
    dots$legend.pos
  }
  dots$legend.bg = if (is.null(dots$legend.bg)) "white" else dots$legend.bg
  dots$bty = if (is.null(dots$bty)) "o" else dots$bty
  dots$box.lwd = if (is.null(dots$box.lwd)) 1 else dots$box.lwd
  dots$box.col = if (is.null(dots$box.col)) "black" else dots$box.col
  dots$box.lty = if (is.null(dots$box.lty)) 1 else dots$box.lty

  # specific settings for the line legend
  dots[["lines.legend"]] = if (is.null(dots[["lines.legend"]])) {
    NULL
  } else {
    dots[["lines.legend"]]
  }
  dots$lines.legend.pos = if (is.null(dots$lines.legend.pos)) {
    "topright"
  } else {
    dots$lines.legend.pos
  }

  # error bar settings
  dots$err.width = if (is.null(dots$err.width)) 0.05 else dots$err.width
  dots$err.col = if (is.null(dots$err.col)) NULL else dots$err.col
  dots$err.eps = if (is.null(dots$err.eps)) .001 else dots$err.eps

  # color and line settings of kdes and histograms
  dots$obs.hist.col = if (is.null(dots$obs.hist.col)) {
    "gray20"
  } else {
    dots$obs.hist.col
  }
  dots$obs.kde.col = if (is.null(dots$obs.kde.col)) {
    "black"
  } else {
    dots$obs.kde.col
  }
  dots$obs.kde.lty = if (is.null(dots$obs.kde.lty)) 2 else dots$obs.kde.lty
  dots$obs.kde.lwd = if (is.null(dots$obs.kde.lwd)) 1 else dots$obs.kde.lwd

  # etc.
  dots$horiz.col = if (is.null(dots$horiz.col)) "gray" else dots$horiz.col
  dots$horiz.lwd = if (is.null(dots$horiz.lwd)) 1 else dots$horiz.lwd

  return(dots)
}
