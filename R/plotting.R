

# FUNCTION FOR PLOTTING TRACES ---------------------------------------------


# Core plotting function to handle trace and boundary plotting
plot_one_traces <- function(traces_obj, col, col_b, xlab, ylab, xlim,
                            ylim, lty, type, new_plot, ...) {

  # Initialize plot
  if (new_plot) {
    plot(c(1, 2) ~ c(1, 1), col = "white", xlab = xlab, ylab = ylab,
         xlim = xlim, ylim = ylim, ...)
    graphics::abline(h = 0, col = "gray", cex = 0.5)
  }

  # Plot each condition's traces
  e_samples = unpack_traces(traces_obj)
  t_vec = attr(traces_obj, "t_vec")

  for (i in 1:nrow(e_samples)) {
    one_trace = e_samples[i,]
    graphics::points(one_trace ~ t_vec, type = type, col = col,
                     lty = lty)
  }

  b_vals <- attr(traces_obj, "b_vals")
  graphics::points(b_vals ~ t_vec, type = type, col = col_b, lty = lty,
                   ...)
  graphics::points(-b_vals ~ t_vec, type = type, col = col_b, lty = lty,
                   ...)
}


#' Plot Traces of a Drift Diffusion Model
#'
#'
#' @description
#' Creates a basic plot showing simulated traces (simulated evidence
#' accumulation processes) from a drift diffusion model. Such plots are useful
#' for exploring and testing model behavior, allowing users to visualize the
#' traces.
#'
#'
#' @param x an object of type `traces_dm_list` or `traces_dm`, containing the
#' traces to be plotted, resulting from a call to [dRiftDM::simulate_traces].
#' @param col character, vector of colors for the evidence accumulation traces,
#' one per condition. Defaults to a rainbow palette if not specified.
#' @param col_b character, a vector of colors for the boundary lines.
#' Defaults to black for all conditions.
#' @param xlim,ylim numeric vectors of length 2, specifying the x and y axis
#'  limits.
#' @param xlab,ylab character, labels for the x and y axes.
#' @param lty integer, line type for both the traces and boundary lines.
#' @param type character, type of plot to use for traces and boundaries.
#' @param legend character vector, specifying legend labels, corresponding to
#' the conditions in the traces. Defaults to the condition names.
#' @param legend_pos character, specifying the position of the legend on the
#' plot.
#' @param ... additional arguments passed to the [plot], [graphics::points],
#'  and [graphics::legend] functions. Not all may work due to a clash of
#'  of arguments.
#'
#'
#' @details
#'
#' `plot.traces_dm_list()` iterates over all conditions and plots the traces.
#'  It includes a legend with condition labels.
#'
#' `plot_traces_dm` only plots the traces provided (i.e., traces for one
#' condition)
#'
#' Boundaries and traces are color-coded according to `col` and `col_b`. The
#' function automatically generates the upper and lower boundaries based on
#' the information stored within `x`.
#'
#'
#' @seealso [dRiftDM::simulate_traces]
#'
#' @export
plot.traces_dm_list <- function(x, ..., col = NULL, col_b = NULL, xlim = NULL,
                                ylim = NULL, xlab = "Time", ylab = "Evidence",
                                lty = 1, type = "l", legend = NULL,
                                legend_pos = "topright") {

  unique_conds <- names(x)
  t_vec <- attr(x, "t_vec")

  # Set defaults for colors, x/y limits, and legend
  col <- set_default_colors(
    colors = col, unique_conds = unique_conds,
    default_colors = grDevices::rainbow(n = length(unique_conds))
  )
  col_b <- set_default_colors(
    colors = col_b, unique_conds = unique_conds,
    default_colors = rep("black", length(unique_conds))
  )
  xlim <- set_plot_limits(lim = xlim, default_lim = c(0, max(t_vec) / 4))
  ylim <- set_plot_limits(
    lim = ylim,
    default_lim = c(-max(unlist(x), na.rm = TRUE), max(unlist(x), na.rm = TRUE))
  )
  if (is.null(legend)) {
    legend = unique_conds
  }

  # iterate over all traces
  plot_one_traces(x[[1]], col[1], col_b[1], xlab, ylab, xlim, ylim,
                  lty, type, new_plot = T)
  n_all = length(x)
  if (n_all == 1) return(invisible(NULL))
  for (idx in 2:n_all) {
    plot_one_traces(x[[idx]], col[idx], col_b[idx], xlab, ylab, xlim, ylim,
                    lty, type, new_plot = F)
  }

  # add legend
  graphics::legend(
    x = legend_pos, legend = legend, col = col, lty = lty, bg = "white", ...
  )
}

#' @rdname plot.traces_dm_list
#' @export
plot.traces_dm <- function(x, ..., col = NULL, col_b = NULL, xlim = NULL,
                           ylim = NULL, xlab = "Time", ylab = "Evidence",
                           lty = 1, type = "l") {

  t_vec <- attr(x, "t_vec")
  unique_conds <- "one_cond_dummy"

  # Set defaults for colors, x/y limits, and legend
  col <- set_default_colors(
    colors = col, unique_conds = unique_conds,
    default_colors = grDevices::rainbow(n = length(unique_conds))
  )
  col_b <- set_default_colors(
    colors = col_b, unique_conds = unique_conds,
    default_colors = rep("black", length(unique_conds))
  )
  xlim <- set_plot_limits(lim = xlim, default_lim = c(0, max(t_vec) / 4))
  ylim <- set_plot_limits(
    lim = ylim,
    default_lim = c(-max(unlist(x), na.rm = TRUE), max(unlist(x), na.rm = TRUE))
  )

  # plot the trace
  plot_one_traces(x, col, col_b, xlab, ylab, xlim, ylim, lty, type,
                  new_plot = T)
}



# FUNCTIONS FOR PLOTTING STATISTICS ---------------------------------------


#' Plot Conditional Accuracy Functions (CAFs)
#'
#' @description
#' This function generates a plot of Conditional Accuracy Functions (CAFs). It
#' can display observed and predicted values, making it useful for assessing
#' model fit or exploring observed data.
#'
#' If the data contains multiple IDs, CAFs are aggregated across IDs before
#' plotting.
#'
#' @param x a [data.frame], containing CAFs, typically resulting from a call
#'  to [dRiftDM::calc_stats].
#' @param conds character vector, specifying the conditions to plot.
#'  Defaults to all unique conditions.
#' @param col Character vector, specifying colors for each condition. If a
#'  single color is provided, it will be repeated for each condition.
#' @param xlim,ylim numeric vectors of length 2, specifying the x and y axis
#'  limits.
#' @param xlab,ylab character, labels for the x and y axes.
#' @param pch integer, specifying the plotting symbol for observed data points.
#' @param lty integer, line type for the predicted CAFs.
#' @param type character, type of plot for the predicted CAFs.
#' @param legend character vector, specifying legend labels corresponding to
#' the conditions in the CAFs. Defaults to the condition names.
#' @param legend_pos character, specifying the position of the legend on the
#' plot.
#' @param ... additional arguments passed to the [plot], [graphics::points],
#'  and [graphics::legend] functions. Not all may work due to a clash of
#'  of arguments.
#'
#'
#' @details
#' The `plot.cafs` function allows for a quick investigation of CAFs, including
#' options for color, symbols, and line types for different data sources
#' (observed vs. predicted). When the supplied [data.frame] includes multiple
#' IDs, CAFs are aggregated across IDs before plotting.
#'
#' @export
plot.cafs <- function(x, ..., conds = NULL, col = NULL, xlim = NULL,
                      ylim = c(0, 1), xlab = "Bins", ylab = NULL, pch = 21,
                      lty = 1, type = "l", legend = NULL,
                      legend_pos = "bottomright") {

  cafs <- x
  caf_name = grep("^P_", colnames(cafs), value = T)


  if ("ID" %in% colnames(cafs) && length(unique(cafs$ID)) >  1) {
    message("Aggregating across ID")
    cafs = aggregate_stats(stat_df = cafs)
  }

  # set default arguments
  if (is.null(conds)) {
    conds <- unique(cafs$Cond)
  }
  conds = match.arg(arg = conds, choices = unique(cafs$Cond),
                    several.ok = T)

  if (is.null(ylab)) {
    # f(upper_boundery_name)
    u_name = substr(caf_name, 3, nchar(caf_name))
    ylab <- paste("f(", u_name, ")", sep = "")
  }
  xlim <- set_plot_limits(
    lim = xlim, default_lim = c(min(cafs$Bin), max(cafs$Bin))
  )

  col <- set_default_colors(
    colors = col, unique_conds = conds,
    default_colors = grDevices::rainbow(n = length(conds))
  )

  if (is.null(legend)) {
    legend = conds
  }


  # prepare plot
  plot(c(1, 2) ~ c(1, 1),
    col = "white", xlab = xlab, ylab = ylab, xlim = xlim,
    ylim = ylim, ...
  )

  # iterate over all conditions and plot everything
  for (idx in seq_along(conds)) {
    sub_dat <- cafs[cafs$Cond == conds[idx], ]
    sub_dat_obs <- sub_dat[sub_dat$Source == "obs", ]
    if (nrow(sub_dat_obs) > 0) {
      graphics::points(
        sub_dat_obs[[caf_name]] ~ sub_dat_obs$Bin,
        col = col[idx],
        type = "p",
        pch = pch,
        ...
      )
    }

    sub_dat_pred <- sub_dat[sub_dat$Source == "pred", ]
    if (nrow(sub_dat_pred) > 0) {
      graphics::points(
        sub_dat_pred[[caf_name]] ~ sub_dat_pred$Bin,
        col = col[idx],
        type = type,
        lty = lty,
        ...
      )
    }
  }

  # plot the legend
  if (!any(cafs$Source == "pred")) {
    lty = NA
  }
  if (!any(cafs$Source == "obs")) {
    pch = NA
  }
  if (length(legend) > 1) {
    graphics::legend(x = legend_pos,
      legend = legend,
      col = col, lty = lty, pch = pch, ...
    )
  }
}




#' Plot Quantiles
#'
#' @description
#' This function generates a plot of quantiles. It can display observed and
#' predicted values, making it useful for assessing model fit or exploring
#' observed data distributions.
#'
#' If the data contains multiple IDs, quantiles are aggregated across IDs
#' before plotting.
#'
#' @param x a [data.frame], containing quantiles, typically resulting from a
#' call to [dRiftDM::calc_stats].
#' @param conds character vector, specifying the conditions to plot. Defaults to
#' all unique conditions.
#' @param dv character, specifying the quantiles to plot. Defaults to
#'  quantiles derived from the upper boundary.
#' @param col character vector, specifying colors for each condition. If a
#'  single color is provided, it will be repeated for each condition.
#' @param xlim,ylim numeric vectors of length 2, specifying the x and y axis
#'  limits.
#' @param xlab,ylab character, labels for the x and y axes.
#' @param pch integer, specifying the plotting symbol for observed data points.
#' @param lty integer, line type for the predicted quantiles.
#' @param type character, type of plot for the predicted quantiles.
#' @param legend character vector, specifying legend labels corresponding to
#'  the conditions in the quantiles. Defaults to the condition names.
#' @param legend_pos character, specifying the position of the legend on the
#'  plot.
#' @param ... additional arguments passed to the [plot], [graphics::points],
#'  and [graphics::legend] functions. Not all may work due to a clash of
#'  arguments.
#'
#' @details
#' The `plot.quantiles` function allows for a quick investigation of quantiles,
#' including options for color, symbols, and line types for different data
#' sources (observed vs. predicted). When the supplied [data.frame] includes
#' multiple IDs, quantiles are aggregated across IDs before plotting.
#'
#' @export
plot.quantiles <- function(x, ..., conds = NULL, dv = NULL, col = NULL,
                           xlim = NULL, ylim = c(0, 1), xlab = "RT [s]",
                           ylab = "F(RT)", pch = 21, lty = 1, type = "l",
                           legend = NULL, legend_pos = "bottomright") {

  quantiles <- x


  # aggregate
  if ("ID" %in% colnames(quantiles) && length(unique(quantiles$ID)) >  1) {
    message("Aggregating across ID")
    quantiles = aggregate_stats(stat_df = quantiles)
  }


  # set default plot arguments
  if (is.null(conds)) {
    conds <- unique(quantiles$Cond)
  }
  conds = match.arg(arg = conds, choices = unique(quantiles$Cond),
                           several.ok = T)

  u_name = names(attr(quantiles, "b_coding")$u_name_value)
  if (is.null(dv)) {
    dv = paste("Quant", u_name, sep = "_")
  }

  xlim <- set_plot_limits(
    lim = xlim,
    default_lim = c(min(quantiles[[dv]]) * 0.75, max(quantiles[[dv]]) * 1.25)
  )

  col <- set_default_colors(
    colors = col, unique_conds = conds,
    default_colors = grDevices::rainbow(n = length(conds))
  )

  if (is.null(legend)) {
    legend = conds
  }


  # prepare plot
  plot(c(1, 2) ~ c(1, 1),
    col = "white", xlab = xlab, ylab = ylab,
    xlim = xlim, ylim = ylim, ...
  )


  # iterate over all conditions
  for (idx in seq_along(conds)) {
    sub_dat <- quantiles[quantiles$Cond == conds[idx], ]
    sub_dat_obs <- sub_dat[sub_dat$Source == "obs", ]
    if (nrow(sub_dat_obs) > 0) {
      graphics::points(sub_dat_obs$Prob ~ sub_dat_obs[[dv]],
        col = col[idx], pch = pch, ...
      )
    }
    sub_dat_pred <- sub_dat[sub_dat$Source == "pred", ]
    if (nrow(sub_dat_pred) > 0) {
      graphics::points(sub_dat_pred$Prob ~ sub_dat_pred[[dv]],
        type = type, lty = lty, col = col[idx], ...
      )
    }
  }

  # plot the legend
  if (!any(quantiles$Source == "pred")) {
    lty = NA
  }
  if (!any(quantiles$Source == "obs")) {
    pch = NA
  }
  if (length(legend) > 1) {
    graphics::legend(x = legend_pos,
      legend = legend,
      col = col, lty = lty, pch = pch, ...
    )
  }
}


#' Plot Delta Functions
#'
#' @description
#' This function generates a plot of delta functions, displaying observed and
#' predicted values, which can be useful for evaluating model fit or exploring
#' data characteristics.
#'
#' If the data contains multiple IDs, delta functions are aggregated across IDs
#' before plotting.
#'
#' @param x a [data.frame], containing delta functions, typically resulting from
#'  a call to [dRiftDM::calc_stats].
#' @param dv character vector, specifying the delta functions to plot. Defaults
#'  to all columns beginning with "Delta_" in `x`.
#' @param col character vector, specifying colors for each delta function. If a
#'  single color is provided, it will be repeated for each function.
#' @param xlim,ylim numeric vectors of length 2, specifying the x and y axis
#'  limits.
#' @param xlab,ylab character, labels for the x and y axes.
#' @param pch integer, specifying the plotting symbol for observed data points.
#' @param lty integer, line type for the predicted delta functions.
#' @param type character, type of plot for the predicted delta functions.
#' @param legend character vector, specifying legend labels corresponding to
#'  the delta functions. Defaults to the way functions were derived.
#' @param legend_pos character, specifying the position of the legend on the
#'  plot.
#' @param ... additional arguments passed to the [plot], [graphics::points],
#'  and [graphics::legend] functions. Not all may work due to a clash of
#'  arguments.
#'
#' @details
#' The `plot.delta_funs` function provides an easy way to investigate delta
#' functions, allowing for customization in color, symbols, and line types for
#' different data sources (observed vs. predicted). If multiple IDs are present
#' in the data, delta functions are aggregated across IDs before plotting.
#' By default, `ylim` is set to twice the range of the delta values to provide
#' more context.
#'
#' @export
plot.delta_funs <- function(x, ..., dv = NULL, col = NULL, xlim = NULL,
                            ylim = NULL, xlab = "RT [s]",
                            ylab = expression(Delta), pch = 21, lty = 1,
                            type = "l", legend = NULL,
                            legend_pos = "topright") {


  delta_fun <- x

  # aggregate
  if ("ID" %in% colnames(delta_fun) && length(unique(delta_fun$ID)) >  1) {
    message("Aggregating across ID")
    delta_fun = aggregate_stats(stat_df = delta_fun)
  }


  # get the columns to plot
  delta_columns = grep("^Delta_", colnames(delta_fun), value = T)
  if (is.null(dv)) {
    dv = delta_columns
  }
  dv = match.arg(arg = dv, choices = delta_columns, several.ok = T)

  uv <- gsub(pattern = "^Delta_", replacement = "", x = dv)
  uv <- paste("Avg_", uv, sep = "")
  stopifnot(length(uv) == length(dv))

  # set default plot arguments
  all_y_vals = unlist(delta_fun[dv])
  y_r <- range(all_y_vals)
  ylim <- c(y_r[1] - (y_r[2] - y_r[1]) / 2, y_r[2] + (y_r[2] - y_r[1]) / 2)
  ylim <- set_plot_limits(
    lim = ylim,
    default_lim = y_r
  )

  all_x_vals = unlist(delta_fun[uv])
  xlim <- set_plot_limits(
    lim = xlim,
    default_lim = c(min(all_x_vals) * 0.75, max(all_x_vals) * 1.25)
  )

  if (length(dv) == 1) {
    def_colors = "black"
  } else {
    def_colors = grDevices::rainbow(n = length(dv))
  }

  col <- set_default_colors(
    colors = col, unique_conds = dv, # dv; doesn't matter because length counts
    default_colors = def_colors
  )

  # prepare plot
  plot(c(1, 2) ~ c(1, 1),
    col = "white", xlab = xlab, ylab = ylab,
    xlim = xlim, ylim = ylim, ...
  )


  # iterate over all dv(s)
  for (idx in seq_along(dv)) {
    sub_dat_obs <- delta_fun[delta_fun$Source == "obs", ]
    sub_dat_obs <- sub_dat_obs[c(dv[idx], uv[idx])]
    if (nrow(sub_dat_obs) > 0) {
      graphics::points(sub_dat_obs[[1]] ~ sub_dat_obs[[2]],
        col = col[idx], pch = pch, ...
      )
    }
    sub_dat_pred <- delta_fun[delta_fun$Source == "pred", ]
    sub_dat_pred <- sub_dat_pred[c(dv[idx], uv[idx])]
    if (nrow(sub_dat_pred) > 0) {
      graphics::points(sub_dat_pred[[1]] ~ sub_dat_pred[[2]],
        type = type, col = col[idx], lty = lty, ...
      )
    }
  }


  # plot the legend
  if (!any(delta_fun$Source == "pred")) {
    lty = NA
  }
  if (!any(delta_fun$Source == "obs")) {
    pch = NA
  }
  legend = gsub(pattern = "Delta_", replacement = "", x = dv)
  if (length(legend) > 1) {
    graphics::legend(x = legend_pos,
      legend = legend,
      col = col, lty = lty, pch = pch, ...
    )
  }
}



#' Plot Multiple Statistics
#'
#' @description
#' This function iterates over a list of statistics data, resulting from a call
#' to [dRiftDM::calc_stats()], and subsequently plots each statistic. It allows
#' for flexible arrangement of multiple plots on a single graphics device.
#'
#'
#' @param x an object of type `list_stats_dm`, which is essentially a list
#'  multiple statistics, resulting from a call to [dRiftDM::calc_stats()].
#' @param mfrow an optional numeric vector of length 2, specifying the number of
#'  rows and columns for arranging multiple panels in a single plot
#'  (e.g., `c(1, 3)`). Plots are provided sequentially if `NULL` (default),
#'  using the current graphics layout of a user.
#' @param ... additional arguments passed to the [plot] function for each
#'  individual `stats_dm` object in `x`.
#'
#' @details
#' The `plot.list_stats_dm` function is "merely" a wrapper. All plotting
#' is done by the respective `plot` method.
#'
#' @seealso [dRiftDM::plot.cafs()], [dRiftDM::plot.quantiles()],
#' [dRiftDM::plot.delta_funs()], [dRiftDM::calc_stats()]
#'
#' @export
plot.list_stats_dm <- function(x, ..., mfrow = NULL) {


  if (!is.null(mfrow)) {
    withr::local_par(mfrow = mfrow)
  }

  for (one_stats_obj in x) {
    plot(one_stats_obj, ...)
  }
}




# HISTOGRAM of Parameters -------------------------------------------------


#' Plot Parameter Distribution(s)
#'
#' This function creates a histogram for each parameter in a `coefs_dm` object,
#' resulting from a call to [dRiftDM::coef.fits_ids_dm].
#'
#' @param x an object of class `coefs_dm` (see [dRiftDM::coef.fits_ids_dm])
#' @param separate_plots logical, indicating whether to display separate panels
#'  for each parameter in a single plot layout (`TRUE`), or to plot them
#'  sequentially (`FALSE`).
#' @param alpha numeric, specifying the transparency level for histogram colors
#'  when conditions are present, with values between 0 (fully transparent) and
#'  1 (fully opaque).
#' @param main character vector, specifying titles for each parameter histogram.
#'  Defaults to parameter names.
#' @param colors character vector, specifying colors for each condition if
#'  conditions are present. Defaults to a rainbow color palette.
#'  If `NULL` and no conditions are present, the default color is `"skyblue"`.
#' @param xlab character, specifying the label for the x-axis.
#' @param ... additional arguments passed to the [graphics::hist] function.
#'
#' @details
#' The `hist.coefs_dm` function is designed for visualizing parameter
#' distributions for a single fit procedure. If multiple conditions are present,
#' it overlays histograms for each condition with adjustable transparency. When
#' `separate_plots` is set to `TRUE`, histograms for each parameter are
#' displayed in a grid layout within a single graphics device.
#'
#' @export
hist.coefs_dm <- function(x, ..., separate_plots = T, alpha = 0.5,
                          main = NULL, colors = NULL, xlab = "values") {

  coefs_obj <- x

  # get the parameter and condition names (if existant)
  prm_names <- setdiff(colnames(coefs_obj), c("ID", "Cond"))
  conds_present <- "Cond" %in% colnames(coefs_obj)
  conds = unique(coefs_obj$Cond)

  # create colors
  if (conds_present && is.null(colors)) {
    colors = grDevices::rainbow(n = length(conds))
  } else if (is.null(colors)) {
    colors = "skyblue"
  }

  # create mains
  if (is.null(main)) {
    main = prm_names
  }
  if (length(main) != length(prm_names)) {
    stop("the number of entries in main must match with the number of ",
         "parameters")
  }



  # figure out the plot outline if separate panels shall be plotted in one plot
  n_plots <- length(prm_names)
  n_rows <- ceiling(sqrt(n_plots))
  n_cols <- ceiling(n_plots / n_rows)
  if (separate_plots) withr::local_par(mfrow = c(n_rows, n_cols))

  # iterate through all paramters
  for (prm_idx in seq_along(prm_names)) {

    # if no condition present, then just call hist
    if (!conds_present) {
      graphics::hist(
        coefs_obj[[prm_names[prm_idx]]],
        col = colors, main = main[prm_idx], xlab = xlab, ...
      )
      next()
    }

    # if conditions are present, then iterate through the conditions ...
    x_r <- range(coefs_obj[[prm_names[prm_idx]]])
    xlim <- c(x_r[1] - (x_r[2] - x_r[1]) / 5, x_r[2] + (x_r[2] - x_r[1]) / 5)

    for (cond_idx in seq_along(conds)) {
      # get subset for one prm and one conditon
      subset_prm <-
        coefs_obj[[prm_names[prm_idx]]][coefs_obj$Cond == conds[cond_idx]]
      # plot or add histogram
      graphics::hist(
        subset_prm,
        col = grDevices::adjustcolor(colors[cond_idx], alpha.f = alpha),
        add = cond_idx != 1, main = main[prm_idx], xlim = xlim,
        xlab = xlab, ...
      )
    }
  }
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
#' @param conds character vector, specifying conditions to plot. Defaults to all
#' conditions in `x`.
#' @param col character vector, specifying colors for each condition. If a
#' single color is provided, it will be repeated for each condition.
#' @param xlim numeric vector of length 2, specifying the x-axis limits for
#' components related to the time space.
#' @param legend character vector, specifying legend labels corresponding to the
#' conditions.
#' @param legend_pos character, specifying the position of the legend on the
#' plot (e.g., `"topright"`).
#' @param ... additional arguments passed forward.
#'
#' @details
#' The `plot.drift_dm` function provides an overview of key DDM components,
#' which include:
#' - `mu_fun`: Drift rate over time.
#' - `mu_int_fun`: Integrated drift rate over time.
#' - `x_fun`: Starting condition as a density across evidence values.
#' - `b_fun`: Boundary values over time.
#' - `dt_b_fun`: Derivative of the boundary function over time.
#' - `nt_fun`: Non-decision time as a density over time.
#'
#' For each component, if multiple conditions are specified, they will be
#' plotted using different colors as specified in `color`.
#'
#' When the evaluation of a model component fails, the respective component
#' will not be plotted, but no warning is ushered.
#'
#'
#' @export
plot.drift_dm <- function(x, ..., conds = NULL, col = NULL, xlim = NULL,
                          legend = NULL, legend_pos = "topright") {

  drift_dm_obj <- x


  # get conditions
  if (is.null(conds)) {
    conds <- conds(drift_dm_obj)
  }
  conds = match.arg(arg = conds, choices = conds(drift_dm_obj),
                    several.ok = T)

  # get default parameters
  col <- set_default_colors(
    colors = col, unique_conds = conds,
    default_colors = grDevices::rainbow(n = length(conds))
  )


  t_max = drift_dm_obj$prms_solve[["t_max"]]

  if (is.null(xlim)) {
    xlim <- c(0, t_max / 4)
  }

  if (is.null(legend)) {
    legend = conds
  }


  # get the time and evidence space
  nx <- drift_dm_obj$prms_solve[["nx"]]
  nt <- drift_dm_obj$prms_solve[["nt"]]
  dx <- drift_dm_obj$prms_solve[["dx"]]
  dt <- drift_dm_obj$prms_solve[["dt"]]

  x_vec <- seq(-1, 1, length.out = nx + 1)
  t_vec <- seq(0, t_max, length.out = nt + 1)

  # get all components
  all_vals = comp_vals(drift_dm_obj, x_vec = x_vec, t_vec = t_vec,
                           nx = nx, nt = nt, dx = dx, dt = dt, t_max = t_max)
  mu_vals = lapply(all_vals, \(x) x$mu_vals)
  mu_int_vals = lapply(all_vals, \(x) x$mu_int_vals)
  x_vals = lapply(all_vals, \(x) x$x_vals)
  b_vals = lapply(all_vals, \(x) x$b_vals)
  dt_b_vals = lapply(all_vals, \(x) x$dt_b_vals)
  nt_vals = lapply(all_vals, \(x) x$nt_vals)


  # some temp functions for easier data handling
  temp_is_not_null = function(x) {
    return(!all(sapply(x, is.null)))
  }

  range_vals = function(x, reduce_t = F, select_indices_t = NULL) {
    as_arr = sapply(x, \(y){
      if (reduce_t)
        return(range(y[select_indices_t]))
      return(range(y))
    })
    return(c(min(as_arr), max(as_arr)))
  }


  # plot everything
  withr::local_par(mfrow = c(3, 2))
  # get the relevant time steps (for y-axis scaling)
  select_indices_t = which(t_vec >= xlim[1] & t_vec <= xlim[2])


  # plot the drift rate
  if (temp_is_not_null(mu_vals)) {
    ylim <- range_vals(mu_vals, reduce_t = T, select_indices_t)

    plot(c(1, 2) ~ c(1, 1),
      col = "white", xlim = xlim,
      ylab = "Drift Rate", xlab = "Time [s]", ylim = ylim,
      main = "mu_fun"
    )

    for (i in seq_along(conds)) {
      graphics::points(mu_vals[[conds[i]]] ~ t_vec, ty = "l",
                       col = col[i])
    }
  }

  # plot the integral of the drift rate
  if (temp_is_not_null(mu_int_vals)) {
    ylim <- range_vals(mu_int_vals, reduce_t = T, select_indices_t)

    plot(c(1, 2) ~ c(1, 1),
      col = "white", xlim = xlim,
      ylab = "Drift", xlab = "Time [s]", ylim = ylim,
      main = "mu_int_fun"
    )

    for (i in seq_along(conds)) {
      graphics::points(mu_int_vals[[conds[i]]] ~ t_vec, ty = "l",
                       col = col[i])
    }
  }

  # plot the starting condition
  if (temp_is_not_null(x_vals)) {
    ylim <- range_vals(x_vals)

    plot(c(1, 2) ~ c(1, 1),
      col = "white", xlim = c(-1, 1),
      ylab = "Density", xlab = "Evidence Value", ylim = ylim,
      main = "x_fun"
    )

    for (i in seq_along(conds)) {
      graphics::points(x_vals[[conds[i]]] ~ x_vec, ty = "l", col = col[i])
    }
  }

  # plot the boundary
  if (temp_is_not_null(b_vals)) {
    ylim <- range_vals(b_vals, reduce_t = T, select_indices_t)

    plot(c(1, 2) ~ c(1, 1),
      col = "white", xlim = xlim,
      ylab = "Boundary", xlab = "Time [s]", ylim = ylim,
      main = "b_fun"
    )

    for (i in seq_along(conds)) {
      graphics::points(b_vals[[conds[i]]] ~ t_vec, ty = "l", col = col[i])
    }
  }

  # plot the derivative of the boundary
  if (temp_is_not_null(dt_b_vals)) {
    ylim <- range_vals(dt_b_vals, reduce_t = T, select_indices_t)

    plot(c(1, 2) ~ c(1, 1),
      col = "white", xlim = xlim,
      ylab = "Derivative Boundary", xlab = "Time [s]", ylim = ylim,
      main = "dt_b_fun"
    )

    for (i in seq_along(conds)) {
      graphics::points(dt_b_vals[[conds[i]]] ~ t_vec, ty = "l", col = col[i])
    }
  }


  # plot the non-decision time
  if (temp_is_not_null(nt_vals)) {
    ylim <- range_vals(nt_vals, reduce_t = T, select_indices_t)

    plot(c(1, 2) ~ c(1, 1),
      col = "white", xlim = xlim,
      ylab = "Density", xlab = "Time [s]", ylim = ylim,
      main = "nt_fun"
    )

    for (i in seq_along(conds)) {
      graphics::points(nt_vals[[conds[i]]] ~ t_vec, ty = "l", col = col[i])
    }
  }

  graphics::legend(x = legend_pos, legend = legend, col = col, lty = 1)
}



# HELPER FUNCTIONS --------------------------------------------------------

#' Set Default Colors
#'
#' @description
#' This function assigns default colors if none are provided or adjusts the
#' color vector to match the number of conditions.
#'
#' @param colors character vector, specifying colors for conditions. If NULL,
#' `default_colors` is used.
#' @param unique_conds character vector, listing unique conditions to match
#' color assignments (only the length counts).
#' @param default_colors character vector, default colors to use if `colors` is
#' not provided.
#'
#' @return A character vector of colors, matching the length of `unique_conds`.
#'
set_default_colors <- function(colors, unique_conds, default_colors) {
  if (is.null(colors)) {
    colors <- default_colors
  } else if (length(colors) == 1) {
    colors <- rep(colors, length(unique_conds))
  } else if (length(colors) != length(unique_conds)) {
    stop("The number of colors must match the number of conditions")
  }
  return(colors)
}


#' Set Plot Limits
#'
#' @description
#' This function determines plot limits, using `default_lim` if `lim` is not
#' specified.
#'
#' @param lim numeric vector of length 2, specifying the desired plot limits.
#' @param default_lim numeric vector of length 2, default limits to use if
#' `lim` is NULL.
#'
#' @return A numeric vector of length 2, specifying the plot limits.
#'
set_plot_limits <- function(lim, default_lim) {
  if (is.null(lim)) default_lim else lim
}

