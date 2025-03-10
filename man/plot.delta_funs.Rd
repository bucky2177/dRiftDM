% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plot.delta_funs}
\alias{plot.delta_funs}
\title{Plot Delta Functions}
\usage{
\method{plot}{delta_funs}(
  x,
  ...,
  dv = NULL,
  col = NULL,
  xlim = NULL,
  ylim = NULL,
  xlab = "RT [s]",
  ylab = expression(Delta),
  pch = 21,
  lty = 1,
  type = "l",
  legend = NULL,
  legend_pos = "topright"
)
}
\arguments{
\item{x}{a \link{data.frame}, containing delta functions, typically resulting from
a call to \link{calc_stats}.}

\item{...}{additional arguments passed to the \link{plot}, \link[graphics:points]{graphics::points},
and \link[graphics:legend]{graphics::legend} functions. Oftentimes, this will (unfortunately) lead
to an error due to a clash of arguments.}

\item{dv}{character vector, specifying the delta functions to plot. Defaults
to all columns beginning with "Delta_" in \code{x}.}

\item{col}{character vector, specifying colors for each delta function. If a
single color is provided, it will be repeated for each function.}

\item{xlim, ylim}{numeric vectors of length 2, specifying the x and y axis
limits.}

\item{xlab, ylab}{character, labels for the x and y axes.}

\item{pch}{integer, specifying the plotting symbol for observed data points.}

\item{lty}{integer, line type for the predicted delta functions.}

\item{type}{character, type of plot for the predicted delta functions.}

\item{legend}{character vector, specifying legend labels corresponding to
the delta functions. Defaults to the way functions were derived.}

\item{legend_pos}{character, specifying the position of the legend on the
plot.}
}
\value{
Nothing (\code{NULL}; invisibly)
}
\description{
This function generates a plot of delta functions, displaying observed and
predicted values, which can be useful for evaluating model fit or exploring
data characteristics.

If the data contains multiple IDs, delta functions are aggregated across IDs
before plotting.
}
\details{
The \code{plot.delta_funs} function provides an easy way to investigate delta
functions, allowing for customization in color, symbols, and line types for
different data sources (observed vs. predicted). If multiple IDs are present
in the data, delta functions are aggregated across IDs before plotting.
By default, \code{ylim} is set to twice the range of the delta values to provide
more context.
}
\examples{
# Example 1: Only model predictions ---------------------------------------
# get a delta function data.frame for demonstration purpose
a_model <- dmc_dm(t_max = 1.5, dt = .0025, dx = .0025)
deltas <- calc_stats(
  a_model,
  type = "delta_funs",
  minuends = "incomp",
  subtrahends = "comp"
)

# call the plot function with default values
plot(deltas)

# modify the plot
plot(deltas,
  col = c("black"),
  lty = 2,
  xlim = c(0.2, 0.65)
)

# Example 2: Model predictions and observed data --------------------------
obs_data(a_model) <- dmc_synth_data
deltas <- calc_stats(
  a_model,
  type = "delta_funs",
  minuends = "incomp",
  subtrahends = "comp"
)
plot(deltas)
# Note: The model was not fitted to the data set, thus observed data and
# model predictions don't match


# Example 3: Only observed data -------------------------------------------
deltas <- calc_stats(
  dmc_synth_data,
  type = "delta_funs",
  minuends = "incomp",
  subtrahends = "comp"
)
plot(deltas)

}
