% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plot_stats}
\alias{plot_stats}
\title{Plot Statistics}
\usage{
plot_stats(obj, type, source = "both", mfrow = NULL, ...)
}
\arguments{
\item{obj}{an object inheriting from \link{drift_dm} or
\code{dm_fits_ids} (see \link{load_fits_ids}). If \code{obj} is of the
latter type, statistics may be calculated via
\link{gather_stats}, depending on the requested statistics. For
quantiles, delta functions, and CAfs, observed data and model predictions
are averaged across individuals.}

\item{type}{character vector, indicating which statistics should be
plotted (see \link{calc_stats} for more info).}

\item{source}{character, indicating whether the model prediction ("pred"),
the observed data ("obs"), or both ("both") should be considered. Default is
"both".}

\item{mfrow}{optional numeric vector, controls the number of rows and columns
of the final plot in case \code{type} is of length > 1. Default is NULL, meaning
that plots will not be grouped.}

\item{...}{optional and mandatory arguments passed down to internal functions
creating the plots. For a full list of arguments see the details.}
}
\description{
This function provides basic plots of statistics derived from a model.
Internally, it calls \link{gather_stats} or \link{calc_stats} and
subsequently creates basic plots.
}
\details{
For more information on each statistics, see \link{calc_stats}
}
\section{Conditional Accuracy Functions (CAFs)}{
Optional arguments
\itemize{
\item \code{n_bins_cafs}: the number of bins for which to calculate accuracy
\item \code{x_lab_cafs}, \code{y_lab_cafs}, \code{x_lim_cafs}, \code{y_lim_cafs}: axes labels and limits
\item \code{line_cols_cafs}: a character vector defining colors for each condition
}
}

\section{Quantiles}{
Optional arguments
\itemize{
\item \code{probs_quantiles}: the probabilities for which quantiles to compute
\item \code{dv_quantiles}: The dependent variable to plot; default are quantiles
associated with the upper boundary
\item \code{x_lab_quantiles}, \code{y_lab_quantiles}, \code{x_lim_quantiles},
\code{y_lim_quantiles}: axes labels and limits
\item \code{line_cols_quantiles}: a character vector defining colors for each condition
}
}

\section{Delta Functions}{
Mandatory arguments
\itemize{
\item \code{minuends_deltas}, \code{subtrahends_deltas}: character vectors specifying how to
calculate delta functions
}

Optional arguments
\itemize{
\item \code{probs_deltas}: the probabilities for which quantiles to calculate
\item \code{dvs_deltas}: The dependent variable to plot; default are quantiles
associated with the upper boundary
\item \code{x_lab_deltas}, \code{y_lab_deltas}, \code{x_lim_deltas}, \code{y_lim_deltas}: axes labels
and limits
\item \code{line_cols_deltas}: a character vector defining colors for each line
}
}

