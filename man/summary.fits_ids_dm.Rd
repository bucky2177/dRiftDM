% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formatting_fits_ids_dm.R
\name{print.summary.fits_ids_dm}
\alias{print.summary.fits_ids_dm}
\alias{summary.fits_ids_dm}
\title{Summary and Printing for fits_ids_dm Objects}
\usage{
\method{print}{summary.fits_ids_dm}(x, ..., round_digits = drift_dm_default_rounding())

\method{summary}{fits_ids_dm}(object, ...)
}
\arguments{
\item{x}{an object of class \code{summary.fits_ids_dm}.}

\item{...}{additional arguments}

\item{round_digits}{integer, specifying the number of decimal places for
rounding in the printed summary. Default is set to 3.}

\item{object}{an object of class \code{fits_ids_dm}, generated by a call
to \link{load_fits_ids}.}
}
\value{
\code{summary.fits_ids_dm()} returns a list of class \code{summary.fits_ids_dm} (see
the Details section summarizing each entry of this list).

\code{print.summary.fits_ids_dm()} returns invisibly the \code{summary.fits_ids_dm}
object.
}
\description{
Methods for summarizing and printing objects of the class \code{fits_ids_dm},
which contain multiple fits across individuals.
}
\details{
The \code{summary.fits_ids_dm} function creates a summary object containing:
\itemize{
\item \strong{fit_procedure_name}: The name of the fit procedure used.
\item \strong{time_call}: Timestamp of the last fit procedure call.
\item \strong{lower} and \strong{upper}: Lower and upper bounds of the search space.
\item \strong{model_type}: Description of the model type, based on class information.
\item \strong{prms}: All parameter values across all conditions (essentially a call
to coef() with the argument select_unique = FALSE).
\item \strong{stats}: A named list of matrices for each condition, including mean and
standard error for each parameter.
\item \strong{N}: The number of individuals.
}

The \code{print.summary.fits_ids_dm} function displays the summary object in a
formatted manner.
}
\examples{
# get an auxiliary object of type fits_ids_dm for demonstration purpose
all_fits <- get_example_fits_ids()
sum_obj <- summary(all_fits)
print(sum_obj, round_digits = 2)

}
