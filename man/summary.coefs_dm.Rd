% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formatting_coefs_dm.R
\name{summary.coefs_dm}
\alias{summary.coefs_dm}
\alias{print.summary.coefs_dm}
\title{Summary for \code{coefs_dm} Objects}
\usage{
\method{summary}{coefs_dm}(object, ..., round_digits = drift_dm_default_rounding())

\method{print}{summary.coefs_dm}(x, ..., show_header = TRUE)
}
\arguments{
\item{object}{an object of type \code{coefs_dm}.}

\item{...}{additional arguments passed forward.}

\item{round_digits}{integer, specifying the number of decimal places for
rounding the summary of the underlying \link{data.frame}. Default is 3.}

\item{x}{an object of class \code{summary.coefs_dm}.}

\item{show_header}{logical. If \code{TRUE}, a header specifying the type of
statistic will be displayed.}
}
\value{
For \code{summary.coefs_dm()} a summary object of class \code{summary.coefs_dm}.

For \code{print.summary.coefs_dm()}, the supplied object is returned
invisibly.
}
\description{
Summary and corresponding printing methods for \code{coefs_dm} objects. These
objects result from a call to \code{\link[=coef.fits_ids_dm]{coef.fits_ids_dm()}} (i.e., when
calling \code{coef()} with an object of type \code{fits_ids_dm}).
}
\details{
\code{summary.coefs_dm()} summarizes \code{coefs_dm} objects, returning the type,
a summary of the underlying \link{data.frame} (\code{summary_dataframe}), and the
number of unique IDs (\code{n_ids}).
}
\examples{
# get a fits_ids object for demonstration purpose
fits_ids <- get_example_fits_ids()
coefs <- coef(fits_ids)
summary(coefs)

}
