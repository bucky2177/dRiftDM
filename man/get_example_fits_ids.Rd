% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_example_fits_ids}
\alias{get_example_fits_ids}
\title{Auxiliary Function to create a fits_ids object}
\usage{
get_example_fits_ids()
}
\value{
An object of type \code{fits_ids_dm}, mimicking a result from calling
\code{\link[=load_fits_ids]{load_fits_ids()}}.
}
\description{
This function is merely a helper function to create an object of type
\code{fits_ids_dm.} It is used for example code.
}
\details{
The returned fit object comprises DMC (see \code{\link[=dmc_dm]{dmc_dm()}}) fitted to
three participants of the ulrich_flanker_data.
}
\examples{
fits <- get_example_fits_ids()

}
