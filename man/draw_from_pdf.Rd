% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core_tools.R
\name{draw_from_pdf}
\alias{draw_from_pdf}
\title{Draw Samples Using Inverse Transform Sampling}
\usage{
draw_from_pdf(a_pdf, x_def, k, seed = NULL)
}
\arguments{
\item{a_pdf}{a numeric vector representing the PDF values.}

\item{x_def}{a numeric vector defining the domain (or x-values) corresponding
to the values in \code{a_pdf}. The vector \code{x_def} must be sorted in increasing
order.}

\item{k}{a single integer specifying the number of samples to generate.}

\item{seed}{an optional single integer value used to set the seed for random
number generation, allowing for reproducibility of results.}
}
\value{
A numeric vector of length \code{k} containing the sampled values from
the specified PDF. If \code{k} is 0, an empty numeric vector is returned.
}
\description{
\code{draw_from_pdf} generates samples from a given probability density function
(PDF) using inverse transform sampling. This function takes in a numeric PDF
vector and a corresponding domain vector, then returns a specified number
of samples.
}
\details{
This function implements inverse transform sampling by first constructing a
cumulative distribution function (CDF) from the given PDF. A uniform random
variable is then sampled for each of the \code{k} samples, and the corresponding
value in \code{x_def} is selected by locating the appropriate interval in the CDF.
}
\keyword{internal}
