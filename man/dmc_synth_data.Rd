% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{dmc_synth_data}
\alias{dmc_synth_data}
\title{A synthetic data set with two conditions}
\format{
A data frame with 600 rows and 3 columns:
\describe{
\item{RT}{Response Times}
\item{Error}{Error Coding (Error Response = 1; Correct Response = 0)}
\item{Cond}{Condition ('comp' and 'incomp')}
}
}
\usage{
dmc_synth_data
}
\description{
This dataset was simulated by using the Diffusion Model for Conflict tasks
(see  \code{\link[=dmc_dm]{dmc_dm()}}) with parameter settings that are typical for a
Simon task.
}
\keyword{datasets}
