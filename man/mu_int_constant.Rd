% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models.R
\name{mu_int_constant}
\alias{mu_int_constant}
\title{Integral of Constant Drift Rate}
\usage{
mu_int_constant(prms_model, prms_solve, t_vec, one_cond, ddm_opts)
}
\arguments{
\item{prms_model}{the model parameters, containing muc}

\item{prms_solve}{solver settings}

\item{t_vec}{time space}

\item{one_cond}{one condition}

\item{ddm_opts}{optional arguments attached to an object}
}
\value{
a vector calculated as t_vec*muc
}
\description{
Integral of Constant Drift Rate
}
\keyword{internal}
