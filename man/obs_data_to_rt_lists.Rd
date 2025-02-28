% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core_dm.R
\name{obs_data_to_rt_lists}
\alias{obs_data_to_rt_lists}
\title{Disassemble an Observed Data set}
\usage{
obs_data_to_rt_lists(obs_data, b_coding = NULL)
}
\arguments{
\item{obs_data}{a data.frame wth columns RT, Cond, and \code{column} matching
\code{b_coding}}

\item{b_coding}{a boundary coding list (see \link{b_coding})}
}
\value{
a list of rts with entries
\itemize{
\item rts_u -> containing a list of numeric vectors, with names according to the
values in Cond
\item rts_l -> containing a list of numeric vectors, with names according to the
values in Cond
}
}
\description{
Takes a data.frame with columns RT, Cond, and \code{column} matching with
\link{b_coding}, and disassembles it into a list of rts
}
\details{
performs checks on \code{b_coding} (\link{check_b_coding}) and
\code{obs_data} (\link{check_raw_data}) before disassembling the data set.
}
\keyword{internal}
