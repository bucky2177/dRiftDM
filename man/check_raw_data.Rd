% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core_dm.R
\name{check_raw_data}
\alias{check_raw_data}
\title{Check the Observed Data}
\usage{
check_raw_data(obs_data, b_coding_column, u_value, l_value)
}
\arguments{
\item{obs_data}{a \link{data.frame}}

\item{b_coding_column}{a single string, indicating which column of \code{obs_data}
indicates how each RT corresponds to the boundaries.}

\item{u_value, l_value}{the value within the \code{b_coding_column} column that
specifies the upper/lower boundary}
}
\value{
the \code{obs_data} for convenience (with edits as listed under Details).
}
\description{
Checks a data set that is considered an "observed data set". Used in the
internals of dRiftDM.
}
\details{
Checks:
\itemize{
\item if \code{obs_data} is a data.frame
\item For missing Values, and drops rows with missing values
\item if "RT", \code{b_coding_column}, and "Cond" column are present
\item if "Cond" is of type character, and if not casts it to character
\item if RT is of type numeric, and of not casts it to numeric
\item RTs are >= 0
\item that the values in \code{b_coding_column} match with u_value and l_value
(casts the column if necessary)
\item if \code{b_coding_column} has only 1 or 2 unique values
\item When IDs are present, if each ID has values on each condition. At the same
time unused factor levels are dropped \link{drop_levels_ID_column}
}
}
\keyword{internal}
