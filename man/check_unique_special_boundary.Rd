% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{check_unique_special_boundary}
\alias{check_unique_special_boundary}
\title{Check for Unique Special Boundary Values}
\usage{
check_unique_special_boundary(drift_dm_obj, l_u)
}
\arguments{
\item{drift_dm_obj}{an object of type \link{drift_dm}}

\item{l_u}{a list specifying the upper/lower parameter/search space (see
\link{simulate_data}, or \link{estimate_model}).}
}
\description{
Internal, deep in the depths of the package, function. Verifies that each
specified parameter value within a condition in \code{l_u} is unique within
the \code{linear_internal_list} in \code{drift_dm_obj}. If the same
value is associated with multiple conditions, an error is raised. Used for
checking the input to \link{get_lower_upper_smart}.
}
\details{
For each condition in \code{l_u}, the function examines if the parameter
value specified is unique with respect to the \code{linear_internal_list}.
Non-unique values for a parameter-condition combination raise an error.
}
\keyword{internal}
