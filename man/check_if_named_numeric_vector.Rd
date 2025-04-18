% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{check_if_named_numeric_vector}
\alias{check_if_named_numeric_vector}
\title{Check if Object is a Named Numeric Vector}
\usage{
check_if_named_numeric_vector(
  x,
  var_name,
  labels = NULL,
  length = NULL,
  allow_non_word_chars = FALSE
)
}
\arguments{
\item{x}{numeric vector, expected to be named.}

\item{var_name}{character, the name of the variable to display in error
messages.}

\item{labels}{character vector, optional, specifying valid names for \code{x}. If
provided, all names in \code{x} must match these labels.}

\item{length}{integer, optional, specifying the exact required length of \code{x}.}

\item{allow_non_word_chars}{logical, whether to permit non-word characters in
names (default is \code{FALSE}).}
}
\value{
Throws an error if the conditions are not met. If all checks pass,
no output is returned.
}
\description{
Validates that an object is a named numeric vector with specified attributes.
Optionally checks specific names, length, and restrictions on label
characters.
}
\details{
Checks for:
\itemize{
\item Numeric type of \code{x} with non-zero length
\item Required length, if specified
\item Unique, non-empty names for each entry in \code{x}
\item Match of all names in \code{x} to \code{labels}, if \code{labels} is specified
\item Absence of \code{NA} of \code{Inf} values in \code{x}
\item Optional absence of non-word names if \code{allow_non_word_chars} is FALSE
}
}
\keyword{internal}
