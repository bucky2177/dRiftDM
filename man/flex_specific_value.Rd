% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core_flex_prms.R
\name{flex_specific_value}
\alias{flex_specific_value}
\title{Set a specific value to the parameter matrix (internal docu)}
\usage{
flex_specific_value(flex_prms_obj, formula_instr)
}
\arguments{
\item{flex_prms_obj}{flex_prms object}

\item{formula_instr}{a string referring to "set"
(see \link{modify_flex_prms})}
}
\value{
an updated flex_prms object with a modified prms_matrix object, and
(if applicable) a modified cust_prms matrix
}
\description{
This function takes a flex_prms_obj and sets certain values to the parameter
matrix, based on the given instruction string (i.e., ' prm ~ conda => 0.3)
}
\keyword{internal}
