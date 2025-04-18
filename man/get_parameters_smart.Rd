% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_parameters_smart}
\alias{get_parameters_smart}
\title{Turn default/special parameter specifications to vectors}
\usage{
get_parameters_smart(
  drift_dm_obj,
  input_a,
  input_b,
  labels = TRUE,
  is_l_u = TRUE
)
}
\arguments{
\item{drift_dm_obj}{an object of type drift_dm}

\item{input_a, input_b}{either a atomic vector or list (see
\link{create_matrix_smart})}

\item{labels}{optional logical, if \code{TRUE}, then the returned vectors have
the unique parameter labels according to \link{prm_cond_combo_2_labels}.}

\item{is_l_u}{optional logical, if \code{TRUE}, a warning is thrown when
\code{input_a} leads to larger values than \code{input_b}. Useful when \code{input_a} and
\code{input_b} span a (search) space.}
}
\value{
a list with two vectors named \code{vec_a/vec_b}. Usually, those will be
the search space or simulation space. The length and names (if requested)
matches with coef(model, select_unique = TRUE).
}
\description{
The function is used in the depths to map parameter inputs to the parameters
of a model. Most of the time, it is used to get the search space as
a vector, matching with the free parameters of a model.
Only relevant when users use the "default parameters" approach where they
only specify the parameter labels and assume the package figures out
how each parameter relates across conditions (see \link{simulate_data}).
This comes in handy, when freeing a parameter across conditions, while the
search space remains the same (otherwise, a user would always have to adapt
the vectors for lower/upper to match with \link{x2prms_vals})
}
\details{
The function first gets all unique parameters across conditions using
\link{prms_cond_combo}. The unique parameter labels are then forwarded
to \link{create_matrix_smart}, together with all (!) the conditions in the
model and the \code{input_a}/\code{input_b} arguments. Subsequently, the created matrices
are wrangled into vectors in accordance with \link{prms_cond_combo}. The
vectors are then passed back.
}
\keyword{internal}
