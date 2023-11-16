#' A simulated data set with one condition
#'
#' This dataset was simulated by using the classical ratcliff diffusion model
#' (see  [dRiftDM::ratcliff_dm()]).
#' It will be used for creating examples in the dRiftDM package...
#'
#' @format ## `ratcliff_data`
#' A data frame with 300 rows and 3 columns:
#' \describe{
#'   \item{RT}{Response Times}
#'   \item{Error}{Error Coding (Error Response = 1; Correct Response = 0)}
#'   \item{Cond}{Condition ('null')}
#' }
"ratcliff_data"


#' A simulated data set with two conditions
#'
#' This dataset was simulated by using the diffusion model for conflict tasks
#' (see  [dRiftDM::dmc_dm()]) with parameter settings that are typical for a
#' Simon task.
#' It will be used for creating examples in the dRiftDM package...
#'
#' @format ## `ratcliff_data`
#' A data frame with 300 rows and 3 columns:
#' \describe{
#'   \item{RT}{Response Times}
#'   \item{Error}{Error Coding (Error Response = 1; Correct Response = 0)}
#'   \item{Cond}{Condition ('comp' and 'incomp')}
#' }
"simon_data"
