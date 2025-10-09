#' A synthetic data set with one condition
#'
#' This dataset was simulated by using the classical Ratcliff diffusion model
#' (see  [dRiftDM::ratcliff_dm()]).
#'
#' @format
#' A data frame with 300 rows and 3 columns:
#' \describe{
#'   \item{RT}{Response Times}
#'   \item{Error}{Error Coding (Error Response = 1; Correct Response = 0)}
#'   \item{Cond}{Condition ('null')}
#' }
"ratcliff_synth_data"


#' A synthetic data set with two conditions
#'
#' This dataset was simulated by using the Diffusion Model for Conflict tasks
#' (see [dRiftDM::dmc_dm()]) with parameter settings that are typical for a
#' Simon task.
#'
#' @format
#' A data frame with 600 rows and 3 columns:
#' \describe{
#'   \item{RT}{Response Times}
#'   \item{Error}{Error Coding (Error Response = 1; Correct Response = 0)}
#'   \item{Cond}{Condition ('comp' and 'incomp')}
#' }
"dmc_synth_data"


#' A synthetic data set with two conditions
#'
#' This dataset was simulated by using the Shrinking Spotlight Model
#' (see [dRiftDM::ssp_dm()]) with parameter settings that are typical for a
#' Flanker task.
#'
#' @format
#' A data frame with 600 rows and 3 columns:
#' \describe{
#'   \item{RT}{Response Times}
#'   \item{Error}{Error Coding (Error Response = 1; Correct Response = 0)}
#'   \item{Cond}{Condition ('comp' and 'incomp')}
#' }
"ssp_synth_data"





#' Exemplary Simon Data
#'
#' Data of the Simon task collected in the course of the study by
#' \insertCite{Ulrichetal.2015;textual}{dRiftDM}.
#'
#' @format
#' A data.frame with 16 individuals and the following columns:
#' \describe{
#'   \item{ID}{Individual IDs}
#'   \item{RT}{Response Times}
#'   \item{Error}{Error Coding (Error Response = 1; Correct Response = 0)}
#'   \item{Cond}{Condition ('comp' and 'incomp')}
#' }
"ulrich_simon_data"



#' Exemplary Flanker Data
#'
#' Data of the Flanker task collected in the course of the study by
#' \insertCite{Ulrichetal.2015;textual}{dRiftDM}.
#'
#' @format
#' A data.frame with 16 individuals and the following columns:
#' \describe{
#'   \item{ID}{Individual IDs}
#'   \item{RT}{Response Times}
#'   \item{Error}{Error Coding (Error Response = 1; Correct Response = 0)}
#'   \item{Cond}{Condition ('comp' and 'incomp')}
#' }
"ulrich_flanker_data"
