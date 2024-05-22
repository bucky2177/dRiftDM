#'@export
logLik.dm_fits_ids <- function(object, ...) {
  values = gather_parameters(object)
  return(values[c("ID", "log_like")])
}

#'@export
AIC.dm_fits_ids <- function(object, ...) {
  values = gather_parameters(object)
  return(values[c("ID", "aic")])
}

#'@export
BIC.dm_fits_ids <- function(object, ...) {
  values = gather_parameters(object)
  return(values[c("ID", "bic")])
}

#'@export
coef.dm_fits_ids <- function(object, ...) {
  values = gather_parameters(object)
  all_prms = names(object$drift_dm_fit_info$drift_dm_obj$prms_model)
  return(values[c("ID", all_prms)])
}
