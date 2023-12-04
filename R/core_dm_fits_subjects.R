# === FOR WORKING ON OBJECTS OF TYPE dm_fits_subjects

#' Get model values across subjects
#'
#'  `gather_*` functions iterate over the individual model fits in
#'  `fits_subjects`, returning a data.frame of the respective parameters/values
#'
#'
#' @param fits_subjects an object inheriting from `dm_fits_subjects`, see
#'  [dRiftDM::load_fits_subjects]
#'
#'  @return
#'  `gather_parameters` returns the model parameters, the log-likelihood, and
#'  AIC/BIC values as a data.frame
#'
#' @export
gather_parameters <- function(fits_subjects) {
  if (!inherits(fits_subjects, "dm_fits_subjects")) {
    stop("fits_subjects not of type dm_fits_subjects")
  }

  prms <- sapply(fits_subjects$all_fits, function(x) {
    prms_one_model <- x$prms_model
    prms_one_model <- c(prms_one_model, log_like = x$log_like_val, x$ic_vals)
    return(prms_one_model)
  })
  prms <- t(prms)
  prms <- as.data.frame(prms)
  prms <- cbind(Subject = rownames(prms), prms)
  rownames(prms) <- NULL
  return(prms)
}
