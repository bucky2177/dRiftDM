#' @importFrom stats BIC
#' @importFrom stats AIC



#' @export
logLik.drift_dm <- function(object, ...) {
  return(object$log_like_val)
}

#' @export
AIC.drift_dm <- function(object, ...) {
  if (is.null(object$ic_vals)) {
    return(NULL)
  }
  return(object$ic_vals[["aic"]])
}

#' @export
BIC.drift_dm <- function(object, ...) {
  if (is.null(object$ic_vals)) {
    return(NULL)
  }
  return(object$ic_vals[["bic"]])
}

#' @export
coef.drift_dm <- function(object, ...) {
  return(object$prms_model)
}
