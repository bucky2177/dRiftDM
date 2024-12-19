# FOR DRIFT_DM OBJECTS ----------------------------------------------------


#' Get the Number of Observations for a drift_dm Object
#'
#' This method retrieves the total number of observations in the `obs_data`
#' list of a `drift_dm` object.
#'
#' @param object a [dRiftDM::drift_dm] object, which contains the observed data
#' in `object$obs_data`.
#' @param ... additional arguments
#'
#' @return An integer representing the total number of observations across
#' all conditions in `object$obs_data`.
#'
#' @details The function iterates over each element in `object$obs_data`, counts
#' the entries in each nested component, and returns the cumulative sum as the
#' total observation count.
#'
#' It was written to provide an `nobs` method for calculating the log-likelihood
#' ([dRiftDM::logLik]), AIC ([stats::AIC]), and BIC ([stats::BIC]) statistics
#' for objects of type [dRiftDM::drift_dm].
#'
#' @importFrom stats nobs
#'
#' @examples
#' # get a pre-built model and data set for demonstration purpose
#' a_model <- dmc_dm()
#' obs_data(a_model) <- dmc_synth_data
#'
#' # then get the number of observations by accessing the model
#' nobs(a_model)
#'
#' # same number of observations as in the original data set
#' nrow(dmc_synth_data)
#'
#' @export
nobs.drift_dm <- function(object, ...) {
  return(sum(sapply(object$obs_data, lengths)))
}


#' Extract Log-Likelihood for a drift_dm Object
#'
#' This method extracts the log-likelihood for a `drift_dm` object, ensuring
#' data is available and evaluating the model if necessary.
#'
#' @param object a [dRiftDM::drift_dm] object containing observed data
#' @param ... additional arguments
#'
#' @return A `logLik` object containing the log-likelihood value for the
#' [dRiftDM::drift_dm] object. This value has attributes for the number of
#' observations (`nobs`) and the number of model parameters (`df`).
#'
#' Returns `NULL` if observed data is not available.
#'
#' @importFrom stats logLik
#'
#' @examples
#' # get a pre-built model and a data set for demonstration purpose
#' # (when creating the model, set the discretization to reasonable values)
#' a_model <- dmc_dm(t_max = 1.5, dx = .0025, dt = .0025)
#' obs_data(a_model) <- dmc_synth_data
#'
#' # calculate the log-likelihood
#' logLik(a_model)
#'
#' @export
logLik.drift_dm <- function(object, ...) {
  # check if data is supplied and maybe re_evaluate
  if (is.null(object$obs_data)) {
    warning("No data in model. Returning NULL")
    return(NULL)
  }
  if (is.null(object$log_like_val)) {
    object <- re_evaluate_model(object)
  }

  val <- object$log_like_val
  class(val) <- "logLik"
  attr(val, "nobs") <- nobs(object)
  attr(val, "df") <- get_number_prms(object$flex_prms_obj)
  return(val)
}


#' Convenient Coefficients Access
#'
#' Extract or set the coefficients/parameters of [dRiftDM::drift_dm] or
#' `fits_ids_dm` objects
#'
#' @param object an object of type [dRiftDM::drift_dm] or `fits_ids_dm`
#'  (see [dRiftDM::load_fits_ids]).
#'
#' @param ... additional arguments passed to the respective method
#'
#' @param eval_model logical, indicating if the model should be re-evaluated or
#'  not when updating the parameters (see [dRiftDM::re_evaluate_model]).
#'  Default is `FALSE`.
#'
#' @param select_unique logical, indicating if only those parameters shall be
#'  returned that are considered unique (e.g., when a parameter is set to be
#'  identical across three conditions, then the parameter is only returned once).
#'  Default is `TRUE`. This will also return only those parameters that are
#'  estimated.
#' @param value numerical, a vector with valid values to update the model's
#' parameters. Must match with the number of (unique and free) parameters.
#'
#' @details
#' `coef()` are methods for the generic `coef` function; `coefs<-()` is a
#'  generic replacement function, currently supporting objects of type
#'  [dRiftDM::drift_dm].
#'
#'  The argument `value` supplied to the `coefs<-()` function must match with
#'  the vector returned from `coef(<object>)`. It is possible to
#'  update just part of the (unique) parameters.
#'
#'  Whenever the argument `select_unique = TRUE`, dRiftDM tries to provide
#'  unique parameter labels.
#'
#' @returns
#'  For objects of type [dRiftDM::drift_dm], `coefs()` returns either a named
#'  numeric vector for `select_unique = TRUE`, or the `prms_matrix` matrix for
#'  `select_unique = FALSE`. If custom parameters exist, they are added to the
#'  matrix.
#'
#'  For objects of type `fits_ids_dm`, `coefs()` returns a [data.frame]. If
#'  `select_unique = TRUE`, the columns will be the (unique, free) parameters,
#'  together with a column coding `IDs`. If `select_unique = FALSE`, the columns
#'  will be the parameters as listed in the columns of `prms_matrix` (see
#'  [dRiftDM::drift_dm]), together with columns coding the conditions and
#'  `IDs`. The returned [data.frame] has the class label `coefs_dm` to easily
#'  plot histograms for each parameter (see [dRiftDM::hist.coefs_dm]).
#'
#' @seealso [dRiftDM::drift_dm()]
#'
#' @importFrom stats coef
#'
#' @examples
#' # get a pre-built model and a data set for demonstration purpose
#' # (when creating the model, set the discretization to reasonable values)
#' a_model <- dmc_dm(t_max = 1.5, dx = .0025, dt = .0025)
#' coef(a_model) # gives the free and unique parameters
#' coef(a_model, select_unique = FALSE) # gives the entire parameter matrix
#'
#' @export
coef.drift_dm <- function(object, ..., select_unique = TRUE) {
  # if unique, get labels for prm cond combos, extract the respective
  # values from the parameter matrix, and simplify parameter.cond labels
  # if cond is the same
  if (select_unique) {
    prms_cond_combo <- prms_cond_combo(object)
    prm_matrix <- object$flex_prms_obj$prms_matrix
    prms <- sapply(1:ncol(prms_cond_combo), function(idx) {
      prm <- prms_cond_combo[1, idx]
      cond <- prms_cond_combo[2, idx]
      prm_matrix[cond, prm]
    })
    names(prms) <- prm_cond_combo_2_labels(prms_cond_combo)
    return(prms)
  } else {
    prms_matrix <- object$flex_prms_obj$prms_matrix
    cust_prms_matrix <- lapply(
      object$flex_prms_obj$cust_prms$values,
      \(x) return(x)
    )
    cust_prms_matrix <- do.call(cbind, cust_prms_matrix)
    all_prms <- cbind(prms_matrix, cust_prms_matrix)
    return(all_prms)
  }
}


# FOR FITS_IDS_DM ---------------------------------------------------------


#' Extract Model Statistics for fits_ids_dm Object
#'
#' These methods are wrappers to extract specific model fit statistics
#' (log-likelihood, AIC, BIC) for each model in a `fits_ids_dm` object.
#'
#' @param object a `fits_ids_dm` object (see [dRiftDM::estimate_model_ids])
#' @param k numeric; penalty parameter for the AIC calculation.
#' Defaults to 2 (standard AIC).
#' @param ... additional arguments
#'
#' @return A data.frame containing the respective statistic in one column (named
#' `Log_Like`, `AIC`, or `BIC`) and a corresponding `ID` column.
#'
#' @details
#'
#' Each function retrieves the relevant statistics by calling
#'  [dRiftDM::calc_stats] with `type = "fit_stats"` and selects the columns
#'  for `ID` and the required statistic.
#'
#'
#' @examples
#' # get an auxiliary fits_ids object for demonstration purpose;
#' # such an object results from calling load_fits_ids
#' all_fits <- get_example_fits_ids()
#'
#' # AICs
#' AIC(all_fits)
#'
#' # BICs
#' BIC(all_fits)
#'
#' # Log-Likelihoods
#' logLik(all_fits)
#'
#' # All unique and free parameters
#' coef(all_fits)
#'
#' # Or all parameters across all conditions
#' coef(all_fits, select_unique = FALSE)
#'
#' @seealso [stats::AIC()], [stats::BIC()], [dRiftDM::logLik.drift_dm]
#'
#' @export
logLik.fits_ids_dm <- function(object, ...) {
  stats <- calc_stats(object, type = "fit_stats")
  return(stats[c("ID", "Log_Like")])
}

#' @rdname logLik.fits_ids_dm
#' @importFrom stats AIC
#' @export
AIC.fits_ids_dm <- function(object, ..., k = 2) {
  stats <- calc_stats(object, type = "fit_stats", k = k)
  return(stats[c("ID", "AIC")])
}

#' @rdname logLik.fits_ids_dm
#' @importFrom stats BIC
#' @export
BIC.fits_ids_dm <- function(object, ...) {
  stats <- calc_stats(object, type = "fit_stats")
  return(stats[c("ID", "BIC")])
}

#' @rdname coef.drift_dm
#' @export
coef.fits_ids_dm <- function(object, ...) {
  all_coefs <- lapply(object$all_fits, function(x) coef(x, ...))
  # special treatment of the ID, because coef might return a matrix or vector
  all_coefs <- lapply(names(all_coefs), function(x) {
    one_coef <- all_coefs[[x]]
    if (is.vector(one_coef)) {
      one_coef <- t(as.matrix(one_coef))
      return_val <- data.frame(ID = x, one_coef)
    } else {
      return_val <- cbind(
        ID = x, Cond = rownames(one_coef),
        data.frame(one_coef)
      )
    }
    return(return_val)
  })
  all_coefs <- do.call("rbind", all_coefs)
  rownames(all_coefs) <- NULL
  all_coefs$ID <- try_cast_integer(all_coefs$ID)
  all_coefs <- all_coefs[order(all_coefs$ID), ]
  rownames(all_coefs) <- NULL
  class(all_coefs) <- c("coefs_dm", "data.frame")
  return(all_coefs)
}



# HELPER FUNCTION ---------------------------------------------------------

#' Convert Character Digits to Numeric Digits
#'
#' This internal function casts a character vector to integer, if the character
#' vector only contains digits for entries. If the input vector is not of
#' type character or if any entry contains a non-digit, then the vector is
#' returned unmodified.
#'
#' @param values a vector of values to attempt conversion to integer.
#' @return an integer vector if conversion succeeds; otherwise, the original
#' vector.
#'
#' @keywords internal
try_cast_integer <- function(values) {
  if (!is.character(values)) {
    return(values)
  }

  checks <- !grepl("\\D", values) # check each entry if only digits exist

  # if each entry only contains digits, then cast to digit
  if (all(checks)) {
    values <- as.integer(values)
  }

  return(values)
}
