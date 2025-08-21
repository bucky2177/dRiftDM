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
#' This method extracts the log-likelihood for a `drift_dm` object if the cost
#' function is a log-likelihood function and if the cost value is available.
#'
#' @param object a [dRiftDM::drift_dm] object containing observed data
#' @param ... additional arguments
#'
#' @return A `logLik` object containing the log-likelihood value for the
#' [dRiftDM::drift_dm] object. This value has attributes for the number of
#' observations (`nobs`) and the number of model parameters (`df`).
#'
#' Returns `NULL` if the cost function is not a (log-)likelihood or if the
#' cost value is not available (e.g., when the model has not yet been
#' evaluated).
#'
#' @importFrom stats logLik
#'
#' @examples
#' # get a pre-built model and a data set for demonstration purpose
#' # (when creating the model, set the discretization to reasonable values)
#' a_model <- dmc_dm(t_max = 1.5, dx = .01, dt = .005)
#' obs_data(a_model) <- dmc_synth_data
#'
#' # this returns NULL because the model has not yet been evaluated!
#' logLik(a_model)
#'
#' # evaluate the model
#' a_model <- re_evaluate_model(a_model)
#' logLik(a_model)
#'
#' @export
logLik.drift_dm <- function(object, ...) {

  # check if the cost_function is "neg_log_like"
  if (object$cost_function != "neg_log_like") {
    warning("The currently set cost_function of the model is not a ",
            "(log)-likelihood. Returning NULL.")
    return(NULL)
  }

  # extract cost value (if NULL return NULL)
  val <- cost_value(object)
  if (is.null(val)) {
    return(NULL)
  }

  # otherwise proceed with creating the log-likelihood object
  val <- val * -1.0 # turn the negative log-likelihood to a log-likelihood
  class(val) <- "logLik"
  attr(val, "nobs") <- nobs(object)
  attr(val, "df") <- get_number_prms(object$flex_prms_obj)
  return(val)
}


#' Access Coefficients of a Model
#'
#' Extract or set the coefficients/parameters objects supported by [dRiftDM].
#'
#' @param object an object of type [dRiftDM::drift_dm], `fits_agg_dm`,
#' `fits_ids_dm` (see also [dRiftDM::estimate_dm()]), or `mcmc_dm`.
#'
#' @param ... additional arguments passed forward (to `coef.drift_dm()` for
#' objects of type `fits_agg_dm`; to `.f` for objects of type `mcmc_dm`.
#'
#' @param eval_model logical, indicating if the model should be re-evaluated or
#'  not when updating the parameters (see [dRiftDM::re_evaluate_model]).
#'  Default is `FALSE`.
#'
#' @param select_unique logical, indicating if only those parameters shall be
#'  returned that are considered unique (e.g., when a parameter is set to be
#'  identical across three conditions, then the parameter is only returned once).
#'  Default is `TRUE`. This will also return only those parameters that are
#'  estimated. The argument is currently not supported for objects of type
#'  `mcmc_dm`.
#' @param select_custom_prms logical, indicating if custom parameters shall be
#'  returned as well. Only has an effect if `select_unique = FALSE`.
#'  The argument is currently not supported for objects of type
#'  `mcmc_dm`.
#' @param value numerical, a vector with valid values to update the model's
#' parameters. Must match with the number of (unique and free) parameters.
#' @inheritParams print.stats_dm
#' @param x an object of type `coefs_dm`, as returned by the function
#' `coef()` when supplied with a `fits_ids_dm` object.
#' @param .f the function to be applied to each parameter of a chain. Must
#' either return a single value or a vector (with always the same length).
#' Default is `mean` (i.e., the mean function).
#' @param id an optional numeric or character vector specifying the IDs of
#' participants from whom to summarize samples. Only applicable when the model was
#' estimated hierarchically. Use `id = NA` as a shorthand to summarize samples
#' for all individuals in the chain object.
#'
#'
#' @details
#' `coef.*()` are methods for the generic [stats::coef()] function; `coefs<-()`
#'  is a generic replacement function, currently supporting objects of type
#'  [dRiftDM::drift_dm].
#'
#'  The argument `value` supplied to the `coefs<-()` function must match with
#'  the vector returned from `coef(<object>)`. It is possible to
#'  update just part of the (unique) parameters.
#'
#'  Whenever the argument `select_unique` is `TRUE`, `dRiftDM` tries to provide
#'  unique parameter labels.
#'
#' @returns
#'  For objects of type [dRiftDM::drift_dm], `coefs()` returns either a named
#'  numeric vector if `select_unique = TRUE`, or a matrix if
#'  `select_unique = FALSE`. If `select_custom_prms = TRUE`, custom parameters
#'  are added to the matrix.
#'
#'  For objects of type `fits_ids_dm`, `coefs()` returns a [data.frame]. If
#'  `select_unique = TRUE`, the columns will be the (unique, free) parameters,
#'  together with a column coding `IDs`. If `select_unique = FALSE`, the columns
#'  will be the parameters as listed in the columns of `prms_matrix` (see
#'  [dRiftDM::drift_dm]), together with columns coding the conditions and
#'  `IDs`. If `select_custom_prms = TRUE`, the [data.frame] will also contain
#'  columns for the custom parameters. The returned [data.frame] has the class
#'  label `coefs_dm` to easily plot histograms for each parameter
#'  (see [dRiftDM::hist.coefs_dm]).
#'
#'  For objects of type `fits_agg_dm`, returns the same as `coef.drift_dm()`
#'  (i.e., as if calling `coef()` with an object of type `drift_dm`)
#'
#' For objects of type `mcmc_dm`, the return type depends on the model structure
#' and the `.f` output:
#'
#' - If the model is non-hierarchical or `id` is a single value (not `NA`),
#'   the function returns either a `vector` or a `matrix`, depending on whether
#'   `.f` returns a single value or a vector.
#'
#' - In the hierarchical case, when `id` is a vector or `NA`, the function
#'   returns a `data.frame`. If `.f` returns a single value, the `data.frame`
#'   will contain one row per participant (with an `ID` column and one column
#'   per parameter). If `.f` returns a vector, the `data.frame` will include
#'   an additional column `.f_out`, coding the output of `.f` in long
#'   format.
#'
#' @seealso [dRiftDM::drift_dm()]
#'
#' @examples
#' # get a pre-built model and a data set for demonstration purpose
#' # (when creating the model, set the discretization to reasonable values)
#' a_model <- dmc_dm(t_max = 1.5, dx = .01, dt = .005)
#' coef(a_model) # gives the free and unique parameters
#' coef(a_model, select_unique = FALSE) # gives the entire parameter matrix
#'
#' @export
coef.drift_dm <- function(object, ..., select_unique = TRUE,
                          select_custom_prms = TRUE) {
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
    all_prms <- object$flex_prms_obj$prms_matrix
    cust_prms_matrix <- lapply(
      object$flex_prms_obj$cust_prms$values,
      \(x) return(x)
    )
    cust_prms_matrix <- do.call(cbind, cust_prms_matrix)
    if (select_custom_prms) {
      all_prms <- cbind(all_prms, cust_prms_matrix)
    }
    return(all_prms)
  }
}


# FOR FITS_AGG_DM ---------------------------------------------------------

# logLik, AIC, BIC etc. is not yet supported (because fits_agg_dm currently
# only works with RMSE)

#' @rdname coef.drift_dm
#' @export
coef.fits_agg_dm <- function(object, ...) {
  coef(object$drift_dm_obj, ...)
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
#' @return An object of type `fit_stats` containing the respective statistic in
#' one column (named `Log_Like`, `AIC`, or `BIC`) and a corresponding `ID`
#' column.
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
  if (object$drift_dm_fit_info$drift_dm_obj$cost_function == "neg_log_like") {
    stats <- calc_stats(object, type = "fit_stats")
  } else {
    warning(
      "The currently set cost_function of the model is not a ",
      "(log)-likelihood. Returning NULL."
    )
    return(NULL)
  }

  return(stats[c("ID", "Log_Like")])
}

#' @rdname logLik.fits_ids_dm
#' @importFrom stats AIC
#' @export
AIC.fits_ids_dm <- function(object, ..., k = 2) {
  if (object$drift_dm_fit_info$drift_dm_obj$cost_function == "neg_log_like") {
    stats <- calc_stats(object, type = "fit_stats", k = k)
  } else {
    warning(
      "The currently set cost_function of the model is not a ",
      "(log)-likelihood. Returning NULL."
    )
    return(NULL)
  }
  return(stats[c("ID", "AIC")])
}

#' @rdname logLik.fits_ids_dm
#' @importFrom stats BIC
#' @export
BIC.fits_ids_dm <- function(object, ...) {
  if (object$drift_dm_fit_info$drift_dm_obj$cost_function == "neg_log_like") {
    stats <- calc_stats(object, type = "fit_stats")
  } else {
    warning(
      "The currently set cost_function of the model is not a ",
      "(log)-likelihood. Returning NULL."
    )
    return(NULL)
  }
  return(stats[c("ID", "BIC")])
}

#' @rdname coef.drift_dm
#' @export
coef.fits_ids_dm <- function(object, ...) {
  all_coefs <- lapply(object$all_fits, function(x) coef(x, ...))
  # special treatment of the ID, because coef might return a matrix or vector
  all_coefs <- lapply(names(all_coefs), function(x) {
    one_coef <- all_coefs[[x]]
    if (!is.matrix(one_coef)) {
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





# FOR MCMC_DM OBJECTS -----------------------------------------------------

#' @rdname coef.drift_dm
#' @export
coef.mcmc_dm <- function(object, ..., .f = mean, id = NULL) {

  # input checks and handling of the ids argument
  if (!is.function(mean)) {
    stop(".f argument must be a function")
  }

  hierarchical = attr(object, "hierarchical")
  if (!is.null(id) & hierarchical) {
    if (is.na(id)) {
      id = dimnames(object[["theta"]])[[3]] # third dimension are the ids
    }
  }

  if (!is.null(id) & !hierarchical) {
    stop("Specifying `id` doesn't make sense in the non-hierarchical case, ",
         "because the chain object refers to only a single participant.")
  }

  # call coef recursively with multiple ids are requested
  if (length(id) > 1) {
    results = lapply(
      id, \(one_id) coef.mcmc_dm(object, ..., .f = .f, id = one_id)
    )
    results <- lapply(seq_along(id), \(id_idx){
      x = results[[id_idx]]
      if (is.matrix(x)) x = cbind(`.f_out` = rownames(x), x)
      if (is.vector(x)) x = t(as.matrix(x))
      x = as.data.frame(x)
      x = cbind(ID = id[id_idx], x)
      return(x)
    })
    results = do.call(rbind, results)
    results$ID = try_cast_integer(results$ID)
    results <- results[order(results$ID), ]
    rownames(results) <- NULL
    return(results)
  }

  # get the relevant chains
  chains = get_subset_chains(chains_obj = object, id = id)
  result = apply(chains, 1, FUN = .f, simplify = TRUE)
  if (!(is.vector(result) | is.matrix(result))) {
    stop("Function supplied as argument .f did not return either a single ",
         "value or a vector of always the same length")
  }
  return(result)
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



# UNPACK METHODS ----------------------------------------------------------

#' @rdname unpack_obj
#' @export
unpack_obj.coefs_dm <- function(object, ..., unpack_elements = TRUE) {
  if (unpack_elements) {
    object <- as.data.frame(object)
  }

  return(object)
}
