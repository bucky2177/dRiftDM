check_if_named_numeric_vector <- function(x, var_name, labels = NULL,
                                          length = NULL) {
  if (!is.numeric(x)) {
    stop(var_name, " is not a (named) numeric vector")
  }

  if (!is.null(length) && length(x) != length) {
    stop(var_name, " has not ", length, " entries")
  }

  if (is.null(names(x))) {
    stop("please ensure that prms_disc is a named vector")
  }

  if (!is.null(labels) && !all(names(x) %in% labels)) {
    stop(
      "the entries of ", var_name, " can not be adressed by ",
      paste(labels, collapse = ", ")
    )
  }
  if (any(is.na(x))) stop(var_name, "contains NAs")
}

prms_to_str <- function(prms, names_prms, round_digits = NULL,
                        sep = "=", collapse = ", ") {
  if (is.null(round_digits)) {
    round_digits <- drift_dm_default_rounding()
  }

  current_prms <- paste(names_prms,
    round(prms, round_digits),
    sep = sep
  )
  current_prms <- paste(current_prms, collapse = collapse)
  return(current_prms)
}


# ============ GLOBAL VARIABLES
drift_dm_approx_error <- function() {
  return(1e-20)
}
drift_dm_small_approx_error <- function() {
  return(.01)
}
drift_dm_rough_approx_error <- function() {
  return(.1)
}
drift_dm_robust_prm <- function() {
  return(1.e-10)
}
drift_dm_default_rounding <- function() {
  return(3)
}