check_if_named_numeric_vector <- function(x, var_name, labels = NULL,
                                          length = NULL) {
  if (!is.numeric(x)) {
    stop(var_name, " is not a (named) numeric vector")
  }

  if (length(x) == 0) {
    stop(var_name, " is an empty vector")
  }

  if (!is.null(length) && length(x) != length) {
    stop(var_name, " has not ", length, " entries")
  }
  if (is.null(names(x))) {
    stop("please ensure that ", var_name, " is a named vector")
  }

  if (any(names(x) == "") | any(is.na(names(x)))) {
    stop(var_name, " does not provide a name for each entry")
  }

  if (!is.null(labels) && !all(names(x) %in% labels)) {
    stop(
      "the entries of ", var_name, " can not be adressed by ",
      paste(labels, collapse = ", ")
    )
  }
  if (any(is.na(x))) stop(var_name, " contains NAs")
  if (any(is.infinite(x))) warning(var_name, " contains infinite values")
}

prms_to_str <- function(prms, names_prms, round_digits = NULL,
                        sep = "=", collapse = ", ") {
  if (is.null(round_digits)) {
    round_digits <- drift_dm_default_rounding()
  }

  if (!is.numeric(round_digits)) {
    stop("round_digits is not of type numeric")
  }

  if (!is.character(names_prms)) {
    stop("names_prms argument not of type character")
  }

  if (!is.numeric(prms)) {
    stop("prms argument not of type numeric")
  }

  if (length(prms) != length(names_prms)) {
    stop("length of argument prms and names_prms don't match")
  }

  if (length(prms) == 0 || length(names_prms) == 0) {
    stop("arguments prms or names_prms are of length zero")
  }

  if (!is.character(sep) || !is.character(collapse)) {
    stop("sep or collapse argument not of type character")
  }

  current_prms <- paste(names_prms,
    round(prms, round_digits),
    sep = sep
  )
  current_prms <- paste(current_prms, collapse = collapse)
  return(current_prms)
}

first_letter_up <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


# ============ GLOBAL VARIABLES
drift_dm_approx_error <- function() {
  return(1e-20)
}
drift_dm_medium_approx_error <- function() {
  return(.0001)
}
drift_dm_small_approx_error <- function() {
  return(.01)
}
drift_dm_rough_approx_error <- function() {
  return(.1)
}
drift_dm_robust_prm <- function() {
  return(1e-12)
}
drift_dm_default_rounding <- function() {
  return(3)
}

drift_dm_default_probs <- function() {
  return(seq(0.1, 0.9, 0.1))
}
