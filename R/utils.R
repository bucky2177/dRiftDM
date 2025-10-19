# ETC ---------------------------------------------------------------------

#' Check if an object is a valid numeric vector
#'
#' This function verifies whether the input is a numeric vector with no missing
#' (`NA`, `NaN`) or infinite (`Inf` or `-Inf`) values.
#'
#' @param x An object to check.
#'
#' @return A logical value: `TRUE` if the input is a numeric vector without any
#' missing or infinite values, otherwise `FALSE`.
#'
#'
#' @keywords internal
is_numeric <- function(x) {
  is.numeric(x) & !anyNA(x) & all(!is.infinite(x))
}

#' Check if Object is a Named Numeric Vector
#'
#' @description
#' Validates that an object is a named numeric vector with specified attributes.
#' Optionally checks specific names, length, and restrictions on label
#' characters.
#'
#' @param x numeric vector, expected to be named.
#' @param var_name character, the name of the variable to display in error
#' messages.
#' @param labels character vector, optional, specifying valid names for `x`. If
#' provided, all names in `x` must match these labels.
#' @param length integer, optional, specifying the exact required length of `x`.
#' @param allow_non_word_chars logical, whether to permit non-word characters in
#' names (default is `FALSE`).
#'
#' @return Throws an error if the conditions are not met. If all checks pass,
#' no output is returned.
#'
#' @details
#' Checks for:
#' - Numeric type of `x` with non-zero length
#' - Required length, if specified
#' - Unique, non-empty names for each entry in `x`
#' - Match of all names in `x` to `labels`, if `labels` is specified
#' - Absence of `NA` of `Inf` values in `x`
#' - Optional absence of non-word names if `allow_non_word_chars` is FALSE
#'
#' @keywords internal
check_if_named_numeric_vector <- function(
  x,
  var_name,
  labels = NULL,
  length = NULL,
  allow_non_word_chars = FALSE
) {
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

  if (anyDuplicated(names(x)) > 0) {
    stop("there are duplicate names in ", var_name)
  }

  if (anyDuplicated(labels) > 0) {
    stop("there are duplicate names in the provided labels")
  }

  if (!is.null(labels) && !all(names(x) %in% labels)) {
    stop(
      "the entries of ",
      var_name,
      " can not be adressed by ",
      paste(labels, collapse = ", ")
    )
  }
  if (any(is.na(x))) {
    stop(var_name, " contains NAs")
  }
  if (any(is.infinite(x))) {
    warning(var_name, " contains infinite values")
  }

  if (!allow_non_word_chars) {
    given_names <- names(x)
    given_names <- grepl("[\\W]", given_names, perl = TRUE)
    if (any(given_names)) {
      stop(var_name, " provides illegal non-alphanumeric characters")
    }
  }
}


#' Format Parameters as String
#'
#' Converts parameter values into a formatted string.
#'
#' @param x a [dRiftDM::drift_dm]object or character vector for labels.
#' @param prms Numeric vector of values (used if `x` is character).
#' @param round_digits Rounding precision (default set by
#' [dRiftDM::drift_dm_default_rounding()]).
#' @param sep Separator between names and values (default: "=>").
#' @param collapse String to separate each name-value pair (default: "\\n").
#'
#' @return A single formatted string with parameter names and values.
#' (e.g., "a => 0 \\n b => 1")
#'
#' @seealso [dRiftDM::coef.drift_dm()], as the numeric
#' vector provided by this call is used when `x` is of type [dRiftDM::drift_dm]
#'
#' @keywords internal
prms_to_str <- function(
  x,
  prms = NULL,
  round_digits = NULL,
  sep = "=>",
  collapse = "\n"
) {
  if (inherits(x, "drift_dm")) {
    prms <- coef(x, select_unique = TRUE)
    names_prms <- names(prms)
    prms <- unname(prms)
  } else {
    names_prms <- x
  }

  if (is.null(round_digits)) {
    round_digits <- drift_dm_default_rounding()
  }

  if (!is_numeric(round_digits)) {
    stop("round_digits is not a valid numeric vector")
  }

  if (!is.character(names_prms)) {
    stop("names_prms argument not of type character")
  }

  if (!all(is_numeric(prms))) {
    stop("prms argument not a valid numeric vector")
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

  current_prms <- paste(names_prms, round(prms, round_digits), sep = sep)
  current_prms <- paste(current_prms, collapse = collapse)
  return(current_prms)
}


#' Generate Parameter-Condition Labels
#'
#' Creates a vector of labels from a parameter-condition combination matrix,
#' resulting from a call to [dRiftDM::prms_cond_combo]. Used, for instance, in
#' [dRiftDM::coef.drift_dm].
#'
#' @param prms_cond_combo a 2-row character matrix where each column represents
#' a unique parameter-condition combination.
#' @param sep Separator for parameter and condition labels (default: "~").
#'
#' @return A vector of labels with as many entries as the columns of
#' `prms_cond_combo` had, combining parameter and condition (if necessary).
#'
#' If the parameter labels are already unique (because all parameters do not
#' vary across conditions or are selectively used for one condition), then
#' only these parameter labels are returned
#'
#' @keywords internal
prm_cond_combo_2_labels <- function(prms_cond_combo, sep = ".") {
  stopifnot(is.character(prms_cond_combo))
  stopifnot(is.matrix(prms_cond_combo))
  stopifnot(nrow(prms_cond_combo) == 2)

  # Create initial labels with only parameters
  labels <- prms_cond_combo[1, ]

  # Identify non-unique parameters and update those labels to include conditions
  non_unique_prms <- duplicated(prms_cond_combo[1, ]) |
    duplicated(prms_cond_combo[1, ], fromLast = TRUE)
  labels[non_unique_prms] <- paste(
    prms_cond_combo[1, non_unique_prms],
    prms_cond_combo[2, non_unique_prms],
    sep = sep
  )

  return(labels)
}


#' Create a matrix for lower and upper
#'
#' Outsourced, deep inside the package function to avoid large nesting
#'
#' @param input either a list or a vector of numeric values
#' @param conds a character string, conceptually representing the
#'  conditions of a model
#' @param prm_labels a character string with parameter labels. Used as a fall
#'  back when the default_values are not labeled (see details)
#'
#' @details
#' The goal of this function is to build up a matrix, serving as the upper or
#' lower end of a parameter space (relevant when simulating data). The function
#' gets called by [dRiftDM::get_parameters_smart()].
#'
#' It assumes the following: `input` is either a list or a numeric vector.
#'
#' * The easiest case is when it is a numeric vector. In this case, the
#'   function builds a matrix with as many rows as entries in `conds`. The
#'   rows will also be labeled according to `conds`. The column names are
#'   either the names specified with the numeric vector, or the labels specified
#'   in `prm_labels`
#'
#' * The less intuitive case is when `input` is a list. In this case, the list
#'   requires an entry called "default_values" which specifies the named or plain
#'   numeric vector as above. If the list only contains this entry, then the
#'   behavior is as if `input` was already a numeric vector. However, the `input`
#'   list can also have entries labeled as specific conditions, which contain
#'   named (!) numeric vectors with parameter labels. This will modify the
#'   value for the upper/lower parameter space with respect to the specified
#'   parameters in the respective condition.#'
#'
#' @returns a matrix indicating either the upper or lower end of a parameter
#' space. There will be as many rows as `conds` implies. The number of columns
#' depend on `input` (matching its length if it is a vector, or matching the
#' length of the entry "default_values" if it is a list). If `input` is
#' `NULL`, then `NULL` is returned.
#'
#' @seealso [dRiftDM::simulate_data()], [dRiftDM::simulate_values()]
#'
#' @keywords internal
create_matrix_smart <- function(input, conds, prm_labels = NULL) {
  if (is.null(input)) {
    return(NULL)
  }
  if (!is.character(conds) | length(conds) == 0) {
    stop("conds must be a character vector")
  }

  # if it is a list, extract default values and keep the rest
  # otherwise, just use the vector directly
  if (is.list(input)) {
    if (sum(names(input) == "default_values") != 1) {
      stop(
        "remember to have (only) one entry of lower/upper with the name",
        " 'default_values', to ensure 'default' parameter ranges"
      )
    }

    if ("default_values" %in% conds) {
      stop(
        "damn, that's unfortunate.. Your model has a condition named ",
        "'default_values' and that clashes with the internal programming of",
        "dRiftDM. Please rename your conditions..."
      )
    }
    def_values <- input$default_values
    input <- input[which(names(input) != "default_values")]
  } else if (is_numeric(input)) {
    # if it is a vector and has condition specific values, wrangle them into a
    # list.. this aligns downstream processing with directly specifying a list
    names_input <- names(input)
    if (!is.null(names_input)) {
      if (any(names_input == "")) {
        stop(
          "Parameter labels must be specified for each element of your ",
          "input vector. Check your arguments!"
        )
      }
      if (any(duplicated(names_input))) {
        stop("Parameter labels must be unique! Check your input.")
      }
      raw_input <- input
      # ensures that condition specific values land at the end
      raw_input <- raw_input[sort(names_input)]
      split_up <- strsplit(names(raw_input), split = "\\.")
      def_values <- c()
      input <- list()
      for (i in seq_along(split_up)) {
        prm <- split_up[[i]][1]
        if (!(prm %in% names(def_values))) {
          def_values[prm] <- raw_input[i]
        } else {
          input[[split_up[[i]][2]]] <- stats::setNames(raw_input[i], prm)
        }
      }
    } else {
      def_values <- input
    }
  } else {
    stop("illegal data type for (values in) input")
  }

  # if there are no parameter names coming with the default values,
  # use the supplied argument
  if (is.null(names(def_values))) {
    if (!is.character(prm_labels) | length(prm_labels) == 0) {
      stop("prm_labels must be a character vector")
    }

    if (length(def_values) != length(prm_labels)) {
      stop(
        "The input (e.g., for lower, upper, means) specified ",
        length(def_values),
        " parameter values, ",
        "which doesn't match with the ",
        length(prm_labels),
        " parameters ",
        "of the model that are considered free in at least one condition (",
        paste(prm_labels, collapse = ","),
        "). Double-check your arguments!"
      )
    }
    names(def_values) <- prm_labels
  }

  # create a matrix of default values
  check_if_named_numeric_vector(x = def_values, var_name = "default_values")
  result <- do.call(
    rbind,
    replicate(length(conds), def_values, simplify = FALSE)
  )
  rownames(result) <- conds

  # if there is a remaining list, then fill in the specific input values
  if (is.list(input) & length(input) > 0) {
    rem_prms_conds <- lapply(input, names)

    if (!all(unlist(rem_prms_conds) %in% colnames(result))) {
      stop(
        "specific input value specified for a parameter ",
        "that is not part of the default values"
      )
    }

    if (!all(names(rem_prms_conds) %in% conds)) {
      stop(
        "specific input values specified for a condition ",
        "that is not part of the provided conditions"
      )
    }

    # Fill the matrix with values from the list
    for (i in seq_along(rem_prms_conds)) {
      one_cond <- names(rem_prms_conds)[i]
      prm_vals <- input[[i]]

      if (is.null(prm_vals)) {
        stop("specific input values must provide parameter names")
      }
      prm_names <- names(prm_vals)
      result[one_cond, prm_names] <- input[[one_cond]]
    }
  }

  stopifnot(!is.null(colnames(result)))
  stopifnot(!is.null(rownames(result)))

  return(result)
}


#' Turn default/special parameter specifications to vectors
#'
#' The function is used in the depths to map parameter inputs to the parameters
#' of a model. One application is to get the search space as a vector, matching
#' with the free parameters of a model. Other applications map, for example,
#' mean values to the free parameters of a model.
#' Relevant when users use the "default parameters" approach where they
#' only specify the parameter labels and assume the package figures out
#' how each parameter relates across conditions (see [dRiftDM::simulate_data]).
#' This comes in handy, when freeing a parameter across conditions, while the
#' search space remains the same (otherwise, a user would always have to adapt
#' the vectors for lower/upper to match with [dRiftDM::x2prms_vals])
#'
#' @param drift_dm_obj an object of type drift_dm
#' @param input_a,input_b either a atomic vector or list (see
#' [dRiftDM::create_matrix_smart])
#' @param labels optional logical, if `TRUE`, then the returned vectors have
#' the unique parameter labels according to [dRiftDM::prm_cond_combo_2_labels].
#' @param is_l_u optional logical, if `TRUE`, a warning is thrown when
#' `input_a` leads to larger values than `input_b`. Useful when `input_a` and
#' `input_b` span a (search) space.
#' @param fill_up_with optional values used to fill up the returned vectors
#' for all parameters that are not specified in `input_a` or `input_b` (requires
#' at least one parameter to specified).
#'
#' @details
#' The function first gets all unique parameters across conditions using
#' [dRiftDM::prms_cond_combo]. The unique parameter labels are then forwarded
#' to [dRiftDM::create_matrix_smart], together with all (!) the conditions in the
#' model and the `input_a`/`input_b` arguments. Subsequently, the created matrices
#' are wrangled into vectors in accordance with [dRiftDM::prms_cond_combo]. The
#' vectors are then passed back.
#'
#'
#' @returns a list with two entries named `vec_a/vec_b`. The length and names
#' (if requested) matches with coef(model, select_unique = TRUE). When
#' `input_a` and/or `input_b` is `NULL`, the respective entry for
#' `vec_a`/`vec_b` will be `NULL` as well.
#'
#' @keywords internal
get_parameters_smart <- function(
  drift_dm_obj,
  input_a,
  input_b = NULL,
  labels = TRUE,
  is_l_u = TRUE,
  fill_up_with = NULL
) {
  # input checks
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }
  stopifnot(is.logical(labels) & length(labels) == 1)

  # get the unique parameters
  prm_cond_combo <- prms_cond_combo(drift_dm_obj = drift_dm_obj)
  conds <- conds(drift_dm_obj)
  prm_labels <- unique(prm_cond_combo[1, ])

  # get the upper and lower matrices (might return NULL if input is NULL)
  matrix_a <- create_matrix_smart(
    input = input_a,
    conds = conds,
    prm_labels = prm_labels
  )
  matrix_b <- create_matrix_smart(
    input = input_b,
    conds = conds,
    prm_labels = prm_labels
  )

  # fill up with default parameter settings
  fill_up <- function(matrix_x) {
    if (is.null(matrix_x)) {
      return(NULL)
    } # no matrix provided
    if (is.null(fill_up_with)) {
      return(matrix_x)
    } # no fill_up requested

    # create a matrix with default values for parameters that were not
    # specified in input_x (if there are no missing parameters, then this will
    # return matrix_x unmodified)
    missing_prms <- prm_labels[!(prm_labels %in% colnames(matrix_x))]
    missing_mat <- matrix(
      data = fill_up_with,
      nrow = nrow(matrix_x),
      ncol = length(missing_prms)
    )
    colnames(missing_mat) <- missing_prms
    return(cbind(matrix_x, missing_mat))
  }

  matrix_a <- fill_up(matrix_a)
  matrix_b <- fill_up(matrix_b)

  # check if all parameters are there (or additional parameters that are )
  diff_a <- setdiff(prm_labels, colnames(matrix_a))
  if (!is.null(matrix_a) && length(diff_a) > 0) {
    stop(
      "Some free model parameters are missing input values. ",
      "Please check your arguments (e.g., lower, upper, means) and provide ",
      "values for the following parameters: ",
      paste(diff_a, collapse = ", ")
    )
  }
  diff_b <- setdiff(prm_labels, colnames(matrix_b))
  if (!is.null(matrix_b) && !all(prm_labels %in% colnames(matrix_b))) {
    stop(
      "Some free model parameters are missing input values. ",
      "Please check your arguments (e.g., lower, upper, means) and provide ",
      "values for the following parameters: ",
      paste(diff_a, collapse = ", ")
    )
  }

  diff_a <- setdiff(colnames(matrix_a), prm_labels)
  if (!is.null(matrix_a) && length(diff_a) > 0) {
    warning(
      "Parameter values (e.g., for lower, upper, means) were provided ",
      "for the following parameters: ",
      paste(diff_a, collapse = ", "),
      ". These parameters are not 'free' or not part of the model and will be ",
      "ignored."
    )
  }
  diff_b <- setdiff(colnames(matrix_b), prm_labels)
  if (!is.null(matrix_b) && length(diff_b) > 0) {
    warning(
      "Parameter values (e.g., for lower, upper, means) were provided ",
      "for the following parameters: ",
      paste(diff_b, collapse = ", "),
      ". These parameters are not 'free' or not part of the model and will be ",
      "ignored."
    )
  }

  # turn the matrix to a vector (which works with unsorted matrices)
  map_to_vec <- function(matrix_x) {
    if (is.null(matrix_x)) {
      return(NULL)
    }
    sapply(1:ncol(prm_cond_combo), function(idx) {
      prm <- prm_cond_combo[1, idx]
      cond <- prm_cond_combo[2, idx]
      prm_is_unique <- sum(prm_cond_combo[1, ] == prm) == 1
      vals_are_unique <- length(unique(matrix_x[, prm])) == 1
      if (prm_is_unique && !vals_are_unique) {
        stop(
          "Condition-specific input found for parameter '",
          prm,
          "', but this parameter is identical across conditions."
        )
      }
      tryCatch(
        matrix_x[cond, prm],
        error = function(e) {
          stop(
            "Couldn't map the input to the model parameters.",
            " Did you forget to list all the model parameters that are 'free'?"
          )
        }
      )
    })
  }

  vec_a <- map_to_vec(matrix_a)
  vec_b <- map_to_vec(matrix_b)

  # label (of requested)
  names_prms <- prm_cond_combo_2_labels(prm_cond_combo)
  if (!is.null(vec_a) && labels) {
    names(vec_a) <- names_prms
  }
  if (!is.null(vec_b) && labels) {
    names(vec_b) <- names_prms
  }

  # will not be thrown when vec_a or vec_b are NULL
  if (is_l_u && any(vec_a > vec_b)) {
    warning(
      "values in the created vec_a (e.g., lower) vector are sometimes larger",
      " than in the created vec_b (e.g., upper) vector. This likely isn't",
      "intended."
    )
  }

  return(list(vec_a = vec_a, vec_b = vec_b))
}


with_muffled_warning <- function(expr, pattern, ignore.case = FALSE) {
  withCallingHandlers(
    {
      eval(expr)
    },
    warning = function(w) {
      if (grepl(pattern, conditionMessage(w), ignore.case = ignore.case)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}


# INTEGRATION -------------------------------------------------------------

#' @rdname trapz
internal_trapz <- function(x, y, return_cumsum = FALSE) {
  if (length(x) != length(y)) {
    stop("'x' and 'y' must have the same length")
  }
  n <- length(x)
  if (n < 2L) {
    stop("need at least two points")
  }
  if (!is_numeric(x) || !is_numeric(y)) {
    stop("'x' and 'y' must be valid numerics")
  }
  if (any(diff(x) <= 0)) {
    stop("'x' must be strictly increasing")
  }

  dx <- diff(x)
  mid_heights <- (y[-n] + y[-1]) / 2.0

  if (return_cumsum) {
    y_int <- c(0, cumsum(dx * mid_heights))
    return(y_int)
  }
  return(sum(dx * mid_heights))
}

#' @rdname trapz
cumtrapz <- function(x, y) {
  internal_trapz(x, y, TRUE)
}

#' Numerical integration using the trapezoidal rule
#'
#' These are internal helper functions to perform numerical integration
#' via the trapezoidal rule. The workhorse is `internal_trapz()`, which
#' computes the full integral or returns the cumulative integral.
#'
#' @param x numeric vector of strictly increasing x-values.
#' @param y numeric vector of function values at `x`.
#' @param return_cumsum logical, if `TRUE` return the cumulative integral at
#'   each point in `x` (starting with 0), if `FALSE` return the total integral.
#'
#' @details
#' - `internal_trapz(x, y, return_cumsum = FALSE)`:
#'   core implementation
#'
#' - `trapz(x, y)`
#'   wrapper for `internal_trapz(x, y, FALSE)`, returns the total integral.
#'
#' - `cumtrapz(x, y)`
#'   wrapper for `internal_trapz(x, y, TRUE)`, returns the cumulative integral.
#'
#' @returns
#' - `trapz()`: a single numeric value
#' - `cumtrapz()`: numeric vector of cumulative integrals (starting with 0)
#' - `internal_trapz()`: either of the above, depending on `return_cumsum`
#'
#' @keywords internal
trapz <- function(x, y) {
  internal_trapz(x, y, FALSE)
}


# GLOBAL VARIABLES  -------------------------------------------------------

#' Default Values for the dRiftDM Package
#'
#' These functions provide default values for various settings in the
#' `dRiftDM` package.
#'
#' @return
#' the respective values/lists as described in the Details section
#'
#' @details
#'
#' - `drift_dm_approx_error()`: Returns the default approximation error
#' for precise calculations (1e-20).
#' - `drift_dm_medium_approx_error()`: Returns a 'medium' level of approximation
#' error (1e-06).
#' - `drift_dm_small_approx_error()`: Returns a 'small' level of approximation
#' error (.01).
#' - `drift_dm_rough_approx_error()`: Returns a rough level of approximation
#' error (.1).
#' - `drift_dm_robust_prm()`: Returns a value that is added to the PDFs after
#' convolution with the non-decision time to make parameter estimation and the
#' evaluation of the log-likelihood more robust (1e-8).
#' - `drift_dm_default_rounding()`: Returns the default rounding precision for
#' numerical outputs (3).
#' - `drift_dm_default_probs()`: Returns the default sequence of probabilities
#' for quantiles (0.1, 0.2, ..., 0.9)
#' - `drift_dm_default_n_bins()`: Returns the default number of bins for a
#' CAF (5)
#' - `drift_dm_default_b_coding()`: Returns the default boundary coding
#' (list(column = "Error", u_name_value = c("corr" = 0),
#' l_name_value = c("err" = 1))
#' - `drift_dm_skip_if_contr_low()`: returns the value 0.0001. If a PDF
#' integrates to a value lower than that (i.e., if there is almost no
#' contribution of a PDF; most likely this will be pdf_l), then summary
#' functions returned by [dRiftDM::calc_stats()] might contain the value NA
#' for the respective PDF.
#' - `drift_dm_n_id_trunc_warn()`: returns 15. If there are warnings relevant
#' to multiple participants, the printed IDs are truncated at 15.
#'
#' @name defaults
#'
#' @keywords internal
drift_dm_approx_error <- function() {
  return(1e-20)
}

#' @rdname defaults
drift_dm_medium_approx_error <- function() {
  return(1e-06)
}

#' @rdname defaults
drift_dm_small_approx_error <- function() {
  return(.01)
}

#' @rdname defaults
drift_dm_rough_approx_error <- function() {
  return(.1)
}

#' @rdname defaults
drift_dm_robust_prm <- function() {
  return(1e-8)
}

#' @rdname defaults
drift_dm_default_rounding <- function() {
  return(3)
}

#' @rdname defaults
drift_dm_default_probs <- function() {
  return(seq(0.1, 0.9, 0.1))
}

#' @rdname defaults
drift_dm_default_n_bins <- function() {
  return(5)
}

#' @rdname defaults
drift_dm_default_b_coding <- function() {
  b_coding <- list(
    column = "Error",
    u_name_value = c("corr" = 0),
    l_name_value = c("err" = 1)
  )
  return(b_coding)
}

#' @rdname defaults
drift_dm_skip_if_contr_low <- function() {
  return(0.0001)
}


#' @rdname defaults
drift_dm_n_id_trunc_warn <- function() {
  return(15L)
}

#' Pre-built Drift Diffusion Models
#'
#' Returns the names of available pre-built DDMs in dRiftDM.
#'
#' @returns A character vector of model names.
#' @keywords internal
drift_dm_pre_built_models <- function() {
  c("dmc_dm", "ratcliff_dm", "ssp_dm")
}


#' Available Cost Functions for Model Estimation
#'
#' Returns the names of implemented cost functions.
#'
#' @return A character vector of cost function names.
#' @keywords internal
drift_dm_cost_functions <- function() {
  c("neg_log_like", "rmse")
}


#' Available types of statistics
#'
#' Internal helper to return supported statistic types depending on the
#' context (e.g., for observed data.frames or fitted model objects).
#'
#' @param context a character string, indicating the context. If
#'   `NULL`, all available types are returned.
#'
#' @return a character vector of valid statistic types for the given context.
#' @keywords internal
drift_dm_stats_types <- function(context = NULL) {
  sum_dist <- c("basic_stats", "cafs", "quantiles", "delta_funs", "densities")
  all_stats <- c(sum_dist, "fit_stats")

  if (is.null(context)) {
    return(all_stats)
  }

  context <- match.arg(
    context,
    choices = c(
      "data.frame",
      "drift_dm",
      "fits_ids_dm",
      "fits_agg_dm",
      "sum_dist"
    )
  )

  if (context == "data.frame" || context == "sum_dist") {
    return(sum_dist)
  } else {
    return(all_stats)
  }
}


# FOR EXAMPLES ------------------------------------------------------------

#' Auxiliary Function to load a `fits_ids_dm`, `fits_agg_dm`, or `mcmc_dm`
#' object
#'
#' The function is merely helper functions to create an object of type
#' `fits_ids_dm`, `fits_agg_dm`, or `mcmc_dm`. It is used for example code.
#'
#' @param class a string of either `"fits_ids_dm"`, `"fits_agg_dm"`, or
#' `"mcmc_dm"` (can be abbreviated)
#' @param hierarchical a logical, relevant when `class = "mcmc_dm"`. If `TRUE`,
#' an object from a hierarchical fit is returned. If `FALSE`, an object from an
#' individual fit is returned.
#'
#' @returns An object of type `fits_ids_dm`, `fits_agg_dm`, or `mcmc_dm`,
#' mimicking a result from calling [dRiftDM::estimate_dm()].
#'
#' @details
#'
#' For `"fits_ids_dm"`, the returned object comprises DMC
#' (see [dRiftDM::dmc_dm()]) fitted to three participants of the
#' `ulrich_flanker_data`.
#'
#' For `"fits_agg_dm"`, the returned object comprises the Ratcliff model
#' (see [dRiftDM::ratcliff_dm()]) fitted to synthetic data of three participants.
#'
#' For `"mcmc_dm"` and `hierarchical = FALSE`, the returned object comprises the
#' Ratcliff model (see [dRiftDM::ratcliff_dm()]) fitted to synthetic data of one
#' participant.
#'
#' For `"mcmc_dm"` and `hierarchical = TRUE`, the returned object comprises the
#' Ratcliff model (see [dRiftDM::ratcliff_dm()]) fitted to synthetic data of ten
#' participants.
#'
#' @examples
#' get_example_fits(class = "fits_agg")
#'
#' @export
get_example_fits <- function(class, hierarchical = FALSE) {
  class <- match.arg(class, c("fits_ids_dm", "fits_agg_dm", "mcmc_dm"))
  sys_path <- system.file(package = "dRiftDM")
  if (class == "fits_ids_dm") {
    obj <- readRDS(
      file = file.path(sys_path, "example_fits_ids.rds")
    )
  }

  if (class == "fits_agg_dm") {
    obj <- readRDS(
      file = file.path(sys_path, "example_fits_agg.rds")
    )
  }

  if (class == "mcmc_dm") {
    if (!hierarchical) {
      obj <- readRDS(
        file = file.path(sys_path, "example_mcmc.rds")
      )
    } else {
      obj <- readRDS(
        file = file.path(sys_path, "example_mcmc_hier.rds")
      )
    }
  }
  return(obj)
}
