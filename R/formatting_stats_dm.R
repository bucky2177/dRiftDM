# print functions ---------------------------------------------------------



#' @rdname calc_stats
#' @export
print.stats_dm <- function(x, ...,
                           round_digits = drift_dm_default_rounding(),
                           print_rows = 10, some = FALSE,
                           show_header = TRUE, show_note = TRUE) {
  if (show_header) {
    cat("Type of Statistic:", class(x)[1])
    cat("\n\n")
  }

  stats_dm_obj <- x
  n_row <- nrow(stats_dm_obj)

  # stats_dm objects are always of type data.frame
  idx_numeric <- sapply(stats_dm_obj, is.numeric)
  stats_dm_obj[idx_numeric] <- lapply(
    stats_dm_obj[idx_numeric], round,
    digits = round_digits
  )

  # if some = TRUE, then randomly select some of the rows
  if (some) {
    idxs <- sort(sample(x = n_row, size = min(print_rows, n_row)))
    stats_dm_obj <- stats_dm_obj[idxs, , drop = FALSE]
  }


  # print the result
  print.data.frame(utils::head(stats_dm_obj, n = print_rows))
  if (n_row > print_rows) {
    cat("...\n")
  }

  if (show_note) {
    # cat("\n(access the data.frame's columns/rows as usual, ")
    # cat("e.g., with $", names(stats_dm_obj)[1], ")", sep = "")
    cat("\n(access the data.frame's columns/rows as usual)")
  }

  invisible(x)
}



#' Print Functions for Stats Objects
#'
#' when calling [dRiftDM::calc_stats()], each returned statistic will be a
#' subclass of `stats_dm` and [data.frame]. The following `print() `methods will
#' call the more generic [dRiftDM::print.stats_dm()] function.
#'
#' @param x a subclass of [data.frame], as returned by [dRiftDM::calc_stats()].
#'
#' @param ... additional arguments passed forward to
#'   [dRiftDM::print.stats_dm()].
#'
#' @return `x` (invisibly)
#'
#' @export
#' @keywords internal
print.cafs <- function(x, ...) {
  NextMethod()
}

#' @rdname print.cafs
#' @export
#' @keywords internal
print.basic_stats <- function(x, ...) {
  NextMethod()
}

#' @rdname print.cafs
#' @export
#' @keywords internal
print.quantiles <- function(x, ...) {
  NextMethod()
}

#' @rdname print.cafs
#' @export
#' @keywords internal
print.delta_funs <- function(x, ...) {
  NextMethod()
}

#' @rdname print.cafs
#' @export
#' @keywords internal
print.fit_stats <- function(x, ...) {
  NextMethod()
}


#' @rdname print.cafs
#' @export
#' @keywords internal
print.sum_dist <- function(x, ...) {
  NextMethod()
}



#' @rdname calc_stats
#' @export
print.stats_dm_list <- function(x, ...) {
  stats_dm_list_obj <- x

  if (length(x) == 0) {
    print(unclass(x))
    return(invisible(x))
  }

  for (idx in seq_along(stats_dm_list_obj)) {
    one_stats_obj <- stats_dm_list_obj[[idx]]
    cat("Element ", idx, ", contains ", names(stats_dm_list_obj)[idx], sep = "")
    cat("\n\n")
    print(one_stats_obj, show_header = FALSE, show_note = FALSE)
    cat("\n")
    if (idx != length(stats_dm_list_obj)) cat("\n")
  }


  cat("(extract the list's elements as usual, ")
  cat("e.g., with $", names(stats_dm_list_obj)[1], ")", sep = "")
  invisible(x)
}

# summary functions -------------------------------------------------------

#' Summary for `stats_dm` Objects
#'
#' Summary and corresponding printing methods for objects of the classes
#' `stats_dm`, `basic_stats`, `cafs`, `quantiles`, `delta_funs`, `fit_stats`,
#' `sum_dist`, and `stats_dm_list`. These object types result from a call to
#' [dRiftDM::calc_stats()].
#'
#' @param object an object of the respective class
#' @param round_digits integer, specifying the number of decimal places for
#' rounding the summary of the underlying [data.frame]. Default is 3.
#' @param show_header logical. If `TRUE`, a header specifying the type of
#'  statistic will be displayed.
#' @param drop_cols character vector, specifying which columns
#'  of the table summarizing the underlying [data.frame] should not be
#'  displayed.
#' @param ... additional arguments passed forward.
#' @param x an object of the respective class.
#'
#' @return
#'  For `summary.*()` methods, a summary object of class corresponding to the
#'  input class.
#'
#'  For `print.*()` methods, the respective object is returned invisibly
#'
#' @details
#'
#' - `summary.stats_dm()`: Summarizes `stats_dm` objects, returning the type,
#'   a summary of the underlying [data.frame] (`summary_dataframe`), and, if
#'   possible, the number of unique IDs (`n_ids`).
#'
#' - `summary.sum_dist()`: Extends `summary.stats_dm()` with additional
#'   information about the source (`source`).
#'
#' - `summary.basic_stats()`: Extends `summary.sum_dist()` with additional
#'    information about the conditions (`conds`).
#'
#' - `summary.cafs()`: Extends `summary.sum_dist()` with additional information
#'    about the bins (`bins`) and conditions (`conds`).
#'
#' - `summary.quantiles()`: Extends `summary.sum_dist()` with additional
#'   information about the quantile levels (`probs`) and conditions (`conds`).
#'
#' - `summary.delta_funs()`: Extends `summary.sum_dist()` with additional
#'   information about the quantile levels (`probs`).
#'
#' - `summary.fit_stats()`: Identical to `summary.stats_dm`.
#'
#'
#' - `summary.stats_dm_list()`: Applies the summary function to each element of
#'   the list and returns a list of the respective summary objects.
#'
#' Note the following class relationships and properties:
#'
#' - `basic_stats`, `cafs`, `quantiles`, and `delta_funs` are all inheriting
#'   from `sum_dist`.
#'
#' - All `sum_dist` and `fit_stats` objects are inheriting from
#'   `stats_dm`.
#'
#' - Each `stats_dm_list` object is just a list containing instances of
#'   `stats_dm`.
#'
#' @examples
#' # get a model with data for demonstration purpose
#' a_model <- dmc_dm(dx = .0025, dt = .0025, t_max = 2)
#' obs_data(a_model) <- dmc_synth_data
#'
#' # now get some statistics and call the summary functions
#' some_stats <- calc_stats(a_model, type = c("quantiles", "fit_stats"))
#' summary(some_stats) # summary.stats_dm_list
#' summary(some_stats$quantiles) # summary.quantiles
#'
#' @export
summary.stats_dm <- function(object, ...,
                             round_digits = drift_dm_default_rounding()) {
  stats_dm_obj <- object
  ans <- list()

  ans$type <- class(stats_dm_obj)[1]
  ans$summary_dataframe <- summary.data.frame(stats_dm_obj,
    digits = round_digits
  )

  if ("ID" %in% names(stats_dm_obj)) {
    ans$n_ids <- length(unique(stats_dm_obj$ID))
  }

  class(ans) <- "summary.stats_dm"
  return(ans)
}



#' @rdname summary.stats_dm
#' @export
summary.basic_stats <- function(object, ...) {
  basic_obj <- object

  ans <- NextMethod()
  ans$conds <- conds(basic_obj)

  class(ans) <- "summary.basic_stats"
  return(ans)
}

#' @rdname summary.stats_dm
#' @export
summary.cafs <- function(object, ...) {
  cafs_obj <- object

  ans <- NextMethod()
  ans$bins <- unique(cafs_obj[["Bin"]])
  ans$conds <- conds(cafs_obj)

  class(ans) <- "summary.cafs"
  return(ans)
}



#' @rdname summary.stats_dm
#' @export
summary.quantiles <- function(object, ...) {
  quantiles_obj <- object

  ans <- NextMethod()
  ans$probs <- unique(quantiles_obj[["Prob"]])
  ans$conds <- conds(quantiles_obj)

  class(ans) <- "summary.quantiles"
  return(ans)
}


#' @rdname summary.stats_dm
#' @export
summary.delta_funs <- function(object, ...) {
  delta_funs_obj <- object

  ans <- NextMethod()
  ans$probs <- unique(delta_funs_obj[["Prob"]])
  ans$conds <- conds(delta_funs_obj)

  class(ans) <- "summary.delta_funs"
  return(ans)
}





#' @rdname summary.stats_dm
#' @export
summary.fit_stats <- function(object, ...) {
  ans <- NextMethod()
  class(ans) <- "summary.fit_stats"
  return(ans)
}


#' @rdname summary.stats_dm
#' @export
summary.sum_dist <- function(object, ...) {
  sum_dist_obj <- object

  ans <- NextMethod()
  ans$source <- unique(sum_dist_obj[["Source"]])

  class(ans) <- "summary.sum_dist"
  return(ans)
}


#' @rdname summary.stats_dm
#' @export
summary.stats_dm_list <- function(object, ...) {
  ans <- lapply(object, summary)
  class(ans) <- "summary.stats_dm_list"
  return(ans)
}


# print.summary functions -------------------------------------------------


#' @rdname summary.stats_dm
#' @export
print.summary.stats_dm <- function(x, ...,
                                   show_header = TRUE,
                                   drop_cols = NULL) {
  summary_obj <- x


  if (show_header) {
    cat("Type of Statistic:", summary_obj$type)
    cat("\n\n")
  }

  # print the table summarizing the underyling data.frame
  all_cols <- colnames(summary_obj$summary_dataframe)
  if (!is.null(drop_cols)) {
    keep_cols <- !(trimws(all_cols) %in% trimws(drop_cols))
    show_cols_summary_table <- all_cols[keep_cols]
  } else {
    show_cols_summary_table <- all_cols
  }

  cat("Dependent Variables:\n")
  print(summary_obj$summary_dataframe[, show_cols_summary_table, drop = F], ...)
  cat("\n")

  # if possible, show the number of IDs
  if (!is.null(summary_obj$n_ids)) {
    cat("N IDs:", summary_obj$n_ids, "\n")
  }

  invisible(x)
}


#' @rdname summary.stats_dm
#' @export
print.summary.basic_stats <- function(x, ...) {
  summary_obj <- x

  # call the higher-order sum_dist printing function, and drop cond and bin
  print.summary.sum_dist(summary_obj, ...,
                         drop_cols = c("ID", "Source", "Cond")
  )

  # print cafs specific information
  cat("Conditions:", paste(summary_obj$conds, collapse = ", "), "\n")

  invisible(x)
}


#' @rdname summary.stats_dm
#' @export
print.summary.cafs <- function(x, ...) {
  summary_obj <- x

  # call the higher-order sum_dist printing function, and drop cond and bin
  print.summary.sum_dist(summary_obj, ...,
    drop_cols = c("ID", "Source", "Cond", "Bin")
  )

  # print cafs specific information
  cat("Conditions:", paste(summary_obj$conds, collapse = ", "), "\n")
  cat("Bins:", paste(summary_obj$bins, collapse = ", "), "\n")



  invisible(x)
}

#' @rdname summary.stats_dm
#' @export
print.summary.quantiles <- function(x, ...) {
  summary_obj <- x

  # call the higher-order sum_dist printing function, and drop cond and probs
  print.summary.sum_dist(summary_obj, ...,
    drop_cols = c("ID", "Source", "Cond", "Prob")
  )

  # print quantiles specific information
  cat("Conditions:", paste(summary_obj$conds, collapse = ", "), "\n")
  cat("Probs:", paste(summary_obj$probs, collapse = ", "), "\n")



  invisible(x)
}

#' @rdname summary.stats_dm
#' @export
print.summary.delta_funs <- function(x, ...) {
  summary_obj <- x

  # call the higher-order sum_dist printing function, and drop probs
  print.summary.sum_dist(summary_obj, ...,
    drop_cols = c("ID", "Source", "Prob")
  )

  # print quantiles specific information
  cat("Probs:", paste(summary_obj$probs, collapse = ", "), "\n")

  invisible(x)
}


#' @rdname summary.stats_dm
#' @export
print.summary.fit_stats <- function(x, ...) {
  summary_obj <- x

  # call the higher-order stats_dm printing function, but drop id
  print.summary.stats_dm(summary_obj, ..., drop_cols = "ID")
  invisible(x)
}



#' @rdname summary.stats_dm
#' @export
print.summary.sum_dist <- function(x, ...) {
  summary_obj <- x
  print.summary.stats_dm(summary_obj, ...)
  cat("Sources:", paste(summary_obj$source, collapse = ", "), "\n")
  invisible(x)
}


#' @rdname summary.stats_dm
#' @export
print.summary.stats_dm_list <- function(x, ...) {
  summary_objs <- x
  for (idx in seq_along(summary_objs)) {
    one_summary_obj <- summary_objs[[idx]]
    cat("Summary of Element ", idx, ": ", one_summary_obj$type, sep = "")
    cat("\n\n")
    print(one_summary_obj, ..., show_header = FALSE)
    cat("-------\n")
    if (idx != length(summary_objs)) cat("\n")
  }

  invisible(x)
}
