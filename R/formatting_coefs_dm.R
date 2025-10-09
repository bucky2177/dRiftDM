# coef.fits_ids_dm points to coef.drift_dm
#' @rdname coef.drift_dm
#' @export
print.coefs_dm <- function(x, ...,
                           round_digits = drift_dm_default_rounding(),
                           print_rows = 10, some = FALSE,
                           show_header = TRUE, show_note = TRUE) {
  if (show_header) {
    cat("Object Type:", class(x)[1])
    cat("\n\n")
  }

  coefs_dm_obj <- x
  n_row <- nrow(coefs_dm_obj)

  # coefs_dm objects are always of type data.frame
  idx_numeric <- sapply(coefs_dm_obj, is.numeric)
  coefs_dm_obj[idx_numeric] <- lapply(
    coefs_dm_obj[idx_numeric], round,
    digits = round_digits
  )

  # if some = TRUE, then randomly select some of the rows
  if (some) {
    idxs <- sort(sample(x = n_row, size = min(print_rows, n_row)))
    coefs_dm_obj <- coefs_dm_obj[idxs, , drop = FALSE]
  }


  # print the result
  print.data.frame(utils::head(coefs_dm_obj, n = print_rows))
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



# summary functions -------------------------------------------------------

#' Summary for `coefs_dm` Objects
#'
#' Summary and corresponding printing methods for `coefs_dm` objects. These
#' objects result from a call to [dRiftDM::coef.fits_ids_dm()] (i.e., when
#' calling `coef()` with an object of type `fits_ids_dm`).
#'
#' @param object an object of type `coefs_dm`.
#' @param x an object of class `summary.coefs_dm`.
#' @inheritParams summary.stats_dm
#'
#' @return
#'  For `summary.coefs_dm()` a summary object of class `summary.coefs_dm`.
#'
#'  For `print.summary.coefs_dm()`, the supplied object is returned
#'  invisibly.
#'
#' @details
#'
#' `summary.coefs_dm()` summarizes `coefs_dm` objects, returning the type,
#'   a summary of the underlying [data.frame] (`summary_dataframe`), and the
#'   number of unique IDs (`n_ids`).
#'
#' @examples
#' # get a fits_ids object for demonstration purpose
#' fits_ids <- get_example_fits("fits_ids_dm")
#' coefs <- coef(fits_ids)
#' summary(coefs)
#'
#' @export
summary.coefs_dm <- function(object, ...,
                             round_digits = drift_dm_default_rounding()) {
  coefs_obj <- object
  ans <- list()

  ans$type <- class(coefs_obj)[1]
  ans$summary_dataframe <- summary.data.frame(
    coefs_obj,
    digits = round_digits
  )

  ans$n_ids <- length(unique(coefs_obj$ID))

  class(ans) <- "summary.coefs_dm"
  return(ans)
}


#' @rdname summary.coefs_dm
#' @export
print.summary.coefs_dm <- function(x, ..., show_header = TRUE) {
  summary_obj <- x


  if (show_header) {
    cat("Object Type:", summary_obj$type)
    cat("\n\n")
  }

  # print the table summarizing the underlying data.frame
  all_cols <- colnames(summary_obj$summary_dataframe)
  keep_cols <- trimws(all_cols) != "ID"
  show_cols_summary_table <- all_cols[keep_cols]

  cat("Parameters:\n")
  print(summary_obj$summary_dataframe[, show_cols_summary_table, drop = F], ...)
  cat("\n")

  # if possible, show the number of IDs
  cat("N IDs:", summary_obj$n_ids, "\n")

  invisible(x)
}
