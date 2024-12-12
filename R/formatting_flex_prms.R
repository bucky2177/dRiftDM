#' Summarizing Flex Parameters
#'
#' summary method for class "flex_prms".
#'
#' @param object an object of class "flex_prms", resulting from a call to
#' [dRiftDM::flex_prms].
#' @param x an object of class "summary.flex_prms"; a result of a call to
#' summary.flex_prms.
#' @param round_digits integer, indicating the number of decimal places (round)
#'  to be used (default is 3).
#' @param dependencies  logical, controlling if a summary of the special
#' dependencies shall be printed (see the "special dependency
#' instruction" in the details of [dRiftDM::flex_prms])
#' @param cust_parameters logical, controlling if a summary of the custom
#' parameters shall be printed (see the "additional/custom parameter
#' instruction" in the details of [dRiftDM::flex_prms])
#' @param ... additional arguments passed forward to the respective method
#'
#' @details
#' The `summary.flex_prms` function creates a summary object containing:
#' - **prms_matrix**: All parameter values across all conditions.
#' - **unique_matrix**: A character matrix, showing how parameters relate across
#' conditions.
#' - **depend_strings**: Special Dependencies, formatted as a string.
#' - **cust_prms_matrix**: (if they exist), a matrix containing all custom
#' parameters.
#'
#' The `print.summary.flex_prms` function displays the summary object in a
#' formatted manner.
#'
#' @export
summary.flex_prms <- function(object, ...) {
  flex_prms_obj <- object

  ans <- list()

  # prms_matrix and internal_list
  ans$prms_matrix <- flex_prms_obj$prms_matrix
  ans$unique_matrix <- internal_list_to_matrix(
    flex_prms_obj$linear_internal_list
  )


  # special dependency strings -> re-built from the expressions
  depend_strings <- NULL
  for (prm in names(flex_prms_obj$internal_list)) {
    for (cond in names(flex_prms_obj$internal_list[[prm]])) {
      cur_val <- flex_prms_obj$internal_list[[prm]][[cond]]
      if (is.expression(cur_val)) {
        cur_val <- as.character(cur_val)
        cur_val <- gsub(
          'prms_matrix\\["(\\w+)",\\s*"(\\w+)"\\]', "\\2 ~ \\1",
          cur_val
        )
        depend_strings <- append(
          depend_strings,
          paste(prm, "~", cond, "==", cur_val)
        )
      }
    }
  }
  ans$depend_strings <- depend_strings

  # custom parameter matrix (if they exist)
  if (!is.null(flex_prms_obj$cust_prms)) {
    cust_prms_matrix <- lapply(
      flex_prms_obj$cust_prms$values,
      \(x) return(x)
    )
    ans$cust_prms_matrix <- do.call(cbind, cust_prms_matrix)
  }

  class(ans) <- "summary.flex_prms"
  return(ans)
}

#' @rdname summary.flex_prms
#' @export
print.summary.flex_prms <- function(x, ...,
                                    round_digits = drift_dm_default_rounding(),
                                    dependencies = T, cust_parameters = T) {
  summary_obj <- x

  cat("Current Parameter Matrix:\n")
  prm_mat <- summary_obj$prms_matrix
  print(round(prm_mat, round_digits))
  cat("\n")


  cat("Unique Parameters:\n")
  unique_mat <- summary_obj$unique_matrix
  print(noquote(unique_mat))
  cat("\n")

  if (dependencies & !is.null(summary_obj$depend_strings)) {
    cat("Special Dependencies:\n")
    depend_string <- paste(summary_obj$depend_strings, collapse = "\n")
    cat(noquote(depend_string))
    cat("\n\n")
  }


  cust_prms_matrix <- summary_obj$cust_prms_matrix
  if (cust_parameters & !is.null(cust_prms_matrix)) {
    cat("Custom Parameters:\n")
    print(round(cust_prms_matrix, round_digits))
    cat("\n")
  }

  invisible(x)
}


#' @rdname flex_prms
#' @export
print.flex_prms <- function(x, ..., round_digits = drift_dm_default_rounding(),
                            dependencies = T, cust_parameters = T) {
  flex_prms_obj <- x
  print(summary(flex_prms_obj),
    round_digits = round_digits,
    dependencies = dependencies
  )
}
