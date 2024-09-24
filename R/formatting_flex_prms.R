#' Summarizing Flex Parameters
#'
#' summary method for class "flex_prms".
#'
#' @param object an object of class "flex_prms", resulting from a call to
#' [dRiftDM::flex_prms].
#' @param x an object of class "summary.flex_prms"; a result of a call to
#' summary.flex_prms.
#' @param round_digits integer, indicating the number of decimal places (round)
#'  to be used (default is 3)
#' @param dependencies  logical, controlling if a summary of the special
#' dependencies shall be printed (see the "special dependency
#' instruction" in the details of [dRiftDM::flex_prms])
#' @param cust_parameters logical, controlling if a summary of the custom
#' parameters shall be printed (see the "additional/custom parameter
#' instruction" in the details of [dRiftDM::flex_prms])
#'
#' @details
#' `print.summary.flex_prms` and [dRiftDM::print.flex_prms] are identical. Both
#' will print out information about:
#'
#' * The actual parameter values currently set
#' * Unique parameters, which also inform about restrains, fixations, or
#'   special dependencies
#' * (if applicable) A list of all special dependencies and how they are
#'    defined as strings
#' * (if applicable) A matrix of the custom paramters and their current values
#'
#' @returns The function `summary.flex_prms` computes and returns a list of
#' summary statistics of the `flex_prms` object given in `object`. The result
#' contains:
#'
#' * `prms_matrix`: All parameter values (unrounded) across all conditions with
#'   conditions as rows and the actual parameters as columns.
#' * `unique_matrix`: A matrix with either numbers or "d"s, indicating how each
#'    parameter varies across conditions. The maximum number of this matrix also
#'    indicates the maximum number of parameters of the model. The "d"s indicate
#'    if for specifc parameter x condition combination, special dependencies are
#'    set
#' * `depend_strings`: A vector of strings with all special dependencies,
#'    in a readable format (non-existant if no dependencies exist)
#' * `cust_prms_matrix`: A matrix of the custom paramters and their current
#'    values (non-existant if no custom parameters exist)
#'
#' @seealso [dRiftDM::flex_prms]
#'
#' @export
summary.flex_prms <- function(object) {
  flex_prms_obj <- object

  ans <- list()

  # prms_matrix and internal_list
  ans$prms_matrix <- flex_prms_obj$prms_matrix
  ans$unique_matrix <- internal_list_to_matrix(
    flex_prms_obj$linear_internal_list
  )


  # special dependency strings -> re-built from the expressions
  depend_strings = NULL
  for (prm in names(flex_prms_obj$internal_list)) {
    for (cond in names(flex_prms_obj$internal_list[[prm]])) {
      cur_val = flex_prms_obj$internal_list[[prm]][[cond]]
      if (is.expression(cur_val)) {
        cur_val = as.character(cur_val)
        cur_val = gsub('prms_matrix\\["(\\w+)",\\s*"(\\w+)"\\]', "\\2 ~ \\1",
                       cur_val)
        depend_strings = append(depend_strings,
                               paste(prm, "~", cond, "==", cur_val))
      }
    }
  }
  ans$depend_strings = depend_strings

  # custom parameter matrix (if they exist)
  if (!is.null(flex_prms_obj$cust_prms)) {
    ans$cust_prms_matrix = sapply(flex_prms_obj$cust_prms$values,
                                  \(x) return(x))
  }

  class(ans) <- "summary.flex_prms"
  return(ans)
}


#' @rdname flex_prms
#' @export
print.flex_prms <- function(x, round_digits = drift_dm_default_rounding(),
                            dependencies = T, cust_parameters = T) {
  flex_prms_obj <- x
  print(summary(flex_prms_obj),
        round_digits = round_digits,
        dependencies = dependencies)
}

#' @rdname summary.flex_prms
#' @export
print.summary.flex_prms <- function(x,
                                    round_digits = drift_dm_default_rounding(),
                                    dependencies = T, cust_parameters = T) {
  summary_obj <- x

  cat("Current Parameter Matrix:\n")
  print(round(summary_obj$prms_matrix, round_digits))
  cat("\n")


  cat("Unique Parameters:\n")
  print(noquote(summary_obj$unique_matrix))
  cat("\n")

  if (dependencies & !is.null(summary_obj$depend_strings)) {
    cat("Special Dependencies:\n")
    depend_string = paste(summary_obj$depend_strings, collapse = "\n")
    print(noquote(depend_string))
    cat("\n")
  }


  if (cust_parameters & !is.null(summary_obj$cust_prms_matrix)) {
    cat("Custom Parameters:\n")
    print(round(summary_obj$cust_prms_matrix, round_digits))
    cat("\n")
  }

}




#### HELPER FUNCTIONS  #########


internal_list_to_matrix = function(internal_list) {

  prms = names(internal_list)
  conds = names(internal_list[[1]])

  values = mapply(function(cond, prm){

    value = internal_list[[prm]][[cond]]
    if (is.numeric(value))
      return(value)
    return("d")

  }, conds, rep(prms, each = length(conds)))
  matrix = matrix(values, nrow = length(conds))
  rownames(matrix) = conds
  colnames(matrix) = prms

  return(matrix)
}
