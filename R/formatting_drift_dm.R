# ==== Function for printing/summarizing information of drift_dm class

#' @rdname drift_dm
#' @export
print.drift_dm <- function(x, ..., round_digits = drift_dm_default_rounding()) {
  drift_dm_obj <- x

  print_classes(class(drift_dm_obj))

  # to ensure backward compatibility -> deprecated, remove in the future
  v022 <- is.null(drift_dm_obj$cost_function)
  if (!v022) {
    print_estimate_info(drift_dm_obj$estimate_info)
  }

  cat("\n")
  print(drift_dm_obj$flex_prms_obj)

  print_deriving_pdfs(
    solver = drift_dm_obj$solver,
    prms_solve = drift_dm_obj$prms_solve
  )

  if (!v022) {
    cat("\n")
    print_cost_function(drift_dm_obj$cost_function)
  }

  cat("\n")
  if (is.null(drift_dm_obj$obs_data)) {
    cat("Observed Data: NULL")
  } else {
    n_trials <- lapply(drift_dm_obj$obs_data, lengths)
    n_trials <- rowSums(do.call(cbind, n_trials))
    print_trial_numbers(trials_vector = n_trials, round_digits = 0,
                        header = "Observed Data:")
  }
  cat("\n")

  invisible(x)
}



#' Summary for `drift_dm` objects
#'
#' summary and corresponding printing methods for objects of class `drift_dm`,
#' created by a call to [dRiftDM::drift_dm()].
#'
#' @param object an object of class `drift_dm`.
#' @param x an object of class `summary.drift_dm`.
#' @param round_digits integer, specifying the number of decimal places for
#'   rounding in the printed summary. Default is 3.
#' @param ... additional arguments passed forward (currently not used).
#'
#' @details
#' `summary.drift_dm()` constructs a summary list with information about the
#' `drift_dm` object. The returned list has class `summary.drift_dm` and can
#' include the following entries:
#'
#' - **class**: Class vector of the `drift_dm` object.
#' - **summary_flex_prms**: Summary of the [dRiftDM::flex_prms] object in the
#'   model (see [dRiftDM::summary.flex_prms]).
#' - **prms_solve**: Parameters used for solving the model (see
#'   [dRiftDM::prms_solve]).
#' - **solver**: Solver used for generating model predictions.
#' - **b_coding**: Boundary coding for the model (see [dRiftDM::b_coding]).
#' - **obs_data**: Summary table of observed response time data, if available,
#'   by response type (upper/lower boundary). rows correspond to upper first
#'   then lower responses; row names are prefixed by the boundary names from
#'   `b_coding`. columns (all lower-case) are: `min`, `1st qu.`, `median`,
#'   `mean`, `3rd qu.`, `max`, and `n`.
#' - **cost_function**: Name (or descriptor) of the cost function used during
#'   estimation.
#' - **fit_stats**: Fit statistics, if available. we return a named atomic
#'   vector created via `unlist(unpack_obj(calc_stats(..., type = "fit_stats")))`.
#' - **estimate_info**: Additional information about the estimation procedure.
#'
#' `print.summary.drift_dm()` displays this summary in a formatted way.
#'
#' @return
#' `summary.drift_dm()` returns a list of class `summary.drift_dm` (see details
#' for the entries).
#'
#' `print.summary.drift_dm()` returns invisibly the `summary.drift_dm` object.
#'
#' @examples
#' # get a pre-built model for demonstration
#' a_model <- dmc_dm(t_max = 1.5, dx = .01, dt = .005)
#' sum_obj <- summary(a_model)
#' print(sum_obj, round_digits = 2)
#'
#' # more information is provided when we add data to the model
#' obs_data(a_model) <- dmc_synth_data  # (data set comes with dRiftDM)
#' summary(a_model)
#'
#' # fit indices are added once we evaluate the model
#' a_model <- re_evaluate_model(a_model)
#' summary(a_model)
#'
#' @export
summary.drift_dm <- function(object, ...) {
  drift_dm_obj <- object
  ans <- list()
  ans$class <- class(drift_dm_obj)
  ans$summary_flex_prms <- summary(drift_dm_obj$flex_prms_obj)
  ans$prms_solve <- drift_dm_obj$prms_solve
  ans$solver <- drift_dm_obj$solver

  b_coding <- attr(drift_dm_obj, "b_coding")
  ans$b_coding <- b_coding

  if (!is.null(drift_dm_obj$obs_data)) {
    temp_summary <- function(x) {
      temp <- unclass(summary(x))
      temp <- c(temp, N = length(x))
      names(temp) <- tolower(names(temp))
      return(temp)
    }
    sum_u <- sapply(drift_dm_obj$obs_data$rts_u, temp_summary)
    sum_u <- t(sum_u)
    rownames(sum_u) <- paste(names(b_coding$u_name_value), rownames(sum_u))

    sum_l <- sapply(drift_dm_obj$obs_data$rts_l, temp_summary)
    sum_l <- t(sum_l)
    rownames(sum_l) <- paste(names(b_coding$l_name_value), rownames(sum_l))
    ans$obs_data <- rbind(sum_u, sum_l)
  }

  ans$cost_function <- drift_dm_obj$cost_function
  fit_stats <- calc_stats(drift_dm_obj, type = "fit_stats")
  if (!is.null(fit_stats)) {
    ans$fit_stats <- unlist(unpack_obj(fit_stats))
  }

  ans$estimate_info = drift_dm_obj$estimate_info

  class(ans) <- "summary.drift_dm"
  return(ans)
}


#' @rdname summary.drift_dm
#' @export
print.summary.drift_dm <- function(x, ...,
                                   round_digits = drift_dm_default_rounding()) {
  summary_obj <- x

  print_classes(summary_obj$class)
  estimate_info <- summary_obj$estimate_info
  if (!is.null(estimate_info)) print_estimate_info(estimate_info, long = TRUE)

  cat("\n")
  print(summary_obj$summary_flex_prms)


  cat("Observed Data:\n")
  obs_data_print <- summary_obj$obs_data
  if (!is.null(obs_data_print)) {
    obs_data_print <- round(obs_data_print, round_digits)
  }
  print(obs_data_print)
  cat("\n")


  print_fit_stats(summary_obj$fit_stats, round_digits)


  cat("\n-------\n")


  print_deriving_pdfs(
    solver = summary_obj$solver,
    prms_solve = summary_obj$prms_solve
  )

  cat("\nBoundary Coding:\n")
  u_name_value <- summary_obj$b_coding$u_name_value
  l_name_value <- summary_obj$b_coding$l_name_value
  upper_str <- paste("  upper:", names(u_name_value))
  lower_str <- paste("  lower:", names(l_name_value))
  in_data_str <- paste0(
    "  expected data column: ", summary_obj$b_coding$column,
    " (", names(u_name_value), " = ", u_name_value,
    "; ", names(l_name_value), " = ", l_name_value, ")"
  )
  cat(upper_str, "\n")
  cat(lower_str, "\n")
  cat(in_data_str, "\n")
  invisible(x)
}


# HELPER FUNCTIONS --------------------------------------------------------

print_deriving_pdfs = function(solver, prms_solve,
                               header = "Deriving PDFS:") {
  cat(header)
  cat("\n  solver:", solver)
  to_str <- prms_to_str(
    x = names(prms_solve),
    prms = prms_solve,
    sep = "=", collapse = ", "
  )
  cat("\n  values:", to_str)
  cat("\n")
}


print_classes = function(class_vector, header = "Class(es)") {
  cat(header, paste(class_vector, collapse = ", "))
  cat("\n")
}

print_cost_function = function(cost_function_label, header = "Cost Function:") {
  cat(header, cost_function_label)
  cat("\n")
}

print_fit_stats = function(fit_stats, round_digits, header = "Fit Indices:") {
  cat(header)
  cat("\n")
  print(fit_stats, digits = round_digits)
}


print_trial_numbers = function(trials_vector, round_digits,
                               header = "Average Trial Numbers:",
                               interim = "trials") {
  trials_out <- paste(
    round(trials_vector, digits = round_digits),
    interim,
    names(trials_vector)
  )
  cat(header, paste(trials_out, collapse = "; "))
  cat("\n")

}

print_estimate_info = function(estimate_info, long = FALSE) {

  if (is.null(estimate_info)) {
    cat("(model has not been estimated yet)\n")
    return(invisible(NULL))
  }

  cat("Optimizer:", estimate_info$optimizer)
  cat("\n")
  cat("Convergence:", estimate_info$conv_flag)
  cat("\n")
  if (isFALSE(estimate_info$conv_flag)) {
    cat("Message:", estimate_info$message)
    cat("\n")
  }
  if (isTRUE(long) & !is.na(estimate_info$n_iter)) {
    cat("Iterations:", estimate_info$n_iter)
    cat("\n")
  }
  if (isTRUE(long) & !is.na(estimate_info$n_eval)) {
    cat("Function Evaluations:", estimate_info$n_eval[1])
    cat("\n")
    if (length(estimate_info$n_eval) > 1) {
      cat("Gradient Evaluations:", estimate_info$n_eval[2])
      cat("\n")
    }
  }
}
