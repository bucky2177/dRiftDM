# ==== Function for printing/summarizing information of drift_dm class

#' @rdname drift_dm
#' @export
print.drift_dm <- function(x, ..., round_digits = drift_dm_default_rounding()) {
  drift_dm_obj <- x
  cat(
    "Class(es):",
    paste(class(drift_dm_obj), collapse = ", ")
  )
  cat("\n")


  cat("\n")
  print(drift_dm_obj$flex_prms_obj)


  cat("Deriving PDFs:")
  cat("\n  solver:", drift_dm_obj$solver)
  to_str <- prms_to_str(
    x = names(drift_dm_obj$prms_solve),
    prms = drift_dm_obj$prms_solve,
    round_digits = round_digits,
    sep = "=", collapse = ", "
  )
  cat("\n  values:", to_str)
  cat("\n")

  cat("\n")
  cat("Observed Data: ")
  if (is.null(drift_dm_obj$obs_data)) {
    cat("NULL")
  } else {
    n_trials <- lapply(drift_dm_obj$obs_data,lengths,)
    n_trials <- rowSums(do.call(cbind, n_trials))
    trials_out = paste(n_trials, "trials", names(n_trials))
    cat(paste(trials_out, collapse = "; "))
  }
  cat("\n")

  invisible(x)
}



#' Summary for `drift_dm` Objects
#'
#' Summary and printing methods for objects of the class `drift_dm`, resulting
#' from a call to [dRiftDM::drift_dm].
#'
#' @param object An object of class `drift_dm`
#' @param x an object of type `summary.drift_dm`
#' @param round_digits Integer specifying the number of decimal places for
#'   rounding in the printed summary. Default is 3.
#' @param ... additional arguments passed forward to the respective method
#'
#' @details
#' The `summary.drift_dm` function constructs a summary list with detailed
#' information about the `drift_dm` object, including:
#' - **class**: The class type of the `drift_dm` object.
#' - **summary_flex_prms**: A summary of [dRiftDM::flex_prms] object in the
#'   model.
#' - **prms_solve**: Parameters used for solving the model (see
#'    [dRiftDM::prms_solve]).
#' - **solver**: The solver used for model fitting.
#' - **obs_data**: A summary table of observed response time data, if available,
#'   by response type (upper/lower boundary responses). Includes sample size,
#'   mean, and quantiles.
#' - **fit_stats**: Fit statistics, if available, including log-likelihood,
#'   AIC, and BIC values.
#'
#' The `print.summary.drift_dm` function displays this summary in a formatted
#' way.
#'
#' @return
#' `summary.drift_dm` returns a list of class `summary.drift_dm` with the above
#' information.
#'
#' @export
summary.drift_dm <- function(object, ...) {
  drift_dm_obj <- object
  ans <- list()
  ans$class <- class(drift_dm_obj)
  ans$summary_flex_prms <- summary(drift_dm_obj$flex_prms_obj)
  ans$prms_solve <- drift_dm_obj$prms_solve
  ans$solver <- drift_dm_obj$solver

  ans$obs_data <- NULL
  b_coding <- attr(drift_dm_obj, "b_coding")
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

  ans$fit_stats <- NULL
  if (!is.null(drift_dm_obj$log_like_val)) {
    fit_stats = calc_stats(drift_dm_obj, type = "fit_stats")
    ans$fit_stats <- c(
      Log_Like = fit_stats[["Log_Like"]],
      AIC = fit_stats[["AIC"]],
      BIC = fit_stats[["BIC"]]
    )
  }

  class(ans) <- "summary.drift_dm"
  return(ans)
}


#' @rdname summary.drift_dm
#' @export
print.summary.drift_dm <- function(x, ...,
                                   round_digits = drift_dm_default_rounding()) {
  summary_obj <- x

  cat(
    "Class(es):",
    paste(summary_obj$class, collapse = ", ")
  )
  cat("\n")


  cat("\n")
  print(summary_obj$summary_flex_prms)


  cat("Observed Data:\n")
  obs_data_print <- summary_obj$obs_data
  if (!is.null(obs_data_print)) {
    obs_data_print <- round(obs_data_print, round_digits)
  }
  print(obs_data_print)
  cat("\n")



  cat("Fit Indices:\n")
  fit_stats_print <- summary_obj$fit_stats
  if (!is.null(fit_stats_print)) {
    fit_stats_print <- round(fit_stats_print, round_digits)

  }
  print(fit_stats_print)
  cat("-------")


  cat("\nSolver:", summary_obj$solver)
  to_str <- prms_to_str(
    x = names(summary_obj$prms_solve),
    prms = summary_obj$prms_solve,
    sep = "=", collapse = ", "
  )
  cat("\nSettings:", to_str)
  cat("\n")

  invisible(x)
}


