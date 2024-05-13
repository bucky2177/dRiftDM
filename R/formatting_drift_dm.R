# ==== Function for printing/summarizing information of drift_dm class

#' @export
print.drift_dm <- function(x, ...) {
  drift_dm_obj <- x
  cat(
    "Class(es):",
    paste(class(drift_dm_obj), collapse = ", ")
  )
  cat("\n")

  to_str <- prms_to_str(
    prms = drift_dm_obj$prms_model,
    names_prms = names(drift_dm_obj$prms_model)
  )
  cat("\nModel Parameters:")
  cat("\n  values:", to_str)
  cat(
    "\n  free:",
    paste(drift_dm_obj$free_prms, collapse = ", ")
  )
  cat("\n")


  cat(
    "\nConditions:",
    paste(drift_dm_obj$conds, collapse = ", ")
  )
  cat("\n")

  cat("\nDeriving PDFs:")
  cat("\n  solver:", drift_dm_obj$solver)
  to_str <- prms_to_str(
    prms = drift_dm_obj$prms_solve,
    names_prms = names(drift_dm_obj$prms_solve)
  )
  cat("\n  values:", to_str)
  cat("\n")

  cat("\n")
  cat("Observed Data: ")
  if (is.null(drift_dm_obj$obs_data)) {
    cat("NULL")
  } else {
    n_trials <- length(unlist(drift_dm_obj$obs_data))
    cat(n_trials, "trials")
  }
  cat("\n")
}


#' @export
print.summary.drift_dm <- function(x, ...) {
  summary_obj <- x

  cat(
    "Class(es):",
    paste(summary_obj$class, collapse = ", ")
  )
  cat("\n")

  cat("\nObserved Data:\n")
  print(round(summary_obj$obs_data, drift_dm_default_rounding()))
  cat("\n")

  cat("\nNumber of Model Parameters:\n")
  print(summary_obj$nPrms)
  cat("\n")

  cat("\nModel Parameter Values:\n")
  print(round(summary_obj$prms_model, drift_dm_default_rounding()))
  cat("\n")

  cat("\nFit Indices:\n")
  print(round(summary_obj$fit_stats, drift_dm_default_rounding()))
  cat("-------")

  cat("\n")
  cat(
    "\nConds:",
    paste(summary_obj$conds, collapse = ", ")
  )
  cat(
    "\nFree Parameters:",
    paste(summary_obj$free_prms, collapse = ", ")
  )
  cat("\nSolver:", summary_obj$solver)
  to_str <- prms_to_str(
    prms = summary_obj$prms_solve,
    names_prms = names(summary_obj$prms_solve)
  )
  cat("\nSettings:", to_str)
  cat("\n")
}

#' @export
summary.drift_dm <- function(object, ...) {
  drift_dm_obj <- object
  ans <- list()
  ans$class <- class(drift_dm_obj)
  ans$nPrms <- c(
    total = length(drift_dm_obj$prms_model),
    free = length(drift_dm_obj$free_prms)
  )
  ans$prms_model <- t(as.matrix(drift_dm_obj$prms_model))
  rownames(ans$prms_model) <- ""
  ans$conds <- drift_dm_obj$conds
  ans$prms_solve <- drift_dm_obj$prms_solve
  ans$free_prms <- drift_dm_obj$free_prms
  ans$solver <- drift_dm_obj$solver

  ans$obs_dat <- NULL
  if (!is.null(drift_dm_obj$obs_data)) {
    temp_summary <- function(x) {
      temp <- unclass(summary(x))
      temp <- c(temp, N = length(x))
      names(temp) <- tolower(names(temp))
      return(temp)
    }
    sum_correct <- sapply(
      drift_dm_obj$obs_data$rts_corr,
      temp_summary
    )
    sum_correct <- t(sum_correct)
    rownames(sum_correct) <- paste("correct", rownames(sum_correct))


    sum_error <- sapply(drift_dm_obj$obs_data$rts_err, temp_summary)
    sum_error <- t(sum_error)
    rownames(sum_error) <- paste("error", rownames(sum_error))

    ans$obs_data <- rbind(sum_correct, sum_error)
  }

  ans$fit_stats <- NULL
  if (!is.null(drift_dm_obj$log_like_val)) {
    ans$fit_stats <- c(
      `log(like)` = drift_dm_obj$log_like_val,
      aic = drift_dm_obj$ic_vals[["aic"]],
      bic = drift_dm_obj$ic_vals[["bic"]]
    )
  }

  class(ans) <- "summary.drift_dm"
  ans
}
