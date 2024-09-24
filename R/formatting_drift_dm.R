# ==== Function for printing/summarizing information of drift_dm class

#' @export
print.drift_dm <- function(x, round_digits = drift_dm_default_rounding(), ...) {
  drift_dm_obj <- x
  cat(
    "Class(es):",
    paste(class(drift_dm_obj), collapse = ", ")
  )
  cat("\n")

  to_str <- prms_to_str(
    prms = drift_dm_obj$prms_model,
    names_prms = names(drift_dm_obj$prms_model),
    round_digits = round_digits
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
    names_prms = names(drift_dm_obj$prms_solve),
    round_digits = round_digits
  )
  cat("\n  values:", to_str)
  cat("\n")

  cat("\n")
  cat("Observed Data: ")
  if (is.null(drift_dm_obj$obs_data)) {
    cat("NULL")
  } else {
    n_trials <- sapply(drift_dm_obj$conds, function(one_cond) {
        n_rts_u = length(unlist(drift_dm_obj$obs_data$rts_u[[one_cond]]))
        n_rts_l = length(unlist(drift_dm_obj$obs_data$rts_l[[one_cond]]))
        return(n_rts_u + n_rts_l)
      })
    trials_out = paste(n_trials, "trials", names(n_trials))
    cat(paste(trials_out, collapse = "; "))
  }
  cat("\n")
}


#' @export
print.summary.drift_dm <- function(x, round_digits = drift_dm_default_rounding(),
                                   ...) {
  summary_obj <- x

  cat(
    "Class(es):",
    paste(summary_obj$class, collapse = ", ")
  )
  cat("\n")

  cat("\nObserved Data:\n")
  obs_data_print <- summary_obj$obs_data
  if (!is.null(obs_data_print)) {
    obs_data_print <- round(obs_data_print, round_digits)
  }
  print(obs_data_print)
  cat("\n")

  cat("\nNumber of Model Parameters:\n")
  print(summary_obj$nPrms)
  cat("\n")

  cat("\nModel Parameter Values:\n")
  print(round(summary_obj$prms_model, round_digits))
  cat("\n")

  cat("\nFit Indices:\n")

  fit_stats_print <- summary_obj$fit_stats
  if (!is.null(fit_stats_print)) {
    fit_stats_print <- round(fit_stats_print, round_digits)
  }
  print(fit_stats_print)
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

  ans$obs_data <- NULL
  b_encoding <- attr(drift_dm_obj, "b_encoding")
  if (!is.null(drift_dm_obj$obs_data)) {
    temp_summary <- function(x) {
      temp <- unclass(summary(x))
      temp <- c(temp, N = length(x))
      names(temp) <- tolower(names(temp))
      return(temp)
    }
    sum_u <- sapply(
      drift_dm_obj$obs_data$rts_u,
      temp_summary
    )
    sum_u <- t(sum_u)

    rownames(sum_u) <- paste(names(b_encoding$u_name_value), rownames(sum_u))


    sum_l <- sapply(drift_dm_obj$obs_data$rts_l, temp_summary)
    sum_l <- t(sum_l)
    rownames(sum_l) <- paste(names(b_encoding$l_name_value), rownames(sum_l))

    ans$obs_data <- rbind(sum_u, sum_l)
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
  return(ans)
}
