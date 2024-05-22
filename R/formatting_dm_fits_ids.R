# ==== Function for printing/summarizing information of drift_dm class

#' @export
print.dm_fits_ids <- function(x, ...) {
  fits_ids <- x

  cat("Fit procedure name:", fits_ids$drift_dm_fit_info$fit_procedure_name)
  cat("\n")
  cat(
    "Fitted model type:",
    paste(
      class(fits_ids$drift_dm_fit_info$drift_dm_obj),
      collapse = ", "
    )
  )
  cat("\n")
  cat(
    "Time of (last) call:",
    format(fits_ids$drift_dm_fit_info$time_call, "%Y-%m-%d %H:%M:%S")
  )
  cat("\n")
  cat("N Individuals:", length(fits_ids$all_fits), "\n")
}




#' @export
print.summary.dm_fits_ids <- function(x, round_digits = 3, ...) {
  summary_obj <- x
  cat("Fit procedure name:", summary_obj$fit_procedure_name)
  cat("\n")
  cat("N Individuals:", summary_obj$n, "\n\n")


  cat("Parameter summary:\n")
  temp <- round(summary_obj$stats, round_digits)
  print(temp)
  cat("\n")

  cat("Parameter space:\n")
  temp <- rbind(summary_obj$lower, summary_obj$upper)
  rownames(temp) <- c("lower", "upper")
  colnames(temp) <- summary_obj$free_prms
  print(temp)

  cat("\n-------\n")
  cat("Fitted model type:", summary_obj$model_type)
  cat("\n")
  cat(
    "Time of (last) call:",
    format(summary_obj$time_call, "%Y-%m-%d %H:%M:%S")
  )
  cat("\n")
}

#' @export
summary.dm_fits_ids <- function(object, ...) {
  fits_ids <- object
  ans <- list()
  ans$fit_procedure_name <- fits_ids$drift_dm_fit_info$fit_procedure_name
  ans$time_call <- fits_ids$drift_dm_fit_info$time_call
  ans$lower <- fits_ids$drift_dm_fit_info$lower
  ans$upper <- fits_ids$drift_dm_fit_info$upper
  ans$model_type <- paste(
    class(fits_ids$drift_dm_fit_info$drift_dm_obj),
    collapse = ", "
  )
  ans$free_prms <- fits_ids$drift_dm_fit_info$drift_dm_obj$free_prms
  ans$prms <- gather_parameters(fits_ids)

  means <- apply(ans$prms[colnames(ans$prms) != "ID"], 2, mean)
  sds <- apply(ans$prms[colnames(ans$prms) != "ID"], 2, stats::sd)
  errs <- apply(
    ans$prms[colnames(ans$prms) != "ID"], 2,
    function(x) stats::sd(x) / sqrt(length(x))
  )
  ans$stats <- rbind(means, sds, errs)
  ans$n <- nrow(ans$prms)

  class(ans) <- "summary.dm_fits_ids"
  return(ans)
}
