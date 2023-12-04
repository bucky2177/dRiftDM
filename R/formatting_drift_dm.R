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

  cat("\nDiscretization:")
  cat("\n  solver:", drift_dm_obj$solver)
  to_str <- prms_to_str(
    prms = drift_dm_obj$prms_solve,
    names_prms = names(drift_dm_obj$prms_solve)
  )
  cat("\n  values:", to_str)
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
  print(summary_obj$obs_data)
  cat("\n")

  cat("\nNumber of Model Parameters:\n")
  print(summary_obj$nPrms)
  cat("\n")

  cat("\nModel Parameter Values:\n")
  print(summary_obj$prms_model)
  cat("\n")

  cat("\nFit Indices:\n")
  print(summary_obj$fit_stats)
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
      aic = drift_dm_obj$ic_vals[["AIC"]],
      bic = drift_dm_obj$ic_vals[["BIC"]]
    )
  }

  # if (F) { # to be implemented in future versions..
  #
  #   t_vec = seq(0, ans$prms_solve[["t_max"]], length.out = ans$prms_solve[["nt"]] + 1)
  #   x_vec = seq(-1, 1, length.out = ans$prms_solve[["nx"]] + 1)
  #
  #   # pdf for x
  #   ans$xs =
  #   lapply(ans$conds, function(one_cond, x_vec){
  #     pdf = drift_dm_obj$comp_funs$x_fun(drift_dm_obj = drift_dm_obj,
  #                                         x_vec = x_vec, one_cond = one_cond)
  #     return(make_text_pdf(x_vec, pdf))
  #   }, x_vec = x_vec)
  #   names(ans$xs) = ans$conds
  #
  #   # pdf for nt
  #   ans$nts =
  #     lapply(ans$conds, function(one_cond, t_vec){
  #       pdf = drift_dm_obj$comp_funs$nt_fun(drift_dm_obj = drift_dm_obj,
  #                                          t_vec = t_vec, one_cond = one_cond)
  #       return(make_text_pdf(t_vec, pdf))
  #     }, t_vec = t_vec)
  #   names(ans$nts) = ans$conds
  # }

  class(ans) <- "summary.drift_dm"
  ans
}



# === HELPER FUNCTIONS WHEN FORMATTING THINS ====
#
# make_text_pdf = function(x_vec, pdf) {
#
#   x_bins = cut(x_vec, breaks = seq(min(x_vec), max(x_vec), length.out = 11),
#                labels = F, include.lowest = T)
#   pdf_binned = tapply(pdf, x_bins, sum)
#   x_bins = unique(x_bins)
#   pdf_binned = pdf_binned / max(pdf_binned)
#
#   prob_cuts = seq(0, 1, length.out = 5)
#   string = c()
#   for (y in length(prob_cuts):2) {
#     to_print = pdf_binned > prob_cuts[y - 1]
#     symbols <- ifelse(pdf_binned > 0.5*prob_cuts[y - 1] + 0.5*prob_cuts[y],
#                       ":", ".")
#     for (x in 1:10) {
#       if (to_print[x]) {
#         string = paste0(string, symbols[x])
#       } else {
#         string = paste0(string, " ")
#       }
#     }
#     string = paste0(string, "\n")
#   }
#   return(string)
# }
