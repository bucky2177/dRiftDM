
# print traces_dm_list and traces_dm ----------------------------------------



#' @rdname simulate_traces
#' @export
print.traces_dm_list <- function(x, ...,
                                 round_digits = drift_dm_default_rounding(),
                                 print_steps = 5) {
  traces_dm_list_obj <- x

  # print class
  cat(
    "Class(es):",
    paste(class(traces_dm_list_obj), collapse = ", ")
  )
  cat("\n")


  # print out time space
  cat("\nTime space:\n")
  t_vec <- attr(traces_dm_list_obj, "t_vec")
  first_steps <- formatC(t_vec[1:(print_steps - 1)],
    format = "f",
    digits = round_digits
  )
  first_steps <- paste(first_steps, collapse = ", ")
  last_step <- formatC(t_vec[length(t_vec)],
    format = "f",
    digits = round_digits
  )
  all_steps <- paste(first_steps, "...", last_step)
  cat(all_steps, "\n")

  # print out traces for each cond
  for (one_cond in names(traces_dm_list_obj)) {
    cat("\nCondition:", one_cond, "\n")
    one_trace_obj <- unpack_obj(
      traces_dm_list_obj,
      unpack_elements = FALSE, conds = one_cond
    )
    print(one_trace_obj,
      round_digits = round_digits,
      print_steps = print_steps, ...
    )
  }

  invisible(x)
}


#' @rdname simulate_traces
#' @export
print.traces_dm <- function(x, ...,
                            round_digits = drift_dm_default_rounding(),
                            print_steps = 5, print_k = 4) {
  traces_dm_obj <- x

  # get all traces for one cond
  e_samples <- unpack_obj(object = traces_dm_obj, unpack_elements = TRUE)


  # find how many steps and rows to print
  k <- nrow(e_samples)
  if (k < print_k) print_k <- k

  w <- ncol(e_samples)
  if (w < print_steps) print_steps <- w

  # print each trace
  for (i in 1:print_k) {
    one_trace <- e_samples[i, ]

    first_steps <- formatC(one_trace[1:(print_steps - 1)],
      format = "f",
      digits = round_digits, flag = " "
    )
    first_steps <- paste(first_steps, collapse = ", ")
    n_not_na <- sum(!is.na(one_trace))
    last_step <- formatC(one_trace[n_not_na],
      format = "f",
      digits = round_digits, flag = " "
    )
    all_steps <- paste("~>", first_steps, "...", last_step)
    cat(all_steps, "\n")
  }

  # if traces were omitted, show that
  if (k > print_k) cat("...\n")

  invisible(x)
}



# summary traces_dm_list and traces_dm ------------------------------------

#' Summary for traces_dm and traces_dm_list Objects
#'
#' Summary and corresponding printing methods for `traces_dm` and
#' `traces_dm_list` objects, resulting from a call to
#' [dRiftDM::simulate_traces()]. Here, `traces_dm` objects are entries of the
#' returned list.
#'
#' @param object an object of class `traces_dm` or `traces_dm_list`.
#' @param x an object of type `summary.traces_dm` or `summary.traces_dm_list`.
#' @param round_digits integer, specifying the number of decimal places for
#' rounding in the printed summary. Default is 3.
#' @param ... additional arguments passed forward.
#'
#' @details
#' The `summary.traces_dm()` function constructs a summary list with
#' information about the `traces_dm` object, including:
#'
#' - **k**: The number of traces in the object.
#' - **add_x**: A logical, indicating whether starting values were added.
#' - **orig_model_class**: The class label of the original model.
#' - **orig_prms**: The parameters with which the traces were simulated (for
#'    the respective condition)
#' - **prms_solve**: The solver settings with which the traces were simulated.
#' - **fpt_desc**: A summary of the first passage times, including mean,
#'    standard deviation, and response probabilities for upper and lower
#'    boundaries.
#'
#' The `summary.traces_dm_list()` function constructs a summary list with
#' information about the `traces_dm_list` object, including:
#'
#' - **k**: A numeric vector, providing the number of traces per condition.
#' - **add_x**: A logical vector, indicating whether starting values were added
#'   for each condition.
#' - **orig_prms**: A matrix, containing the original parameter values per
#'   condition, with which the traces were simulated.
#' - **orig_model_class**: The class label of the original model
#' - **prms_solve**: A matrix of solver settings per condition.
#' - **fpt_desc**: A summary of the first passage times per condition, including
#'    mean, standard deviation, and response probabilities for the upper or
#'    lower boundary.
#'
#'
#' The `print.summary.traces_dm()` and  `print.summary.traces_dm_list()`
#' functions display the summary in a formatted way.
#'
#' @return
#' `summary.traces_dm()` returns a list of class `summary.traces_dm` (see the
#' Details section summarizing each entry of this list).
#'
#' `summary.traces_dm_list()` returns a list of class `summary.traces_dm_list`
#' (see the Details section summarizing each entry of this list).
#'
#' `print.summary.traces_dm()` returns the `summary.traces_dm` object invisibly.
#'
#' `print.summary.traces_dm_list()` returns the `summary.traces_dm_list` object
#' invisibly.
#'
#' @examples
#' # get a couple of traces a cross conditions
#' traces <- simulate_traces(dmc_dm(), k = c(5, 10))
#' summary(traces)
#'
#' # get a single traces object
#' one_traces_obj <- traces[[1]]
#' summary(one_traces_obj)
#'
#' @export
summary.traces_dm <- function(object, ...) {

  # one set of traces
  traces = object

  # store infos
  ans = list()
  ans$k = dim(traces)[1]

  attrbs = attributes(traces)
  ans <- c(ans, attrbs[c("add_x", "orig_model_class", "orig_prms",
                         "prms_solve")])

  # first passage times
  idx_fpt = apply(traces, 1, \(x) max(which(!is.na(x))))
  t_vec = seq(0, ans$prms_solve["t_max"], length.out = ans$prms_solve["nt"] + 1)
  ts_fpt = t_vec[idx_fpt]
  resp = sapply(seq_along(idx_fpt), \(i) sign(traces[i,idx_fpt[i]]))

  # responsees
  b_coding = attrbs$b_coding
  p_u = mean(resp == 1)
  fpt_desc = c(mean(ts_fpt), stats::sd(ts_fpt), p_u, 1 - p_u)
  names(fpt_desc) = c("mean", "sd",
                      paste0("p_", names(b_coding$u_name_value)),
                      paste0("p_", names(b_coding$l_name_value)))

  # add and pass back
  ans$fpt_desc = fpt_desc
  class(ans) <- "summary.traces_dm"
  return(ans)
}



#' @rdname summary.traces_dm
#' @export
print.summary.traces_dm <- function(x, ...,
                                   round_digits = drift_dm_default_rounding()) {

  summary_obj <- x

  bool = ifelse(summary_obj$add_x, "yes", "no")
  cat("Starting Points Added:", bool)
  cat("\n")

  cat("\nNumber of Traces:", summary_obj$k)
  cat("\n")

  cat("\nSummary of First Passage Times:\n")
  print(round(summary_obj$fpt_desc, round_digits))
  cat("\n")


  cat("\nOrginal Parameter Values:\n")
  print(summary_obj$orig_prms)
  cat("\n-------")

  cat(
    "\nOriginal Model Class(es):",
    paste(summary_obj$orig_model_class, collapse = ", ")
  )

  to_str <- prms_to_str(
    x = names(summary_obj$prms_solve),
    prms = round(summary_obj$prms_solve, round_digits),
    sep = "=", collapse = ", "
  )
  cat("\nSettings:", to_str)
  cat("\n")


  invisible(x)
}


#' @rdname summary.traces_dm
#' @export
summary.traces_dm_list = function(object, ...){


  traces_list = object
  ans <- list()
  summaries = lapply(traces_list, summary)

  # number of traces per condition and conditions
  ks = lapply(summaries, \(x) x[["k"]])
  ans$k = unlist(ks)

  # add_x
  add_xs = lapply(summaries, \(x) x[["add_x"]])
  ans$add_x = unlist(add_xs)

  # parameters
  prms = sapply(summaries, \(x) x[["orig_prms"]])
  ans$orig_prms = t(prms)

  # orig_model_class (should always be the same, as this is not an argument to
  # simulate_traces)
  ans$orig_model_class = summaries[[1]][["orig_model_class"]]

  # prms_solve (maybe sigma can be variable in future version)
  prms_solve = sapply(summaries, \(x) x[["prms_solve"]])
  ans$prms_solve = t(prms_solve)


  # fpt summaries
  fpt_desc = sapply(summaries, \(x) x[["fpt_desc"]])
  ans$fpt_desc = t(fpt_desc)


  # pass back
  class(ans) <- "summary.traces_dm_list"
  return(ans)
}


#' @rdname summary.traces_dm
#' @export
print.summary.traces_dm_list <- function(x, ...,
                                    round_digits = drift_dm_default_rounding()) {

  summary_obj <- x

  bool = ifelse(summary_obj$add_x, "yes", "no")
  cat("Starting Points Added:\n")
  print(noquote(bool))
  cat("\n")

  cat("\nNumber of Traces:\n")
  print(summary_obj$k)
  cat("\n")

  cat("\nSummary of First Passage Times:\n")
  print(round(summary_obj$fpt_desc, round_digits))
  cat("\n")


  cat("\nOrginal Parameter Values:\n")
  print(round(summary_obj$orig_prms, round_digits))
  cat("\n-------")

  cat(
    "\nOriginal Model Class(es):",
    paste(summary_obj$orig_model_class, collapse = ", ")
  )

  cat("\n")
  cat("\nSettings:\n")
  print(round(summary_obj$prms_solve, round_digits))
  cat("\n")


  invisible(x)
}
