
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
  t_vec = attr(traces_dm_list_obj, "t_vec")
  first_steps = formatC(t_vec[1:(print_steps - 1)], format = "f",
                        digits = round_digits)
  first_steps = paste(first_steps, collapse = ", ")
  last_step = formatC(t_vec[length(t_vec)], format = "f",
                        digits = round_digits)
  all_steps = paste(first_steps, "...", last_step)
  cat(all_steps, "\n")

  # print out traces for each cond
  for (one_cond in names(traces_dm_list_obj)) {

    cat("\nCondition:", one_cond, "\n")
    one_trace_obj = unpack_traces(
      traces_dm_list_obj, unpack = F, conds = one_cond
    )
    print(one_trace_obj, round_digits = round_digits,
          print_steps = print_steps, ...)
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
  e_samples = unpack_traces(object = traces_dm_obj, unpack = T)


  # find how many steps and rows to print
  k = nrow(e_samples)
  if (k < print_k) print_k = k

  w = ncol(e_samples)
  if (w < print_steps) print_steps = w

  # print each trace
  for (i in 1:print_k) {

    one_trace = e_samples[i,]

    first_steps = formatC(one_trace[1:(print_steps-1)], format = "f",
                          digits = round_digits, flag = " ")
    first_steps = paste(first_steps, collapse = ", ")
    n_not_na = sum(!is.na(one_trace))
    last_step = formatC(one_trace[n_not_na], format = "f",
                        digits = round_digits, flag = " ")
    all_steps = paste("~>", first_steps, "...", last_step)
    cat(all_steps, "\n")
  }

  # if traces were omitted, show that
  if (k > print_k) cat("...\n")

  invisible(x)
}
