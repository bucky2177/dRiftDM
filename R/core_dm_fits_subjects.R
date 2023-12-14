# === FOR WORKING ON OBJECTS OF TYPE dm_fits_subjects

#' Get model values across subjects
#'
#' @description
#'  `gather_*` functions iterate over the individual model fits in
#'  `fits_subjects`, returning a data.frame of the requested parameters or
#'  statistics:
#'
#'  - `gather_parameters`: Gathers the (estimated) model parameters
#'
#'  - `gather_stats` calls [dRiftDM::calc_stats] repeatedly for each subject.
#'
#'
#' @param fits_subjects an object inheriting from `dm_fits_subjects`, see
#'  [dRiftDM::load_fits_subjects]
#'
#' @param type character vector when calling [dRiftDM::calc_stats]
#'
#' @param verbose optional, integer, indicating if information about the progress
#'  should be displayed. Can be set for some of the `gather_*` functions.
#'  0 -> no information, 1 -> a progress bar. Default is 0.
#'
#'
#' @param ... further arguments which are passed further down
#'
#'  @details
#'
#'  `gather_parameters` returns the model parameters, the log-likelihood, and
#'  AIC/BIC values as a data.frame
#'
#'  `gather_stats` calls [dRiftDM::calc_stats], and further arguments via ...
#'  can be passed. The return value is determined by [dRiftDM::calc_stats] and
#'  can be controlled via the `type` argument (see the documentation
#'  [dRiftDM::calc_stats] for more information). In any case, the returned
#'  value will contain the statistics as a data.frame, separately for each
#'  subject.
#'
#' @export
gather_parameters <- function(fits_subjects) {
  if (!inherits(fits_subjects, "dm_fits_subjects")) {
    stop("fits_subjects not of type dm_fits_subjects")
  }

  prms <- sapply(fits_subjects$all_fits, function(x) {
    prms_one_model <- x$prms_model
    prms_one_model <- c(prms_one_model, log_like = x$log_like_val, x$ic_vals)
    return(prms_one_model)
  })
  prms <- t(prms)
  prms <- as.data.frame(prms)
  prms <- cbind(Subject = rownames(prms), prms)
  rownames(prms) <- NULL
  return(prms)
}



#' @rdname gather_parameters
#' @export
gather_stats = function(fits_subjects, type, verbose = 0, ...) {
  if (!(verbose %in% c(0,1)))
    stop("verbose must be 0 or 1")

  if (!inherits(fits_subjects, "dm_fits_subjects")) {
    stop("fits_subjects not of type dm_fits_subjects")
  }

  # create a progress bar
  if (verbose == 1) {
    n_iter = length(fits_subjects$all_fits)
    pb <- progress::progress_bar$new(
      format = "calculating [:bar] :percent; done in: :eta",
      total = n_iter, clear = FALSE, width = 60
    )
  }

  # call statistics across subjects
  all_stats <-
    lapply(names(fits_subjects$all_fits), function(one_sbj) {
      if (verbose == 1) pb$tick()
      one_model = fits_subjects$all_fits[[one_sbj]]
      stats_one_model <- calc_stats(drift_dm_obj = one_model, type = type, ...)

      if (is.list(stats_one_model) & is.data.frame(stats_one_model)) {
        stats_one_model = data.frame(c(Subject = one_sbj, stats_one_model))
      } else if (is.list(stats_one_model)) {
        stats_one_model = lapply(stats_one_model, function(x){
          return(data.frame(c(Subject = one_sbj, x)))
        })
      } else {
        stop("unexpected return value")
      }
      return(stats_one_model)
    })

  # combine that stats across subjects
  n_stats = sapply(all_stats, function(one_entry){
    if (is.data.frame(one_entry)) {
      return(1)
    } else {
      return(length(one_entry))
    }
  })
  n_stats = unique(n_stats)
  stopifnot(length(n_stats) == 1)


  final_stats =
    lapply(1:n_stats, function(inner_entry_index){
      inner_entries = lapply(all_stats, function(x){
        if (is.data.frame(x))
          return(x)
        x[[inner_entry_index]]
      })
      inner_entries = do.call("rbind", inner_entries)
      return(inner_entries)
    })

  if (length(final_stats) == 1) {
    return(final_stats[[1]])
  } else {
    names(final_stats) = type
    return(final_stats)
  }
}
