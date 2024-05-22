# === FOR WORKING ON OBJECTS OF TYPE dm_fits_ids

#' Get model values across individuals
#'
#' @description
#'  `gather_*` functions iterate over the individual model fits in
#'  `fits_ids`, returning a data.frame of the requested parameters or
#'  statistics:
#'
#'  - `gather_parameters`: Gathers the (estimated) model parameters
#'
#'  - `gather_stats` calls [dRiftDM::calc_stats] repeatedly for each individual.
#'
#'
#' @param fits_ids an object inheriting from `dm_fits_ids`, see
#'  [dRiftDM::load_fits_ids]
#'
#' @param fit_tats logical, indicating if `gather_parameters` should also return
#'  fit indices.
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
#' @details
#'
#' `gather_parameters` returns the model parameters, the log-likelihood, and
#' AIC/BIC values as a data.frame
#'
#' `gather_stats` calls [dRiftDM::calc_stats], and further arguments via ...
#' can be passed forward. The return value is determined by [dRiftDM::calc_stats]
#' and can be controlled via the `type` argument (see the documentation
#' [dRiftDM::calc_stats] for more information). In any case, the returned
#' value will contain the statistics as a data.frame, separately for each
#' individual.
#'
#' @export
gather_parameters <- function(fits_ids, fit_stats = T) {
  if (!inherits(fits_ids, "dm_fits_ids")) {
    stop("fits_ids not of type dm_fits_ids")
  }

  prms <- sapply(fits_ids$all_fits, function(x) {
    prms_one_model <- x$prms_model
    if (fit_stats) {
      prms_one_model <- c(prms_one_model, log_like = x$log_like_val, x$ic_vals)
    }
    return(prms_one_model)
  })
  prms <- t(prms)
  prms <- as.data.frame(prms)
  prms <- cbind(ID = rownames(prms), prms)
  prms$ID <- tryCatch(
    as.numeric(prms$ID),
    error = function(e) prms$ID,
    warning = function(e) prms$ID
  )
  prms <- prms[order(prms$ID), ]
  rownames(prms) <- NULL
  return(prms)
}



#' @rdname gather_parameters
#' @export
gather_stats <- function(fits_ids, type, verbose = 0, ...) {
  if (!(verbose %in% c(0, 1))) {
    stop("verbose must be 0 or 1")
  }

  if (!inherits(fits_ids, "dm_fits_ids")) {
    warning("fits_ids not of type dm_fits_ids, calling calc_stats()")
    calc_stats(drift_dm_obj = fits_ids, type = type, source = "both")
  }

  # create a progress bar
  if (verbose == 1) {
    n_iter <- length(fits_ids$all_fits)
    pb <- progress::progress_bar$new(
      format = "calculating [:bar] :percent; done in: :eta",
      total = n_iter, clear = FALSE, width = 60
    )
    pb$tick(0)
  }

  # call statistics across individuals
  all_stats <-
    lapply(names(fits_ids$all_fits), function(one_id) {
      one_model <- fits_ids$all_fits[[one_id]]
      stats_one_model <- calc_stats(drift_dm_obj = one_model, type = type, ...)

      if (is.list(stats_one_model) & is.data.frame(stats_one_model)) {
        stats_one_model <- data.frame(c(ID = one_id, stats_one_model))
      } else if (is.list(stats_one_model)) {
        stats_one_model <- lapply(stats_one_model, function(x) {
          return(data.frame(c(ID = one_id, x)))
        })
      } else {
        stop("unexpected return value")
      }
      if (verbose == 1) pb$tick()
      return(stats_one_model)
    })

  # combine that stats across individuals
  n_stats <- sapply(all_stats, function(one_entry) {
    if (is.data.frame(one_entry)) {
      return(1)
    } else {
      return(length(one_entry))
    }
  })
  n_stats <- unique(n_stats)
  stopifnot(length(n_stats) == 1)


  final_stats <-
    lapply(1:n_stats, function(inner_entry_index) {
      inner_entries <- lapply(all_stats, function(x) {
        if (is.data.frame(x)) {
          return(x)
        }
        x[[inner_entry_index]]
      })
      inner_entries <- do.call("rbind", inner_entries)
      return(inner_entries)
    })

  if (length(final_stats) == 1) {
    return(final_stats[[1]])
  } else {
    names(final_stats) <- type
    return(final_stats)
  }
}
