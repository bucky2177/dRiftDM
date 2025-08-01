# Functions for calculating the log_likelihood ----------------------------

#' Calculate the Log-Likelihood
#'
#' Wrapper function around `log_like_heart`
#'
#' @param pdfs a list of pdfs (see details)
#' @param t_vec time space
#' @param obs_data a list of obs_data
#' @param pdf_u,pdf_l numeric vectors of the pdfs (unpacked)
#' @param rts_u,rts_l numeric vectors of the observed RTs (unpacked)
#'
#' @details
#'
#' ## calc_log_like
#'
#' Iterates over all conditions, and passes forward the (unpacked) arguments
#' to `log_like_heart`, adding each log-likelihood of a condition.
#'
#' `pdfs` must be a list with entries named as the conditions, and then
#' each condition being a list of the two PDFs (named pdf_u and pdf_l)
#'
#' `obs_data` must be a list with entries "rts_u" and "rts_l", and then
#' each rts_* entry being a named list with the RT values for each condition
#'
#' ## log_like_heart
#'
#' Gets the density values for RTs in rts_u/rts_l via [stats::approx()],
#' takes the log of that, and then sums across both.
#' Wraps up the calculation in a tryCatch statement, throwing warnings when
#' log_like_values can not be calculated
#'
#'
#' @returns a single value of the log-likelihood. If no data is provided,
#'   `NULL` is returned. If the calculation fails, `-Inf` is returned.
#'
#' @keywords internal
calc_log_like <- function(pdfs, t_vec, obs_data) {
  if (is.null(obs_data)) {
    return(NULL)
  }

  log_like <- 0

  for (one_cond in names(pdfs)) {
    log_like <- log_like + log_like_heart(
      pdf_u = pdfs[[one_cond]]$pdf_u,
      pdf_l = pdfs[[one_cond]]$pdf_l,
      t_vec = t_vec,
      rts_u = obs_data$rts_u[[one_cond]],
      rts_l = obs_data$rts_l[[one_cond]]
    )
  }

  return(log_like)
}

#' @rdname calc_log_like
log_like_heart <- function(pdf_u, pdf_l, t_vec, rts_u, rts_l) {
  tryCatch(
    expr = {
      app_like_u <- stats::approx(
        x = t_vec, y = pdf_u,
        xout = rts_u
      )$y
      app_like_l <- stats::approx(
        x = t_vec, y = pdf_l,
        xout = rts_l
      )$y

      log_like <- sum(log(app_like_u)) + sum(log(app_like_l))
      if (is.nan(log_like)) { # log(0) gives -Inf
        if (min(app_like_u) < 0 | min(app_like_l) < 0) {
          warning(
            "negative density values encountered",
            " when calculating the log-likelihood"
          )
        }
        return(-Inf)
      }
      return(log_like)
    },
    error = function(e) {
      warning("we encountered an untreated error! ", conditionMessage(e))
      return(-Inf)
    }
  )
}


# HELPER FUNCTIONS --------------------------------------------------------

# FUNCTIONS FOR CALCULATING RMSE -----------------------------------------

calc_rmse <- function(pdfs, t_vec, dt, stats_agg, stats_agg_info,
                       weight_err = 1.5) {

  # if no observed stats are present, return NULL
  if (is.null(stats_agg)) {
    return(NULL)
  }

  # get the observed quantiles and cafs
  quantiles_obs <- unlist(lapply(stats_agg, \(x) x$quantiles_corr))
  cafs_obs <- unlist(lapply(stats_agg, \(x) x$cafs))


  # get the cafs
  cafs_pred <- lapply(names(pdfs), \(one_cond) {
    tryCatch(
      expr = {
        calc_cafs_pred(
          pdf_u = pdfs[[one_cond]]$pdf_u,
          pdf_l = pdfs[[one_cond]]$pdf_l,
          t_vec = t_vec,
          one_cond = one_cond,
          n_bins = stats_agg_info[[one_cond]]$n_bins
        )[["P_U"]]
      },
      error = function(e){
        NA
      }
    )
  })

  # get the predicted quantiles
  quantiles_pred <- lapply(names(pdfs), \(one_cond) {
    tryCatch(
      expr = {
       calc_quantiles_pred(
          pdf_u = pdfs[[one_cond]]$pdf_u,
          pdf_l = pdfs[[one_cond]]$pdf_l,
          t_vec = t_vec,
          one_cond = one_cond,
          probs = stats_agg_info[[one_cond]]$probs_corr,
          dt = dt
        )[["Quant_U"]]
      },
      error = function(e){
        NA
      }
    )
  })

  quantiles_pred <- unlist(quantiles_pred)
  cafs_pred <- unlist(cafs_pred)

  if (any(is.na(quantiles_pred)) | any(is.na(cafs_pred))) {
    return(Inf)
  }

  # finally, calculate the rmse
  stopifnot(length(quantiles_pred) == length(quantiles_obs))
  stopifnot(length(cafs_pred) == length(cafs_obs))

  n_quantiles = length(quantiles_pred)
  n_cafs = length(cafs_pred)

  rmse_quantiles <- sqrt(mean((quantiles_pred - quantiles_obs)^2))
  rmse_cafs <- sqrt(mean((cafs_pred - cafs_obs)^2))

  w_quantiles <- n_quantiles / (n_cafs + n_quantiles)
  w_cafs <- (1 - w_quantiles) * weight_err

  rmse <- w_cafs * rmse_cafs + w_quantiles * rmse_quantiles

  return(rmse)
}


