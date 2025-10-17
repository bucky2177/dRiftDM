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
    log_like <- log_like +
      log_like_heart(
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
        x = t_vec,
        y = pdf_u,
        xout = rts_u
      )$y
      app_like_l <- stats::approx(
        x = t_vec,
        y = pdf_l,
        xout = rts_l
      )$y

      log_like <- suppressWarnings(sum(log(app_like_u)) + sum(log(app_like_l)))
      if (is.nan(log_like)) {
        # log(0) gives -Inf
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


# FUNCTIONS FOR CALCULATING RMSE -----------------------------------------

#' RMSE Calculation and Helpers
#'
#' Internal helpers for computing the root mean squared error (RMSE) between
#' predicted and observed quantiles and conditional accuracy
#' functions.
#'
#' * `calc_rmse_eval()` prepares observed and predicted quantiles/CAFs from PDFs
#'   and aggregated info, then calls `calc_rmse()`.
#' * `calc_rmse()` computes the weighted RMSE given predicted and observed
#'   quantiles/CAFs.
#'
#' @param pdfs list of PDFs per condition (named).
#' @param t_vec numeric time vector.
#' @param dt numeric time step.
#' @param stats_agg list of observed summary statistics.
#' @param stats_agg_info list with info on quantile probabilities and CAF bins.
#' @param quants_pred numeric vector of predicted quantiles (already flattened).
#' @param cafs_pred numeric vector of predicted CAFs (already flattened).
#' @param quants_obs numeric vector of observed quantiles (already flattened).
#' @param cafs_obs numeric vector of observed CAFs (already flattened).
#' @param weight_err non-negative numeric scalar; weight factor for CAF error
#'   relative to quantile error. Default is 1.5
#'
#' @return A single numeric RMSE value, or `NULL` if no observed stats were
#'   provided, or `Inf` if predictions failed (contain `NA`).
#'
#' @seealso [dRiftDM::stats_from_pdfs_agg_info()]
#' @keywords internal
calc_rmse_eval <- function(
  pdfs,
  t_vec,
  dt,
  stats_agg,
  stats_agg_info,
  weight_err = 1.5
) {
  # if no observed stats are present, return NULL
  if (is.null(stats_agg)) {
    return(NULL)
  }

  # ensure consistent condition order for obs stats: follow 'pdfs' order
  conds <- names(pdfs)
  if (is.null(conds) || length(conds) == 0L) {
    stop("'pdfs' must be a named list with at least one condition")
  }

  # observed stats in the same order as 'pdfs'
  quants_obs <- unlist(lapply(conds, \(x) stats_agg[[x]]$quantiles_corr))
  cafs_obs <- unlist(lapply(conds, \(x) stats_agg[[x]]$cafs))

  # build predictions
  cafs_pred <- unlist(stats_from_pdfs_agg_info(
    pdfs = pdfs,
    t_vec = t_vec,
    dt = dt,
    stats_agg_info = stats_agg_info,
    "cafs"
  ))
  quants_pred <- unlist(stats_from_pdfs_agg_info(
    pdfs = pdfs,
    t_vec = t_vec,
    dt = dt,
    stats_agg_info = stats_agg_info,
    "quantiles"
  ))

  # final RMSE
  rmse <- calc_rmse(
    quants_pred = quants_pred,
    cafs_pred = cafs_pred,
    quants_obs = quants_obs,
    cafs_obs = cafs_obs,
    weight_err = weight_err
  )
  return(rmse)
}


#' Compute RMSE from predicted and observed quantiles/CAFs
#' @keywords internal
#' @rdname calc_rmse_eval
calc_rmse <- function(
  quants_pred,
  cafs_pred,
  quants_obs,
  cafs_obs,
  weight_err = 1.5
) {
  # basic checks
  if (!is.numeric(weight_err) || length(weight_err) != 1 || weight_err < 0) {
    stop("'weight_err' must be a single non-negative numeric")
  }

  if (
    length(quants_pred) == 0L ||
      length(cafs_pred) == 0L ||
      length(quants_obs) == 0L ||
      length(cafs_obs) == 0L
  ) {
    stop("Empty inputs: all vectors must have length > 0")
  }

  # any NA -> treat as failure (Inf)
  any_na <- anyNA(
    list(quants_pred, cafs_pred, quants_obs, cafs_obs),
    recursive = TRUE
  )
  if (any_na) {
    return(Inf)
  }

  # length checks
  if (length(quants_pred) != length(quants_obs)) {
    stop("Length mismatch between 'quantiles_pred' and 'quantiles_obs'")
  }
  if (length(cafs_pred) != length(cafs_obs)) {
    stop("Length mismatch between 'cafs_pred' and 'cafs_obs'")
  }

  n_q <- length(quants_pred)
  n_c <- length(cafs_pred)

  rmse_q <- sqrt(mean((quants_pred - quants_obs)^2))
  rmse_c <- sqrt(mean((cafs_pred - cafs_obs)^2))

  w_q <- n_q / (n_q + n_c)
  w_c <- (1 - w_q) * weight_err

  rmse <- w_c * rmse_c + w_q * rmse_q
  return(rmse)
}


# HELPER FUNCTIONS --------------------------------------------------------

#' Get Quantiles/CAFs from PDFs and stats_agg_info
#'
#' Internal helper to compute predicted summary statistics from condition-wise
#' PDFs, following the condition order. Calculates summary
#' statistics according to the information in `stats_agg_info`. Currently
#' supports either quantiles or conditional accuracy functions (CAFs).
#'
#' @param pdfs named list of PDFs per condition, each containing elements
#'   `pdf_u` and `pdf_l`.
#' @param t_vec numeric time vector.
#' @param dt numeric time step.
#' @param stats_agg_info list with information needed to compute summaries
#'   (e.g., quantile probabilities or CAF bin counts; optional).
#' @param what character, one of `"quantiles"` or `"cafs"`, selecting which
#'   statistic to compute.
#'
#' @return A list of numeric vectors, one per condition, containing the
#'   predicted quantiles or CAFs. In case of failure, `NA_real_` values are
#'   returned.
#' @keywords internal
stats_from_pdfs_agg_info <- function(
  pdfs,
  t_vec,
  dt,
  stats_agg_info = NULL,
  what
) {
  stopifnot(is.list(pdfs))
  stopifnot(is.numeric(t_vec))
  stopifnot(is.numeric(dt))
  if (is.null(stats_agg_info)) {
    stats_agg_info = list()
  }
  stopifnot(is.list(stats_agg_info))
  what <- match.arg(what, c("quantiles", "cafs"))

  conds <- names(pdfs)
  if (is.null(conds) || length(conds) == 0L) {
    stop("'pdfs' must be a named list with at least one condition")
  }

  # CAFs
  if ("cafs" == what) {
    cafs_pred_list <- lapply(conds, function(one_cond) {
      n_bins = stats_agg_info[[one_cond]]$n_bins %||% drift_dm_default_n_bins()
      tryCatch(
        calc_cafs_pred(
          pdf_u = pdfs[[one_cond]]$pdf_u,
          pdf_l = pdfs[[one_cond]]$pdf_l,
          t_vec = t_vec,
          one_cond = one_cond,
          n_bins = n_bins
        )[["P_U"]],
        error = function(e) NA_real_
      )
    })
    return(cafs_pred_list)
  }

  # Quantiles
  if ("quantiles" == what) {
    quants_pred_list <- lapply(conds, function(one_cond) {
      probs = stats_agg_info[[one_cond]]$probs_corr %||%
        drift_dm_default_probs()
      tryCatch(
        calc_quantiles_pred(
          pdf_u = pdfs[[one_cond]]$pdf_u,
          pdf_l = pdfs[[one_cond]]$pdf_l,
          t_vec = t_vec,
          one_cond = one_cond,
          probs = probs,
          dt = dt
        )[["Quant_U"]],
        error = function(e) NA_real_
      )
    })
    return(quants_pred_list)
  }
}
