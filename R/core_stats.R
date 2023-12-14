# ==== FUNCTIONS FOR CALCULATING THE CAF

# calc cafs for one cond
calc_cafs_obs <- function(rts_corr, rts_err, one_cond, n_bins) {
  if (is.null(rts_corr) & is.null(rts_err)) {
    warning(
      "CAF of observed values requested for condition ", one_cond,
      " but no data can be found. Please double-check your model.",
      " Not calculating cafs for observed data"
    )
    return(NULL)
  }
  rts <- c(rts_corr, rts_err)
  probs <- seq(0, 1, length.out = n_bins + 1)
  borders <- stats::quantile(rts, probs = probs)
  bins <- cut(rts, breaks = borders, labels = FALSE, include.lowest = TRUE)
  stopifnot(sort(unique(bins)) == 1:n_bins)
  corr <- rep(c(1, 0), times = c(length(rts_corr), length(rts_err)))
  caf <- tapply(corr, bins, mean)
  caf <- data.frame(
    Cond = one_cond,
    Bin = names(caf),
    P_Corr = as.numeric(caf)
  )
  return(caf)
}

# cafs for one pdf_u and pdf_l pair
calc_cafs_pred <- function(pdf_u, pdf_l, one_cond, n_bins) {
  stopifnot(length(pdf_u) == length(pdf_l))

  # make a cdf and scale it to a value between 0 and 1
  cdf <- pdf_u + pdf_l
  cdf <- cumsum(cdf)
  cdf <- cdf - min(cdf)
  cdf <- cdf / max(cdf)

  x <- 1:length(cdf)
  probs <- seq(0, 1, length.out = n_bins + 1)
  probs <- probs[2:(length(probs) - 1)]
  x_borders <- stats::approx(x = cdf, y = x, xout = probs, ties = "mean")$y
  x_borders <- append(x_borders, min(x), after = 0) # ensure that x_borders
  x_borders <- append(x_borders, max(x)) # contains the lower and upper part
  bins <- cut(x, breaks = x_borders, labels = FALSE, include.lowest = TRUE)
  stopifnot(unique(bins) == 1:n_bins)
  sum_u <- tapply(pdf_u, bins, sum)
  sum_l <- tapply(pdf_l, bins, sum)
  caf <- sum_u / (sum_u + sum_l)
  caf <- data.frame(
    Cond = one_cond,
    Bin = names(caf),
    P_Corr = as.numeric(caf)
  )
  return(caf)
}

# internal function for input checking, data wrangling, and default values
calc_cafs <- function(pdf_u, pdf_l, rts_corr, rts_err, one_cond, n_bins = NULL,
                      source = "both") {

  if (is.null(n_bins))
    n_bins = 5

  # input checks
  if (!is.numeric(n_bins) | length(n_bins) != 1) {
    stop("n_bins must a single numeric")
  }
  if (n_bins <= 1) {
    stop("argument n_bins nust be larger than 1")
  }

  source <- match.arg(source, c("obs", "pred", "both"))

  # calculations
  if (source == "obs" | source == "both") {
    result_obs = calc_cafs_obs(rts_corr = rts_corr, rts_err = rts_err,
                           one_cond = one_cond, n_bins = n_bins)
    if (!is.null(result_obs)) result_obs <- cbind(Source = "obs", result_obs)
  }

  if (source == "pred" | source == "both") {
    result_pred = calc_cafs_pred(pdf_u = pdf_u, pdf_l = pdf_l,
                                 one_cond = one_cond, n_bins = n_bins)
    result_pred <- cbind(Source = "pred", result_pred)

  }

  if (source == "obs") return(result_obs)
  if (source == "pred") return(result_pred)
  if (source == "both") return(rbind(result_obs, result_pred))
}






# ==== FUNCTIONS FOR CALCULATING QUANTILES
# calculate obs quantiles for one set of observed rts
calc_quantiles_obs <- function(rts_corr, rts_err, one_cond, probs) {
  if (is.null(rts_corr) | is.null(rts_err)) {
    warning(
      "Quantiles of observed values requested for condition ", one_cond,
      " but no data can be found. Please double-check your model.",
      " Not calculating quantiles for observed data"
    )
    return(NULL)
  }

  quants_rts_corr <- stats::quantile(rts_corr, probs = probs)
  quants_rts_err <- stats::quantile(rts_err, probs = probs)
  quants <- data.frame(
    Cond = one_cond,
    Prob = probs,
    Quant_Corr = unname(quants_rts_corr),
    Quant_Err = unname(quants_rts_err)
  )
  return(quants)
}

# calculate obs quantiles for one set of observed pdfs
calc_quantiles_pred <- function(pdf_u, pdf_l, t_vec, one_cond, probs) {
  stopifnot(length(pdf_u) == length(pdf_l))
  stopifnot(length(pdf_u) == length(t_vec))

  quants <-
    apply(cbind(pdf_u, pdf_l), MARGIN = 2, function(a_pdf, t_vec, probs) {
      cdf <- cumsum(a_pdf)
      cdf <- cdf - min(cdf)
      cdf <- cdf / max(cdf)
      return(stats::approx(x = cdf, y = t_vec, xout = probs, ties = "mean")$y)
    }, t_vec = t_vec, probs = probs)

    colnames(quants) <- c("Quant_Corr", "Quant_Err")
    quants <- as.data.frame(quants)
    quants <- cbind(Cond = one_cond, Prob = probs, quants)
    return(quants)
}


# internal function for input checking, data wrangling, and default values
calc_quantiles <- function(pdf_u, pdf_l, t_vec, rts_corr, rts_err, one_cond,
                           probs = NULL, source = "both") {

  if (is.null(probs))
    probs = seq(0.1, 0.9, 0.1)


  # input checks
  if (!is.numeric(probs) | length(probs) < 2) {
    stop("probs must a numeric vector of length > 1")
  }

  if (min(probs) <= 0 | max(probs) >= 1) {
    stop("argument probs must be in the range ]0, 1[")
  }

  source <- match.arg(source, c("obs", "pred", "both"))

  # calculations
  if (source == "obs" | source == "both") {
    result_obs = calc_quantiles_obs(rts_corr = rts_corr, rts_err = rts_err,
                                    one_cond = one_cond, probs = probs)
    if (!is.null(result_obs)) result_obs <- cbind(Source = "obs", result_obs)
  }

  if (source == "pred" | source == "both") {
    result_pred = calc_quantiles_pred(pdf_u = pdf_u, pdf_l = pdf_l,
                                      t_vec = t_vec,  one_cond = one_cond,
                                      probs = probs)
    result_pred <- cbind(Source = "pred", result_pred)
  }

  if (source == "obs") return(result_obs)
  if (source == "pred") return(result_pred)
  if (source == "both")  return(rbind(result_obs, result_pred))
}


# wrapper function for multiple prediction stats


#' Calcuating Statistics
#'
#' This function is a wrapper for calculating statistics on model predictions
#' or observed data. Currently supported are:
#' - Conditional Accuracy Functions (CAFs)
#' - Quantiles
#'
#' @param drift_dm_obj an object inheriting from [dRiftDM::drift_dm]
#'
#' @param type character vector, indicating which statistics should be
#'  calculated. Currently supported options are "cafs" or "quantiles".
#' @param source character, indicating whether CAFs of the observed data ("obs"),
#'  of the model's predictions ("pred"), or both ("both") should be calculated.
#'  Default is "both".
#' @param ... additional parameters passed down to the functions handling the
#' calculations. See Details for more information
#' @param n_bins numeric, providing the number of bins that should be used when
#'  calculating the CAFs
#'
#' @details
#'
#' # Conditional Accuracy Function (CAFs)
#'
#' CAFs are a way to quantify response accuracy against speed. To calculate
#' CAFs, RTs (whether correct or incorrect) are first binned and then the
#' percentage of correct responses per bin is calculated.
#'
#' When calculating model-based CAFs, a joint cdf combining both the pdf
#' of correct and incorrect responses is calculated. Afterwards, this cdf
#' is separated into even-spaced segments and the contribution of
#' the pdf associated with a correct response relative to the joint cdf is
#' calculated.
#'
#' The number of bins can be controlled by passing the argument `n_bins`.
#' The default is 5.
#'
#' # Quantiles
#'
#'  For observed response times, the function [stats::quantile] is used with
#'  default settings.
#'
#'  Which quantiles are calcuated can be controlled by providing the
#'  probabilites with values in \eqn{\[0, 1s\]}. Default is
#'  `seq(0.1, 0.9, 0.1)`.
#'
#' @returns
#'
#' The return value depends on the `type` argument. If `type` is  a charcter
#' vector of length 1, a single data.frame is returned containing the requested
#' statistics. If `type` is a character vector of length > 1, a named list
#' is passed back, with each entry in the list corresponding to a data.frame of
#' statistics.
#'
#' @export
calc_stats = function(drift_dm_obj, type, source = "both", ...) {

  dotdot = list(...)

  # input checks
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }

  if (!is.character(type) | length(type) == 0) {
    stop("type must be character vector of length >= 1")
  }

  for (one_type in type) {
    match.arg(one_type, c("cafs", "quantiles"))
  }

  # get all rts and pdfs for quick reference
  if (is.null(drift_dm_obj$pdfs)){
    drift_dm_obj = re_evaluate_model(drift_dm_obj = drift_dm_obj,
                                     eval_model = T)
  }
  all_pdfs = drift_dm_obj$pdfs

  all_rts_corr = drift_dm_obj$obs_data$rts_corr
  all_rts_err = drift_dm_obj$obs_data$rts_err

  stopifnot(all(names(all_rts_corr) == names(all_rts_err)))
  stopifnot(all(names(all_pdfs) == names(all_rts_err)))


  # get time space (needed for quantiles)
  t_max <- drift_dm_obj$prms_solve[["t_max"]]
  nt <- drift_dm_obj$prms_solve[["nt"]]
  t_vec <- seq(0, t_max, length.out = nt + 1)



  # iterate through the requested types and calculate the stats
  all_stats =
    lapply(type, function(one_type) {

      if (one_type == "quantiles") {
        result =
          lapply(drift_dm_obj$conds, function(one_cond){

            pdf_u = all_pdfs[[one_cond]]$pdf_u
            pdf_l = all_pdfs[[one_cond]]$pdf_l
            rts_corr = all_rts_corr[[one_cond]]
            rts_err = all_rts_err[[one_cond]]

            calc_quantiles(pdf_u = pdf_u, pdf_l = pdf_l, t_vec = t_vec,
                           rts_corr = rts_corr, rts_err = rts_err,
                           one_cond = one_cond, probs = dotdot$probs,
                           source = source)
          })
        result = do.call("rbind", result)
      }

      if (one_type == "cafs") {
        result =
          lapply(drift_dm_obj$conds, function(one_cond){

            pdf_u = all_pdfs[[one_cond]]$pdf_u
            pdf_l = all_pdfs[[one_cond]]$pdf_l
            rts_corr = all_rts_corr[[one_cond]]
            rts_err = all_rts_err[[one_cond]]

            calc_cafs(pdf_u = pdf_u, pdf_l = pdf_l, rts_corr = rts_corr,
                      rts_err = rts_err, one_cond = one_cond,
                      n_bins = dotdot$n_bins, source = source)
          })
        result = do.call("rbind", result)
      }
      if (!is.null(result)) {
        result = result[order(result$Source),]
        rownames(result) = 1:nrow(result)
      }
      return(result)
  })

  if (length(all_stats) == 1) {
    all_stats = all_stats[[1]]
  } else {
    names(all_stats) = type
  }

  return(all_stats)
}

