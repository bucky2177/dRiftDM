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
  if (is.null(n_bins)) {
    n_bins <- 5
  }

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
    result_obs <- calc_cafs_obs(
      rts_corr = rts_corr, rts_err = rts_err,
      one_cond = one_cond, n_bins = n_bins
    )
    if (!is.null(result_obs)) result_obs <- cbind(Source = "obs", result_obs)
  }

  if (source == "pred" | source == "both") {
    result_pred <- calc_cafs_pred(
      pdf_u = pdf_u, pdf_l = pdf_l,
      one_cond = one_cond, n_bins = n_bins
    )
    result_pred <- cbind(Source = "pred", result_pred)
  }

  if (source == "obs") {
    return(result_obs)
  }
  if (source == "pred") {
    return(result_pred)
  }
  if (source == "both") {
    return(rbind(result_obs, result_pred))
  }
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
calc_quantiles_pred <- function(pdf_u, pdf_l, t_vec, one_cond, probs,
                                skip_if_contr_low = 0.0001) {
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

  sum_pdf_l = sum(pdf_l)
  sum_pdf_u = sum(pdf_u)

  if (sum_pdf_u / (sum_pdf_u + sum_pdf_l) >= 1 - skip_if_contr_low) {
    quants["Quant_Err"] = NA
  }
  if (sum_pdf_u / (sum_pdf_u + sum_pdf_l) <= skip_if_contr_low) {
    quants["Quant_Corr"] = NA
  }

  return(quants)
}


# internal function for input checking, data wrangling, and default values
calc_quantiles <- function(pdf_u, pdf_l, t_vec, rts_corr, rts_err, one_cond,
                           probs = NULL, source = "both") {
  if (is.null(probs)) {
    probs <- drift_dm_default_probs()
  }


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
    result_obs <- calc_quantiles_obs(
      rts_corr = rts_corr, rts_err = rts_err,
      one_cond = one_cond, probs = probs
    )
    if (!is.null(result_obs)) result_obs <- cbind(Source = "obs", result_obs)
  }

  if (source == "pred" | source == "both") {
    result_pred <- calc_quantiles_pred(
      pdf_u = pdf_u, pdf_l = pdf_l,
      t_vec = t_vec, one_cond = one_cond,
      probs = probs
    )
    result_pred <- cbind(Source = "pred", result_pred)
  }

  if (source == "obs") {
    return(result_obs)
  }
  if (source == "pred") {
    return(result_pred)
  }
  if (source == "both") {
    return(rbind(result_obs, result_pred))
  }
}

# given a dataset providing the quantiles, calculates delta function(s)
# for the character vectors minuends and subtrahends
calc_delta_fun <- function(quantiles_dat, minuends = NULL, subtrahends = NULL,
                           dvs = NULL) {

  # input checks on data frame
  if (!is.data.frame(quantiles_dat)) {
    stop("the provided quantiles_dat is not a data.frame")
  }
  nec_columns = c("Source", "Cond", "Prob", "Quant_Corr", "Quant_Err")
  if (any(colnames(quantiles_dat) != nec_columns)) {
    stop("the provided quantiles_dat provides unexpected column names",
         "\n\tprovided: ", paste(colnames(quantiles_dat), collapse = " "),
         "\n\tnecessary: ", paste(nec_columns, collapse = " "))
  }

  # input checks on minuends/subtrahends
  if (is.null(minuends)) {
    stop("calc_delta_fun was called but the argument minuends not provided")
  }
  if (is.null(subtrahends)) {
    stop("calc_delta_fun was called but the argument minuends not provided")
  }
  if (!is.character(minuends) | length(minuends) < 1) {
    stop("minuends must a character vector of length >= 1")
  }
  if (!is.character(subtrahends) | length(subtrahends) < 1) {
    stop("subtrahends must a character vector of length >= 1")
  }
  if (length(subtrahends) != length(minuends)) {
    stop("different length of minuends and subtrahends")
  }
  if (!all(minuends %in% unique(quantiles_dat$Cond))) {
    stop("Conds specified in minuends are not provided within quantiles_dat")
  }
  if (!all(subtrahends %in% unique(quantiles_dat$Cond))) {
    stop("Conds specified in subtrahends are not provided within quantiles_dat")
  }

  # input checks on dvs
  if (is.null(dvs)) {
    dvs <- "Quant_Corr"
  }
  dvs <- sapply(dvs, function(x) {
    match.arg(x, c("Quant_Err", "Quant_Corr"))
  })
  dvs <- unname(dvs)

  if (length(dvs) > 1 & length(dvs) != length(minuends)) {
    if (length(minuends) == 1) {
      minuends = rep(minuends, length(dvs))
      subtrahends = rep(subtrahends, length(dvs))
    } else {
      stop("if several dvs are provided, the length must match minuends/subtrahends")
    }
  }

  # reduce and make wide format
  quantiles_dat = quantiles_dat[c("Source", "Cond", "Prob", dvs)]

  n_probs = length(unique(quantiles_dat$Prob))
  n_source = length(unique(quantiles_dat$Source))
  n_cond = length(unique(quantiles_dat$Cond))
  if (nrow(quantiles_dat) != n_probs * n_source * n_cond) {
    stop("quantiles_dat doesn't code uniquely rows solely by Probs, Source, ",
         "and Cond")
  }
  quantiles_dat = stats::reshape(quantiles_dat, idvar = c("Source", "Prob"),
                                 timevar = "Cond", direction = "wide", sep = "_")

  # calculate delta functions

  if (length(dvs) == 1) {
    delta_names = paste("Delta", paste(minuends, subtrahends, sep = "_"), sep = "_")
    avg_names = paste("Avg", paste(minuends, subtrahends, sep = "_"), sep = "_")
  } else {
    delta_names = paste("Delta", gsub("^Quant_", "", dvs), sep = "_")
    avg_names = paste("Avg", gsub("^Quant_", "", dvs), sep = "_")
    delta_names = paste(delta_names, paste(minuends, subtrahends, sep = "_"), sep = "_")
    avg_names = paste(avg_names, paste(minuends, subtrahends, sep = "_"), sep = "_")
  }
  minuendss_wide = paste(dvs, minuends, sep = "_")
  subtrahends_wide = paste(dvs, subtrahends, sep = "_")


  for (i in seq_along(minuends)) {
    vals_minuends = quantiles_dat[[minuendss_wide[i]]]
    vals_subtrahends = quantiles_dat[[subtrahends_wide[i]]]

    quantiles_dat[[delta_names[i]]] = vals_minuends - vals_subtrahends
    quantiles_dat[[avg_names[i]]] = 0.5*vals_minuends + 0.5*vals_subtrahends
  }
  return(quantiles_dat)
}


#' Calcuating Statistics
#'
#' This function is a wrapper for calculating statistics on model predictions
#' or observed data. Currently supported are:
#' - Conditional Accuracy Functions (CAFs)
#' - Quantiles
#' - Delta Functions
#'
#' @param drift_dm_obj an object inheriting from [dRiftDM::drift_dm]
#'
#' @param type character vector, indicating which statistics should be
#'  calculated. Currently supported options are "cafs", "quantiles", "delta_funs".
#' @param source character, indicating whether statistics for observed data ("obs"),
#'  the model's predictions ("pred"), or both ("both") should be calculated.
#'  Default is "both".
#' @param ... additional optional and necessary parameters passed down to the
#' functions handling the calculations. See Details for more information
#'
#' @details
#'
#' # Conditional Accuracy Function (CAFs)
#'
#' CAFs are a way to quantify response accuracy against speed. To calculate
#' CAFs, RTs (whether correct or incorrect) are first binned and then the
#' percent correct responses per bin is calculated.
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
#'  probabilites, `probs`, with values in \eqn{[0, 1]}. Default is
#'  `seq(0.1, 0.9, 0.1)`.
#'
#' # Delta Functions
#'
#'  Delta functions calculate the difference between quantiles
#'  of two conditions against their mean:
#'  - \eqn{Delta_i = Q_{i,j} - Q_{i,k}}
#'  - \eqn{Avg_i = 0.5 \cdot Q_{i,j} + 0.5 \cdot Q_{i,k}}
#'
#'  With i indicating a quantile, and j and k two conditions.
#'
#'  To calculate delta functions, users have to specify:
#'  - `minuends`: character vector, specifying condition(s) j. Must be in
#'    `drift_dm_obj$conds`.
#'  - `subtrahends`: character vector, specifying condition(s) k. Must be in
#'    `drift_dm_obj$conds`
#'  - `dvs`: character vector, specifying the dependent variable(s) to use.
#'     Supported are `Quant_Corr` and `Quant_Err`
#'  - optional: `probs`, see the section on quantiles
#'
#'
#'
#' @returns
#'
#' The return value depends on the `type` argument. If `type` is  a character
#' vector of length 1, a single data.frame is returned containing the requested
#' statistics. If `type` is a character vector of length > 1, a named list
#' is passed back, with each entry in the list corresponding to a data.frame of
#' statistics.
#'
#' @export
calc_stats <- function(drift_dm_obj, type, source = "both", ...) {
  dotdot <- list(...)

  # input checks
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }

  if (!is.character(type) | length(type) == 0) {
    stop("type must be character vector of length >= 1")
  }


  type <- sapply(type, function(x) {
      match.arg(x, c("cafs", "quantiles", "delta_funs"))
    })
  type <- unname(type)


  # get all rts and pdfs for quick reference
  if (is.null(drift_dm_obj$pdfs)) {
    drift_dm_obj <- re_evaluate_model(
      drift_dm_obj = drift_dm_obj,
      eval_model = T
    )
  }
  all_pdfs <- drift_dm_obj$pdfs

  all_rts_corr <- drift_dm_obj$obs_data$rts_corr
  all_rts_err <- drift_dm_obj$obs_data$rts_err

  stopifnot(all(names(all_rts_corr) == names(all_rts_err)))
  stopifnot(all(names(all_pdfs) == names(all_rts_err)))


  # get time space (needed for quantiles)
  t_max <- drift_dm_obj$prms_solve[["t_max"]]
  nt <- drift_dm_obj$prms_solve[["nt"]]
  t_vec <- seq(0, t_max, length.out = nt + 1)



  # iterate through the requested types and calculate the stats
  all_stats <-
    lapply(type, function(one_type) {
      if (one_type == "quantiles") {
        result <-
          lapply(drift_dm_obj$conds, function(one_cond) {
            pdf_u <- all_pdfs[[one_cond]]$pdf_u
            pdf_l <- all_pdfs[[one_cond]]$pdf_l
            rts_corr <- all_rts_corr[[one_cond]]
            rts_err <- all_rts_err[[one_cond]]

            calc_quantiles(
              pdf_u = pdf_u, pdf_l = pdf_l, t_vec = t_vec,
              rts_corr = rts_corr, rts_err = rts_err,
              one_cond = one_cond, probs = dotdot$probs,
              source = source
            )
          })
        result <- do.call("rbind", result)
      }
      if (one_type == "cafs") {
        result <-
          lapply(drift_dm_obj$conds, function(one_cond) {
            pdf_u <- all_pdfs[[one_cond]]$pdf_u
            pdf_l <- all_pdfs[[one_cond]]$pdf_l
            rts_corr <- all_rts_corr[[one_cond]]
            rts_err <- all_rts_err[[one_cond]]

            calc_cafs(
              pdf_u = pdf_u, pdf_l = pdf_l, rts_corr = rts_corr,
              rts_err = rts_err, one_cond = one_cond,
              n_bins = dotdot$n_bins, source = source
            )
          })
        result <- do.call("rbind", result)
      }
      if (one_type == "delta_funs") {
        interim <- calc_stats(drift_dm_obj, type = "quantiles", source = source,
                              probs = dotdot$probs)
        result <- calc_delta_fun(quantiles_dat = interim,
                                 minuends = dotdot$minuends,
                                 subtrahends = dotdot$subtrahends,
                                 dvs = dotdot$dvs)
      }

      if (!is.null(result)) {
        result <- result[order(result$Source), ]
        rownames(result) <- 1:nrow(result)
      }
      return(result)
    })

  if (length(all_stats) == 1) {
    all_stats <- all_stats[[1]]
  } else {
    names(all_stats) <- type
  }

  return(all_stats)
}
