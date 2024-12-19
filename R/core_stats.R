# FUNCTIONS FOR CALCULATING THE CAF ---------------------------------------

#' Calculate CAFs
#'
#' Backend functions to calculate conditional accuracy functions for RT
#' vectors or pdfs
#'
#' @param rts_u,rts_l vectors of RTs for the upper and lower boundary
#' @param pdf_u,pdf_l density values for the upper and lower boundary
#' @param one_cond character label
#' @param n_bins number of bins to use for the CAFs
#'
#' @returns a data.frame with the "Cond" label, the "Bin"s and "P_U" for
#' the CAFs
#'
#' @details
#' for RTs:
#' first elements are attributed to a bin (with bins calculated across all RTs
#' using equally spaced quantiles),
#' then accuracy per bin is calculated.
#'
#' for Densities: Add density values, calculate a CDF and force it between 0 and
#' 1. Then determine the indices that cut the CDF into bins by considering
#' equally spaced quantiles. Then calculate the ratio of probability mass per
#' bin.
#'
#' @keywords internal
calc_cafs_obs <- function(rts_u, rts_l, one_cond, n_bins) {
  # cut (all) RTs into n bins
  rts <- c(rts_u, rts_l)
  probs <- seq(0, 1, length.out = n_bins + 1)
  borders <- stats::quantile(rts, probs = probs)
  bins <- cut(rts, breaks = borders, labels = FALSE, include.lowest = TRUE)
  stopifnot(sort(unique(bins)) == 1:n_bins)

  # create a vector to code accuracy
  corr <- rep(c(1, 0), times = c(length(rts_u), length(rts_l)))

  # separate accuracy vector by the calcualted bins, and get mean accuracy per
  # bin
  caf <- tapply(corr, bins, mean)
  caf <- data.frame(
    Cond = one_cond,
    Bin = as.numeric(names(caf)),
    P_U = as.numeric(caf)
  )
  return(caf)
}

#' @rdname calc_cafs_obs
calc_cafs_pred <- function(pdf_u, pdf_l, one_cond, n_bins) {
  stopifnot(length(pdf_u) == length(pdf_l))

  # make a cdf and scale it to a value between 0 and 1
  cdf <- pdf_u + pdf_l
  cdf <- cumsum(cdf)
  cdf <- cdf - min(cdf)
  cdf <- cdf / max(cdf)

  # get the quantiles that determine the bins
  probs <- seq(0, 1, length.out = n_bins + 1)
  probs <- probs[2:(length(probs) - 1)]

  # get the indices that determine the "cuts"
  x <- 1:length(cdf)
  x_borders <- stats::approx(x = cdf, y = x, xout = probs, ties = "mean")$y
  x_borders <- append(x_borders, min(x), after = 0) # ensure that x_borders
  x_borders <- append(x_borders, max(x)) # contains the lower and upper part

  # "label" the indices by the bins
  bins <- cut(x, breaks = x_borders, labels = FALSE, include.lowest = TRUE)
  stopifnot(unique(bins) == 1:n_bins)

  # determine the probability mass per bin and pdf
  sum_u <- tapply(pdf_u, bins, sum)
  sum_l <- tapply(pdf_l, bins, sum)

  # calculate the ratio and pass back
  caf <- sum_u / (sum_u + sum_l)
  caf <- data.frame(
    Cond = one_cond,
    Bin = as.numeric(names(caf)),
    P_U = as.numeric(caf)
  )
  return(caf)
}

# internal function for input checking, data wrangling, and default values
#' Calculate CAFs
#'
#' Function that calls the underlying CAF calculation functions
#' [dRiftDM::calc_cafs_obs] and [dRiftDM::calc_cafs_pred]. Does input checks
#' and the data wrangling
#'
#' @param pdf_u,pdf_l either NULL or density vectors
#' @param rts_u,rts_l either NULL or RT vectors
#' @param one_cond a label for the data.frame
#' @param n_bins the number of bins, default is 5
#' @param b_coding used for accessing the upper boundary label, determines
#' the corresponding column of the returned data.frame (e.g., P_`corr`).
#'
#' @returns a data.frame with "Source", "Cond", "Bin"s, "P_<u_label>" for
#' the CAFs of type c("cafs", "sum_dist", "stats_dm", "data.frame")
#'
#' @details
#' if pdf_u and pdf_l are not NULL, returns CAFs of the densities
#'
#' if rts_u and rts_l are not NULL, returns CAFs of the response times
#'
#' if all are not NULL, returns both.
#'
#' @seealso [dRiftDM::new_stats_dm()]
#'
#' @keywords internal
calc_cafs <- function(pdf_u = NULL, pdf_l = NULL, rts_u = NULL, rts_l = NULL,
                      one_cond, n_bins = NULL, b_coding) {
  # default settings and parameter extraction
  if (is.null(n_bins)) {
    n_bins <- 5
  }

  if (!is_numeric(n_bins) | length(n_bins) != 1) {
    stop("n_bins must a valid numeric")
  }
  if (n_bins <= 1) {
    stop("argument n_bins nust be larger than 1")
  }

  u_name <- names(b_coding$u_name_value)


  # check pdf_l and pdf_u
  if (xor(is.null(pdf_l), is.null(pdf_u))) {
    stop("pdf_l and pdf_u either have to be both NULL or not")
  }

  # check rts_u and rts_l
  if (xor(is.null(rts_u), is.null(rts_l))) {
    stop("rts_u and rts_l either have to be both NULL or not")
  }

  # calculate the respective statistics
  # if pdfs are supplied ...
  result_pred <- NULL
  if (!is.null(pdf_u)) {
    result_pred <- calc_cafs_pred(
      pdf_u = pdf_u, pdf_l = pdf_l,
      one_cond = one_cond, n_bins = n_bins
    )
    result_pred <- cbind(Source = "pred", result_pred)
    colnames(result_pred)[which(names(result_pred) == "P_U")] <-
      paste("P", u_name, sep = "_")
  }

  result_obs <- NULL
  if (!is.null(rts_u)) {
    result_obs <- calc_cafs_obs(
      rts_u = rts_u, rts_l = rts_l,
      one_cond = one_cond, n_bins = n_bins
    )
    result_obs <- cbind(Source = "obs", result_obs)
    colnames(result_obs)[which(names(result_obs) == "P_U")] <-
      paste("P", u_name, sep = "_")
  }



  # combine
  result <- rbind(result_obs, result_pred)

  # make it stats_dm class and pass back
  stats_dm_obj <- new_stats_dm(
    stat_df = result, type = "cafs",
    b_coding = b_coding
  )
  return(stats_dm_obj)
}






# FUNCTIONS FOR CALCULATING QUANTILES -------------------------------------

#' Calculate Quantiles
#'
#' Backend functions to calculate quantiles for RT vectors or pdfs
#'
#' @param rts_u,rts_l vectors of RTs for the upper and lower boundary
#' @param pdf_u,pdf_l density values for the upper and lower boundary
#' @param one_cond character label
#' @param probs numeric vector with values between 0 and 1 for the probability
#' levels
#' @param t_vec the time space (required for the pdfs)
#'
#' @param skip_if_contr_low numeric. If the relative contribution of the upper
#' or lower PDF to the overall PDF is too low (default 0.01%), return NAs for
#' this PDF.
#'
#' @returns a data.frame with the "Cond" label, the "Prob"s and "Quant_U" and
#' "Quant_L" for the quantiles
#'
#' @details
#' for RTs:
#' straightforward via [stats::quantile].
#'
#' for Densities: Calculate CDF (for each pdf separately here), and then map
#' the desired probability level via the CDF (y-axis) to the time space (x-axis)
#'
#' @keywords internal
calc_quantiles_obs <- function(rts_u, rts_l, one_cond, probs) {
  quants_rts_u <- stats::quantile(rts_u, probs = probs)
  quants_rts_l <- stats::quantile(rts_l, probs = probs)
  quants <- data.frame(
    Cond = one_cond,
    Prob = probs,
    Quant_U = unname(quants_rts_u),
    Quant_L = unname(quants_rts_l)
  )
  return(quants)
}

#' @rdname calc_quantiles_obs
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

  colnames(quants) <- c("Quant_U", "Quant_L")

  quants <- as.data.frame(quants)
  quants <- cbind(Cond = one_cond, Prob = probs, quants)

  sum_pdf_l <- sum(pdf_l)
  sum_pdf_u <- sum(pdf_u)

  if (sum_pdf_u / (sum_pdf_u + sum_pdf_l) <= skip_if_contr_low) {
    quants["Quant_U"] <- NA
  }
  if (sum_pdf_l / (sum_pdf_u + sum_pdf_l) <= skip_if_contr_low) {
    quants["Quant_L"] <- NA
  }

  return(quants)
}


#' Calculate Quantiles
#'
#' Function that calls the underlying quantile calculation functions
#' [dRiftDM::calc_quantiles_obs] and [dRiftDM::calc_quantiles_pred]. Does
#' input checks and the data wrangling
#'
#' @param pdf_u,pdf_l either NULL or density vectors
#' @param t_vec the time space (required for the pdfs)
#' @param rts_u,rts_l either NULL or RT vectors
#' @param one_cond character label
#' @param probs numeric vector with values between 0 and 1 for the probability
#' levels. Default is [dRiftDM::drift_dm_default_probs()].
#' @param b_coding used for accessing the upper/lower boundary labels,
#' determines the corresponding columns of the returned data.frame
#' (e.g., Quant_`corr`).
#'
#' @returns a data.frame with "Source", "Cond", "Prob"s, "Quant_<u_label>",
#' "Quant_<l_label>" of type
#' c("quantiles", "sum_dist", "stats_dm", "data.frame")
#'
#' @details
#' if pdf_u and pdf_l are not NULL, returns quantiles for the densities
#'
#' if rts_u and rts_l are not NULL, returns quantiles for the response times
#'
#' if all are not NULL, returns both.
#'
#' @seealso [dRiftDM::new_stats_dm()]
#'
#' @keywords internal
calc_quantiles <- function(pdf_u = NULL, pdf_l = NULL, t_vec = NULL,
                           rts_u = NULL, rts_l = NULL, one_cond,
                           probs = NULL, b_coding) {
  # default settings and parameter extraction
  if (is.null(probs)) {
    probs <- drift_dm_default_probs()
  }

  if (!is.numeric(probs) | length(probs) < 2) {
    stop("probs must a valid numeric vector of length > 1")
  }

  if (min(probs) <= 0 | max(probs) >= 1) {
    stop("argument probs must be in the range ]0, 1[")
  }

  u_name <- names(b_coding$u_name_value)
  l_name <- names(b_coding$l_name_value)

  # check pdf_l and pdf_u
  if (xor(is.null(pdf_l), is.null(pdf_u))) {
    stop("pdf_l and pdf_u either have to be both NULL or not")
  }

  # check rts_u and rts_l
  if (xor(is.null(rts_u), is.null(rts_l))) {
    stop("rts_u and rts_l either have to be both NULL or not")
  }

  # calculate the respective statistics
  # if pdfs are supplied ...
  result_pred <- NULL
  if (!is.null(pdf_u)) {
    result_pred <- calc_quantiles_pred(
      pdf_u = pdf_u, pdf_l = pdf_l,
      t_vec = t_vec, one_cond = one_cond,
      probs = probs
    )
    result_pred <- cbind(Source = "pred", result_pred)

    colnames(result_pred)[which(names(result_pred) == "Quant_U")] <-
      paste("Quant", u_name, sep = "_")
    colnames(result_pred)[which(names(result_pred) == "Quant_L")] <-
      paste("Quant", l_name, sep = "_")
  }


  # if rts are supplied ...
  result_obs <- NULL
  if (!is.null(rts_u)) {
    result_obs <- calc_quantiles_obs(
      rts_u = rts_u, rts_l = rts_l,
      one_cond = one_cond, probs = probs
    )
    result_obs <- cbind(Source = "obs", result_obs)

    colnames(result_obs)[which(names(result_obs) == "Quant_U")] <-
      paste("Quant", u_name, sep = "_")
    colnames(result_obs)[which(names(result_obs) == "Quant_L")] <-
      paste("Quant", l_name, sep = "_")
  }



  # maybe combine
  result <- rbind(result_obs, result_pred)

  # make it dm_* class and pass back
  stats_dm_obj <- new_stats_dm(
    stat_df = result, type = "quantiles",
    b_coding = b_coding
  )
  return(stats_dm_obj)
}




#' Calculate delta function(s)
#'
#' Given a dataset providing the quantiles ([dRiftDM::calc_quantiles]),
#' calculates delta function(s) for the character vectors minuends and
#' subtrahends
#'
#' @param quantiles_dat a data.frame of quantiles ([dRiftDM::calc_quantiles])
#' @param minuends,subtrahends character vectors (with equal length), specifying
#' the conditions to use for the delta function: minuend - subtrahend
#' @param dvs character, indicating which quantile columns to use. Default
#' is "Quant_<u_label>". If multiple dvs are provided, then minuends and
#' subtrahends must have the same length, and matching occurs pairwise. In this
#' case, if only one minuend/subtrahend is specified, minuend and subtrahend are
#' recycled to the necessary length.
#' @param b_coding a [dRiftDM::b_coding] object, necessary to build default
#' dvs
#'
#' @details
#' Takes the quantile data_frame, [stats::reshape] it to wide, and then
#' access the relevant `dv` columns, together with minuends and subtrahends
#' to calculate the delta functions.
#'
#' @returns a data.frame with columns "Source", "Prob", the "Quant_<u_label>",
#' "Quant_<l_label". May have the following additional columns:
#'
#'  * if only one dv: as many Delta_<minuend_subtrahend> and
#'  Avg_<minuends_subtrahends> as minuends and subtrahends.
#'
#'  * if more than one dv: as many Delta_<u/l-label>_<minuend_subtrahend> and
#'  Avg_<u/l-label>_<minuends_subtrahends> as minuends and subtrahends.
#'
#'  The data.frame is of type
#'  c("delta_funs", "sum_dist", "stats_dm", "data.frame")
#'
#'
#' @seealso [dRiftDM::new_stats_dm()]
#'
#' @keywords internal
calc_delta_funs <- function(quantiles_dat, minuends = NULL, subtrahends = NULL,
                            dvs = NULL, b_coding) {
  # input checks on data frame
  if (!is.data.frame(quantiles_dat)) {
    stop("the provided quantiles_dat is not a data.frame")
  }

  u_name <- names(b_coding$u_name_value)
  l_name <- names(b_coding$l_name_value)
  quant_name_u <- paste("Quant", u_name, sep = "_")
  quant_name_l <- paste("Quant", l_name, sep = "_")

  nec_columns <- c("Source", "Cond", "Prob", quant_name_u, quant_name_l)
  if (any(colnames(quantiles_dat) != nec_columns)) {
    stop(
      "the provided quantiles_dat provides unexpected column names",
      "\n\tprovided: ", paste(colnames(quantiles_dat), collapse = " "),
      "\n\tnecessary: ", paste(nec_columns, collapse = " ")
    )
  }

  # input checks on minuends/subtrahends
  if (is.null(minuends)) {
    stop("calc_delta_funs was called but the argument minuends not provided")
  }
  if (is.null(subtrahends)) {
    stop("calc_delta_funs was called but the argument subtrahends not provided")
  }
  if (!is.character(minuends) | length(minuends) < 1) {
    stop("minuends must be a character vector of length >= 1")
  }
  if (!is.character(subtrahends) | length(subtrahends) < 1) {
    stop("subtrahends must be a character vector of length >= 1")
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
    dvs <- quant_name_u
  }
  dvs <- sapply(dvs, function(x) {
    match.arg(x, c(quant_name_u, quant_name_l))
  })
  dvs <- unname(dvs)

  if (length(dvs) > 1 & length(dvs) != length(minuends)) {
    if (length(minuends) == 1) {
      minuends <- rep(minuends, length(dvs))
      subtrahends <- rep(subtrahends, length(dvs))
    } else {
      stop(
        "if several dvs are provided, the length must match",
        " minuends/subtrahends"
      )
    }
  }

  # reduce and make wide format
  quantiles_dat <- quantiles_dat[c("Source", "Cond", "Prob", dvs)]

  n_probs <- length(unique(quantiles_dat$Prob))
  n_source <- length(unique(quantiles_dat$Source))
  n_cond <- length(unique(quantiles_dat$Cond))
  if (nrow(quantiles_dat) != n_probs * n_source * n_cond) {
    stop(
      "quantiles_dat doesn't uniquely code rows solely by Probs, Source, ",
      "and Cond"
    )
  }
  quantiles_dat <- stats::reshape(quantiles_dat,
    idvar = c("Source", "Prob"),
    timevar = "Cond", direction = "wide", sep = "_"
  )

  # calculate delta functions
  if (length(dvs) == 1) {
    delta_names <- paste("Delta",
      paste(minuends, subtrahends, sep = "_"),
      sep = "_"
    )
    avg_names <- paste("Avg",
      paste(minuends, subtrahends, sep = "_"),
      sep = "_"
    )
  } else {
    delta_names <- paste("Delta", gsub("^Quant_", "", dvs), sep = "_")
    avg_names <- paste("Avg", gsub("^Quant_", "", dvs), sep = "_")
    delta_names <- paste(delta_names,
      paste(minuends, subtrahends, sep = "_"),
      sep = "_"
    )
    avg_names <- paste(avg_names,
      paste(minuends, subtrahends, sep = "_"),
      sep = "_"
    )
  }
  minuends_wide <- paste(dvs, minuends, sep = "_")
  subtrahends_wide <- paste(dvs, subtrahends, sep = "_")


  for (i in seq_along(minuends)) {
    vals_minuends <- quantiles_dat[[minuends_wide[i]]]
    vals_subtrahends <- quantiles_dat[[subtrahends_wide[i]]]

    quantiles_dat[[delta_names[i]]] <- vals_minuends - vals_subtrahends
    quantiles_dat[[avg_names[i]]] <- 0.5 * vals_minuends + 0.5 * vals_subtrahends
  }

  # make it stats_dm class and pass back
  # since delta_funs depends on quantiles, this will remove the quantiles class
  stats_dm_obj <- new_stats_dm(
    stat_df = quantiles_dat, type = "delta_funs",
    b_coding = b_coding
  )
  return(stats_dm_obj)
}



# FUNCTIONS FOR FIT STATISTICS --------------------------------------------



#' Calculate Information Criteria (AIC and BIC)
#'
#' Computes/Summarizes the Log-Likelihood, Akaike Information Criterion (AIC),
#' and the Bayesian Information Criterion (BIC)
#'
#' @param drift_dm_obj an object of type [dRiftDM::drift_dm]
#' @param ... further arguments (only relevant: k, for the penality of
#' [stats::AIC])
#'
#' @details The functions calls [dRiftDM::logLik.drift_dm], and subsequently
#' [stats::AIC] and [stats::BIC]
#'
#' @return A custom object of class `stats_dm`
#' (c("fit_stats", "stats_dm", "data.frame")), containing a data frame with
#' columns:
#' * `Log_Like`: the input log-likelihood value
#' * `AIC`: the calculated AIC value
#' * `BIC`: the calculated BIC value
#'
#'
#' @seealso [dRiftDM::new_stats_dm()], [dRiftDM::logLik.drift_dm]
#'
#' @keywords internal
calc_ic <- function(drift_dm_obj, ...) {
  dots <- list(...)
  if (is.null(dots$k)) {
    k <- 2
  } else {
    k <- dots$k
  }

  ll <- logLik(drift_dm_obj)
  aic <- stats::AIC(ll, k = k)
  bic <- stats::BIC(ll)

  result <- data.frame(Log_Like = ll, AIC = aic, BIC = bic)
  stats_obj <- new_stats_dm(stat_df = result, type = "fit_stats")
  return(stats_obj)
}





# ACCESS FUNCTION FOR EACH STATISTIC --------------------------------------

#' Calculate Statistics for Model Prediction and/or Observed Data
#'
#' This function derives statistics that can be calculated for both model
#' predictions and observed data. However, it does not calculate it, but
#' rather calls the respective backend functions.
#' Supported statistics currently include:
#' - Conditional Accuracy Functions (CAFs; [dRiftDM::calc_cafs()])
#' - Quantiles ([dRiftDM::calc_quantiles()])
#' - Delta Functions ([dRiftDM::calc_delta_funs()])
#'
#' @param type character string, specifying the type of statistic to calculate.
#'   Available options are `"cafs"`, `"quantiles"`, and `"delta_funs"`.
#' @param b_coding list for boundary coding (see [dRiftDM::b_coding]).
#' @param conds character vector, specifying the conditions to include in
#' calculations (used for labeling and subsetting the model PDFs and the
#' observed data).
#' @param ... Additional parameters passed on to the specific statistic
#' calculation function (see Details).
#'
#' @details
#'
#' When calling this function the arguments `all_rts_u`/`all_rts_l` and/or
#' `all_pdfs` must be specified (see
#' [dRiftDM::re_evaluate_model], [dRiftDM::obs_data]). Otherwise, the backend
#' functions won't work properly. Further arguments are:
#'
#' - for CAFS: `n_bins` controls the number of bins, with a default of 5.
#' - for Quantiles and Delta Functions: `probs` c ontrols the quantiles to
#' calculate. Default is `seq(0.1, 0.9, 0.1)`
#' (see [dRiftDM::drift_dm_default_probs()]).
#'
#' This function gets called by [dRiftDM::calc_stats]
#'
#'
#' @return A data frame with the calculated statistic across `conds`
#'  (ordered according to `Source`).
#'
#' @keywords internal
calc_stats_pred_obs <- function(type, b_coding, conds, ...) {
  dotdot <- list(...)

  if (!is.character(type) | length(type) != 1) {
    stop("type must be a single character vector of length 1")
  }

  # only these types are available for both observed and predicted data
  type <- match.arg(type, c("cafs", "quantiles", "delta_funs"))

  # iterate through the requested types and calculate the stats
  if (type == "quantiles") {
    result <-
      lapply(conds, function(one_cond) {
        pdf_u <- dotdot$all_pdfs[[one_cond]]$pdf_u
        pdf_l <- dotdot$all_pdfs[[one_cond]]$pdf_l
        rts_u <- dotdot$all_rts_u[[one_cond]]
        rts_l <- dotdot$all_rts_l[[one_cond]]

        calc_quantiles(
          pdf_u = pdf_u, pdf_l = pdf_l, t_vec = dotdot$t_vec,
          rts_u = rts_u, rts_l = rts_l,
          one_cond = one_cond, probs = dotdot$probs,
          b_coding = b_coding
        )
      })
    result <- do.call("rbind", result)
  }
  if (type == "cafs") {
    result <-
      lapply(conds, function(one_cond) {
        pdf_u <- dotdot$all_pdfs[[one_cond]]$pdf_u
        pdf_l <- dotdot$all_pdfs[[one_cond]]$pdf_l
        rts_u <- dotdot$all_rts_u[[one_cond]]
        rts_l <- dotdot$all_rts_l[[one_cond]]

        calc_cafs(
          pdf_u = pdf_u, pdf_l = pdf_l, rts_u = rts_u,
          rts_l = rts_l, one_cond = one_cond,
          n_bins = dotdot$n_bins, b_coding = b_coding
        )
      })
    result <- do.call("rbind", result)
  }
  if (type == "delta_funs") {
    interim <- calc_stats_pred_obs(
      type = "quantiles", b_coding = b_coding, conds = conds, ...
    )
    result <- calc_delta_funs(
      quantiles_dat = interim,
      minuends = dotdot$minuends,
      subtrahends = dotdot$subtrahends,
      dvs = dotdot$dvs, b_coding = b_coding
    )
  }

  result <- result[order(result$Source), ]
  rownames(result) <- NULL

  return(result)
}




# METHODS FOR drift_dm and data.frame OBJECTS TO CALCULATE STATS  ----------


#' Calculate Statistics
#'
#' @description
#'
#' `calc_stats` provides an interface for calculating statistics/metrics on
#' model predictions and/or observed data. Supported statistics include
#' Conditional Accuracy Functions (CAFs), Quantiles, Delta Functions, and Fit
#' Statistics. Results can be aggregated across individuals.
#'
#' @param object an object for which statistics are calculated. This can be a
#' [data.frame] of observed data, a [dRiftDM::drift_dm] object, or a
#' `fits_ids_dm` object (see [dRiftDM::estimate_model_ids]).
#' @param type a character vector, specifying the statistics to calculate.
#' Supported values include `"cafs"`, `"quantiles"`, `"delta_funs"`, and
#' `"fit_stats"`.
#' @param ... additional arguments passed to the respective method and the
#' underlying calculation functions (see Details for mandatory arguments).
#' @param conds optional character vector specifying conditions to include.
#' Conditions must match those found in the `object`.
#' @param split_by_ID logical. If `TRUE`, statistics are calculated separately
#' for each individual ID in `object` (when `object` is a [data.frame]). Default
#' is `TRUE`.
#' @param b_coding a list for boundary coding (see [dRiftDM::b_coding]). Only
#' relevant when `object` is a [data.frame]. For other `object` types, the
#' `b_coding` of the `Object` is used.
#' @param average logical. If `TRUE`, averages the statistics across individuals
#' where applicable. Default is `FALSE`.
#' @param verbose integer, indicating if information about the progress
#'  should be displayed. 0 -> no information, 1 -> a progress bar. Default is 0.
#'
#' @details
#' `calc_stats` is a generic function to handle the calculation of different
#' statistics/metrics for the supported object types. Per default, it returns
#' the requested statistics/metrics.
#'
#'
#'
#' ## Conditional Accuracy Function (CAFs)
#'
#' CAFs are a way to quantify response accuracy against speed. To calculate
#' CAFs, RTs (whether correct or incorrect) are first binned and then the
#' percent correct responses per bin is calculated.
#'
#' When calculating model-based CAFs, a joint CDF combining both the pdf
#' of correct and incorrect responses is calculated. Afterwards, this CDF
#' is separated into even-spaced segments and the contribution of
#' the pdf associated with a correct response relative to the joint CDF is
#' calculated.
#'
#' The number of bins can be controlled by passing the argument `n_bins`.
#' The default is 5.
#'
#' ## Quantiles
#'
#'  For observed response times, the function [stats::quantile] is used with
#'  default settings.
#'
#'  Which quantiles are calcuated can be controlled by providing the
#'  probabilites, `probs`, with values in \eqn{[0, 1]}. Default is
#'  `seq(0.1, 0.9, 0.1)`.
#'
#' ## Delta Functions
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
#'    `conds(drift_dm_obj)`.
#'  - `subtrahends`: character vector, specifying condition(s) k. Must be in
#'    `conds(drift_dm_obj)`
#'  - `dvs`: character, indicating which quantile columns to use.
#'     Default is "Quant_<u_label>". If multiple dvs are provided,
#'     then minuends and subtrahends must have the same length,
#'     and matching occurs pairwise. In this case, if only one
#'     minuend/subtrahend is specified, minuend and subtrahend are recycled to
#'     the necessary length.
#'
#'
#' ## Fit Statistics
#'
#' Calculates the Akaike and Bayesian Information Criteria (AIC and BIC). Users
#' can provide a `k` argument to penalize the AIC statistic (see [stats::AIC]
#' and [dRiftDM::AIC.fits_ids_dm])
#'
#' @returns
#' If `type` is a single character string, then a [data.frame] is returned.
#' If `type` contains multiple character strings (i.e., is a character vector)
#' a list with the calculated statistics (with entries being [data.frame]s) is
#' returned.
#'
#' Each returned [data.frame] has a certain class label and may store additional
#' attributes required for the custom `plot()` functions. If a list is returned,
#' then that list will have the class label `list_stats_dm` (to easily create
#' multiple panels using the respective `plot()` method).
#'
#' @examples
#' # Example 1: Calculate CAFs and Quantiles from a model ---------------------
#' # get a model for demonstration purpose
#' a_model <- ssp_dm(dx = .0025, dt = .0025, t_max = 2)
#' # and then calculate cafs and quantiles
#' some_stats <- calc_stats(a_model, type = c("cafs", "quantiles"))
#' head(some_stats$cafs)
#' head(some_stats$quantiles)
#'
#' # Example 2: Calculate a Delta Function from a data.frame ------------------
#' # get a data set for demonstration purpose
#' some_data <- ulrich_simon_data
#' conds(some_data) # relevant for minuends and subtrahends
#' some_stats <- calc_stats(
#'   a_model,
#'   type = "delta_funs",
#'   minuends = "incomp",
#'   subtrahends = "comp"
#' )
#' head(some_stats)
#'
#'
#' # Example 3: Calculate Quantiles from a fits_ids_dm object -----------------
#' # get an auxiliary fits_ids_dm object
#' all_fits <- get_example_fits_ids()
#' some_stats <- calc_stats(all_fits, type = "quantiles")
#' head(some_stats) # note the ID column
#'
#' # one can also request that the statistics are averaged across individuals
#' head(
#'   calc_stats(all_fits, type = "quantiles", average = TRUE)
#' )
#'
#' @export
calc_stats <- function(object, type, ...) {
  if (length(type) > 1) {
    all_stats <- sapply(type, function(one_type) {
      calc_stats(object = object, type = one_type, ...)
    }, simplify = FALSE, USE.NAMES = TRUE)

    class(all_stats) <- c("list_stats_dm", "list")
    return(all_stats)
  }

  UseMethod("calc_stats")
}


#' @rdname calc_stats
#' @export
calc_stats.data.frame <- function(object, type, ..., conds = NULL, verbose = 0,
                                  average = FALSE, split_by_ID = TRUE,
                                  b_coding = NULL) {
  obs_data <- object

  # verbose check
  if (!(verbose %in% c(0, 1))) {
    stop("verbose must be 0 or 1")
  }

  # get b_coding
  if (is.null(b_coding)) {
    b_coding <- drift_dm_default_b_coding()
  }

  #  split by ID, but check if it actually exists
  if (split_by_ID & ("ID" %in% colnames(obs_data))) {
    obs_data <- check_raw_data(
      obs_data = obs_data,
      b_coding_column = b_coding$column,
      u_value = b_coding$u_name_value,
      l_value = b_coding$l_name_value
    )
    list_obs_data <- split(x = obs_data, f = obs_data$ID)


    # create a progress bar
    if (verbose == 1) {
      n_iter <- length(list_obs_data)
      pb <- progress::progress_bar$new(
        format = "calculating [:bar] :percent; done in: :eta",
        total = n_iter, clear = FALSE, width = 60
      )
      pb$tick(0)
    }

    all_results <- lapply(names(list_obs_data), function(id) {
      stat <- calc_stats(
        object = list_obs_data[[id]], type = type, ...,
        conds = conds, verbose = 0, average = FALSE, split_by_ID = FALSE,
        b_coding = b_coding
      )
      stat_id <- cbind(ID = try_cast_integer(id), stat)
      stat_id <- copy_class_attributes(old = stat, new = stat_id)
      if (verbose == 1) pb$tick()
      return(stat_id)
    })
    results <- do.call("rbind", all_results) # preserves class and attributes
    results <- results[order(results$ID), ]
    rownames(results) <- NULL
  } else { # else "ignore" ID column (also the case when it does not exist)

    # turn to list of rts, as stored in any dm_object
    rts <- obs_data_to_rt_lists(obs_data = obs_data, b_coding = b_coding)

    all_rts_u <- rts$rts_u
    all_rts_l <- rts$rts_l

    stopifnot(all(names(all_rts_u) == names(all_rts_l)))


    # get the conds
    data_conds <- unique(obs_data$Cond)
    if (is.null(conds)) {
      conds <- data_conds
    } else {
      conds <- match.arg(arg = conds, choices = data_conds, several.ok = TRUE)
    }

    # call the internal calc_stats function
    results <- calc_stats_pred_obs(
      type = type, b_coding = b_coding,
      conds = conds,
      all_rts_u = all_rts_u,
      all_rts_l = all_rts_l, ...
    )
  }

  # average if necessary
  if (average) {
    results <- aggregate_stats(results)
  }

  return(results)
}


#' @rdname calc_stats
#' @export
calc_stats.drift_dm <- function(object, type, ..., conds = NULL) {
  drift_dm_obj <- object
  type <- match.arg(type, c("cafs", "quantiles", "delta_funs", "fit_stats"))


  # get b_coding
  b_coding <- attr(drift_dm_obj, "b_coding")

  # get all rts and pdfs for quick reference
  if (is.null(drift_dm_obj$pdfs)) {
    drift_dm_obj <- re_evaluate_model(
      drift_dm_obj = drift_dm_obj,
      eval_model = TRUE
    )
  }

  # if fit_stats requested
  if (type == "fit_stats") {
    result <- calc_ic(drift_dm_obj, ...)
    return(result)
  }


  # otherwise cafs, quantiles, delta_funs ....
  # extract pdfs and check if at least 1% of the PDFs is missing
  all_pdfs <- drift_dm_obj$pdfs
  dt <- drift_dm_obj$prms_solve[["dt"]]
  check_loss <- sapply(all_pdfs, function(one_set_pdfs) {
    sum_both <- sum(one_set_pdfs$pdf_u) * dt + sum(one_set_pdfs$pdf_l) * dt
    return(sum_both < .99)
  }, simplify = TRUE, USE.NAMES = TRUE)
  if (any(check_loss)) {
    warning(
      "calc_stats called with missing probability mass for some ",
      "conditions (likely occured after truncating pdfs to the ",
      "time space). Some statistics scale the pdfs! ",
      "Interprete 'quantiles' etc. accordingly (or increase t_max)."
    )
  }


  # get time space (needed for quantiles)
  t_max <- drift_dm_obj$prms_solve[["t_max"]]
  nt <- drift_dm_obj$prms_solve[["nt"]]
  t_vec <- seq(0, t_max, length.out = nt + 1)

  # get the conds
  model_conds <- conds(drift_dm_obj)
  if (is.null(conds)) {
    conds <- model_conds
  } else {
    conds <- match.arg(arg = conds, choices = model_conds, several.ok = TRUE)
  }

  # and call the underlying internal function calc_stats_pred_obs
  result <- calc_stats_pred_obs(
    type = type, b_coding = b_coding,
    conds = conds, all_pdfs = all_pdfs,
    all_rts_u = drift_dm_obj$obs_data$rts_u,
    all_rts_l = drift_dm_obj$obs_data$rts_l,
    t_vec = t_vec, ...
  )

  return(result)
}


#' @rdname calc_stats
#' @export
calc_stats.fits_ids_dm <- function(object, type, ..., verbose = 1,
                                   average = FALSE) {
  fits_ids <- object

  if (!(verbose %in% c(0, 1))) {
    stop("verbose must be 0 or 1")
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



  # call statistic across individuals
  all_results <-
    lapply(names(fits_ids$all_fits), function(id) {
      stat <- calc_stats(object = fits_ids$all_fits[[id]], type = type, ...)
      stat_id <- cbind(ID = try_cast_integer(id), stat)
      stat_id <- copy_class_attributes(old = stat, new = stat_id)
      if (verbose == 1) pb$tick()
      return(stat_id)
    })
  results <- do.call("rbind", all_results) # preserves class and attributes
  results <- results[order(results$ID), ]
  rownames(results) <- NULL

  # average if desired
  if (average) {
    results <- aggregate_stats(results)
  }

  return(results)
}


# CREATE stats_dm objects -------------------------------------------------


#' Create a New stats_dm Object
#'
#' @description
#' `new_stats_dm` initializes a `stats_dm` object to label statistic types and
#' store necessary attributes for the custom methods (such as `plot_*`)
#'
#'
#' @param stat_df a `data.frame`, containing calculated statistics to be
#' encapsulated within the `stats_dm` class.
#' @param type a character string, specifying the type of statistic provided by
#' `stat_df`. Valid options include `"cafs"`, `"quantiles"`, `"delta_funs"`,
#' and `"fit_stats"`.
#' @param ... Additional arguments passed to set attributes. For `"cafs"`,
#' `"quantiles"`, and `"delta_funs"`, a `b_coding` attribute is required.
#'
#' @details
#' `new_stats_dm` sets up the `stat_df` object by assigning it the class
#' `stats_dm`, along with additional classes based on the specified `type`.
#' For"cafs", "quantiles", "delta_funs", this will be c(`<type>`, "sum_dist",
#' "stats_dm", "data.frame")". For fit statistics, this will be c("fit_stats",
#' "stats_dm", "data.frame")".
#'
#' For Conditional Accuracy Functions (CAFs), Quantiles, and Delta Functions,
#' the function requires a `b_coding` argument, which specifies boundary coding
#' details and is set as an attribute.
#'
#' The function performs validation through [dRiftDM::validate_stats_dm] to
#' ensure that the `stats_dm` object is well formatted.
#'
#' @return An object of class `stats_dm`, with additional classes and attributes
#' depending on `type`.
#'
#' @keywords internal
new_stats_dm <- function(stat_df, type, ...) {
  # input checks
  stopifnot(is.data.frame(stat_df))
  type <- match.arg(
    arg = type,
    choices = c("cafs", "quantiles", "delta_funs", "fit_stats")
  )

  # define the stat_df object as an object of class stats_dm
  class(stat_df) <- c("stats_dm", "data.frame")

  # if cafs, quantiles, delta_funs, add b_coding and more info about object
  dots <- list(...)
  if (type %in% c("cafs", "quantiles", "delta_funs")) {
    b_coding <- dots$b_coding
    stopifnot(!is.null(b_coding))
    attr(stat_df, "b_coding") <- b_coding
    class(stat_df) <- c(type, "sum_dist", class(stat_df))

    # if fit_stats, add the object info
  } else if (type == "fit_stats") {
    class(stat_df) <- c(type, class(stat_df))
  }

  # check if everything went well
  stat_df <- validate_stats_dm(stat_df)
  return(stat_df)
}




# VALIDATE stats_dm objects -----------------------------------------------


#' Validate a stats_dm Object
#'
#' @description
#' `validate_stats_dm` is an internal (i.e., not exported) generic function to
#' ensure that `stats_dm` objects, as well as their specific subclasses
#' (`cafs`, `quantiles`, `delta_funs`, `sum_dist`, and `fit_stats`), meet the
#' necessary structural and column requirements. Each method performs
#' class-specific validation checks.
#'
#' @param stat_df A `data.frame` of class `stats_dm`, `cafs`, `quantiles`,
#' `delta_funs`, `sum_dist`, or `fit_stats` containing the calculated statistics
#' to be validated.
#'
#' @details
#' The validation process checks for required columns and structure based on the
#' class of `stat_df`. Each class has specific requirements:
#'
#' - **`validate_stats_dm.stats_dm`:** Ensures `stat_df` is a `data.frame`.
#' - **`validate_stats_dm.cafs`:** Checks for the presence of `"Bin"`, `"Cond"`,
#'   and exactly one column prefixed with `"P_"`
#' - **`validate_stats_dm.quantiles`:** Requires `"Prob"`, `"Cond"`, and exactly
#'   two columns prefixed with `"Quant_"`
#' - **`validate_stats_dm.delta_funs`:** Ensures `"Prob"` exists, at least two
#'   columns prefixed with `"Quant_"`, and at least one column  each `Avg_`
#'   and `Delta_`
#' - **`validate_stats_dm.sum_dist`:** Checks for a `"Source"` column.
#' - **`validate_stats_dm.fit_stats`:** Checks for `"Log_Like"`, `"AIC"`, and
#' `"BIC"` columns.
#'
#'
#' @return Returns the unmodified `stat_df` for convenience.
#'
#' @keywords internal
validate_stats_dm <- function(stat_df) {
  UseMethod("validate_stats_dm")
}


#' @rdname validate_stats_dm
#' @export
validate_stats_dm.cafs <- function(stat_df) {
  NextMethod() # to validate sum_dist objects


  if (!("Bin" %in% colnames(stat_df))) {
    stop("no column 'Bin' in stats_dm")
  }

  if (!("Cond" %in% colnames(stat_df))) {
    stop("no column 'Cond' in stats_dm")
  }

  if (sum("P_" == substr(colnames(stat_df), 1, 2)) != 1) {
    stop("no unique column 'P_' in stats_dm")
  }
  return(stat_df)
}

#' @rdname validate_stats_dm
#' @export
validate_stats_dm.quantiles <- function(stat_df) {
  NextMethod() # to validate sum_dist objects

  if (!("Prob" %in% colnames(stat_df))) {
    stop("no column 'Prob' in stats_dm")
  }

  if (!("Cond" %in% colnames(stat_df))) {
    stop("no column 'Cond' in stats_dm")
  }

  if (sum("Quant_" == substr(colnames(stat_df), 1, 6)) != 2) {
    stop("couldn't find two Quant_ columns")
  }

  return(stat_df)
}

#' @rdname validate_stats_dm
#' @export
validate_stats_dm.delta_funs <- function(stat_df) {
  NextMethod() # to validate sum_dist objects

  if (!("Prob" %in% colnames(stat_df))) {
    stop("no column 'Prob' in stats_dm")
  }

  if (sum("Quant_" == substr(colnames(stat_df), 1, 6)) < 2) {
    stop("couldn't find at least two Quant_ columns")
  }

  if (sum("Avg_" == substr(colnames(stat_df), 1, 4)) < 1) {
    stop("couldn't find a Avg_ column")
  }
  if (sum("Delta_" == substr(colnames(stat_df), 1, 6)) < 1) {
    stop("couldn't find a Delta_ column")
  }
  return(stat_df)
}

#' @rdname validate_stats_dm
#' @export
validate_stats_dm.sum_dist <- function(stat_df) {
  NextMethod() # to validate stats_dm objects

  if (!("Source" %in% colnames(stat_df))) {
    stop("no column 'Source' in stats_dm")
  }
  if (is.null(attr(stat_df, "b_coding"))) {
    stop("no attribute b_coding in stats_dm")
  }
  return(stat_df)
}



#' @rdname validate_stats_dm
#' @export
validate_stats_dm.fit_stats <- function(stat_df) {
  NextMethod() # to validate stats_dm objects

  if (!("Log_Like" %in% colnames(stat_df))) {
    stop("no column 'Log_Like' in stats_dm")
  }

  if (!("AIC" %in% colnames(stat_df))) {
    stop("no column 'AIC' in stats_dm")
  }

  if (!("BIC" %in% colnames(stat_df))) {
    stop("no column 'BIC' in stats_dm")
  }
  return(stat_df)
}


#' @rdname validate_stats_dm
#' @export
validate_stats_dm.stats_dm <- function(stat_df) {
  if (!is.data.frame(stat_df)) {
    stop("stats_dm object to validate is not of type data.frame")
  }
  return(stat_df)
}





# AGGREGATE stats_dm OBJECTS ----------------------------------------------

#' Aggregate Statistics ACROSS ID
#'
#' @description
#' `aggregate_stats` is a (not exported) generic function to aggregate
#' `stats_dm` objects across `ID`s. Since the column names may vary by the
#' statistic type, the behavior of aggregate depends on the subclass of
#' `stats_dm` (`cafs`, `quantiles`, `delta_funs`, or `fit_stats`).
#'
#' @param stat_df A `data.frame` of class `stats_dm`
#'
#' @details
#' For each supported subclass, `aggregate_stats` calls
#' [dRiftDM::internal_aggregate()] with the relevant arguments
#'
#' @return If no `"ID"` column exists in `stat_df` returns `stat_df` as-is.
#' If an `"ID"` column exists, then statistics are aggregated across it.
#'
#' @seealso [dRiftDM::new_stats_dm], [dRiftDM::calc_stats],
#' [dRiftDM::internal_aggregate()]
#'
#' @keywords internal
aggregate_stats <- function(stat_df) {
  if (!("ID" %in% colnames(stat_df))) {
    return(stat_df)
  }

  UseMethod("aggregate_stats")
}

#' @rdname aggregate_stats
#' @export
aggregate_stats.cafs <- function(stat_df) {
  internal_aggregate(
    data = stat_df,
    group_cols = c("Source", "Cond", "Bin")
  )
}

#' @rdname aggregate_stats
#' @export
aggregate_stats.quantiles <- function(stat_df) {
  internal_aggregate(
    data = stat_df,
    group_cols = c("Source", "Cond", "Prob")
  )
}

#' @rdname aggregate_stats
#' @export
aggregate_stats.delta_funs <- function(stat_df) {
  internal_aggregate(
    data = stat_df,
    group_cols = c("Source", "Prob")
  )
}

#' @rdname aggregate_stats
#' @export
aggregate_stats.fit_stats <- function(stat_df) {
  internal_aggregate(
    data = stat_df,
    group_cols = NULL
  )
}



#' Aggregate Data Frame Columns by Group
#'
#' internal function to aggregate columns of a data frame across "ID"
#' while considering a set of grouping columns. It retains the class and
#' attriubtes of the input data.
#'
#' @param data A [data.frame] containing the data to be aggregated. It should
#'  include both the grouping columns, an "ID" column, and the columns for which
#'  aggregation shall take place.
#' @param group_cols A character vector specifying the names of the columns to
#'  group by during aggregation.
#'
#' @details
#' `internal_aggregate` identifies DV columns as those not in `group_cols` or
#' `"ID"`. It then calculates the mean of these DV columns, grouped by the
#' specified columns.
#'
#' @return A `data.frame` containing the aggregated data.
#'
#' @seealso [dRiftDM::aggregate_stats()], [dRiftDM::calc_stats()],
#' [dRiftDM::new_stats_dm()]
#'
#' @keywords internal
internal_aggregate <- function(data, group_cols) {
  all_cols <- colnames(data)

  # Select columns that don't start with the group_cols or ID
  dv_cols <- all_cols[!(colnames(data) %in% c("ID", group_cols))]

  # Aggregate by ID for those columns
  agg_df <- stats::aggregate(data[dv_cols],
    data[rev(group_cols)],
    FUN = mean,
    na.rm = TRUE
  )

  # reorder columns to have consistency with the supplied data.frame
  agg_df <- agg_df[c(group_cols, dv_cols)]

  # keep class and attributes and pass back
  agg_df <- copy_class_attributes(old = data, new = agg_df)

  agg_df <- validate_stats_dm(agg_df)

  return(agg_df)
}



# HELPER TO KEEP CLASS/ATTRIBUTS  --------------------------------------------

#' Copy Class Attributes from One Object to Another
#'
#' This function transfers class attributes from an `old` object to a `new`
#' object, ensuring that `new` inherits the class structure and missing
#' attributes of `old`. The primary purpose is to enforce class consistency and
#' restore any lost attributes when modifying or combining objects. It is
#' used in the internals of the package and it is not exported.
#'
#' @param old The source object from which class attributes will be copied.
#' @param new The target object to which class attributes will be assigned.
#' @return The modified `new` object with attributes and class from `old`.
#'
#' @details
#' The function assumes that all class attributes of `new` can be found in
#' `old`. Note also, that the order of attributes is not ensured.
#'
#' @keywords internal
copy_class_attributes <- function(old, new) {
  UseMethod("copy_class_attributes")
}


#' @rdname copy_class_attributes
#' @export
copy_class_attributes.stats_dm <- function(old, new) {
  stopifnot(class(new) %in% class(old))
  lost_attribtues <- setdiff(names(attributes(old)), names(attributes(new)))

  class(new) <- class(old) # ensures sorting
  for (one_attr in lost_attribtues) { # doesn't ensure sorting
    attr(new, one_attr) <- attr(old, one_attr)
  }

  return(new)
}
