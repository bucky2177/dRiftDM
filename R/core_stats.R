# ==== FUNCTIONS FOR CALCULATING THE CAF

calc_cafs_obs <- function(drift_dm_obj, n_bins) {
  if (is.null(drift_dm_obj$obs_data)) {
    warning(
      "CAF of observed values requested, but no data can be found.",
      " Please double-check your model"
    )
    return(NULL)
  }


  all_rts_corr <- drift_dm_obj$obs_data$rts_corr
  all_rts_err <- drift_dm_obj$obs_data$rts_err
  stopifnot(all(names(all_rts_corr) == names(all_rts_err)))

  # rts_corr and rts_err are a list of rts. Calculates the caf using recursion
  caf_temp <- function(list_rts_corr, list_rts_err, n_bins) {
    stopifnot(length(list_rts_corr) == length(list_rts_err))

    if (length(list_rts_corr) == 1) {
      rts_corr <- list_rts_corr[[1]]
      rts_err <- list_rts_err[[1]]
      rts <- c(rts_corr, rts_err)
      probs <- seq(0, 1, length.out = n_bins + 1)
      borders <- stats::quantile(rts, probs = probs)
      bins <- cut(rts, breaks = borders, labels = FALSE, include.lowest = TRUE)
      stopifnot(sort(unique(bins)) == 1:n_bins)
      corr <- rep(c(1, 0), times = c(length(rts_corr), length(rts_err)))
      caf <- tapply(corr, bins, mean)
      caf <- data.frame(
        Cond = names(list_rts_corr),
        Bin = names(caf),
        P_Corr = as.numeric(caf)
      )
      return(list(caf))
    }

    container <- c(
      caf_temp(list_rts_corr[1], list_rts_err[1], n_bins),
      caf_temp(list_rts_corr[-1], list_rts_err[-1], n_bins)
    )
    return(container)
  }

  all_cafs <- do.call("rbind", caf_temp(all_rts_corr, all_rts_err, n_bins))
  return(all_cafs)
}

calc_cafs_pred <- function(drift_dm_obj, n_bins) {
  calc_temp <- function(pdf_u, pdf_l, one_cond, n_bins) {
    stopifnot(length(pdf_u) == length(pdf_l))

    # make a cdf and scale it to a value between 0 and 1
    cdf <- pdf_u + pdf_l
    cdf <- cumsum(cdf)
    cdf <- cdf - min(cdf)
    cdf <- cdf / max(cdf)

    x <- 1:length(cdf)
    probs <- seq(0, 1, length.out = n_bins + 1)
    probs = probs[2:(length(probs) - 1)]
    x_borders <- stats::approx(x = cdf, y = x, xout = probs, ties = "mean")$y
    x_borders = append(x_borders, min(x), after = 0) # ensure that x_borders
    x_borders = append(x_borders, max(x)) # contains the lower and upper part
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

  all_cafs <- lapply(drift_dm_obj$conds,
    function(one_cond, drift_dm_obj, n_bins) {
      pdfs <- get_pdfs(drift_dm_obj,
        one_cond = one_cond,
        solver = drift_dm_obj$solver
      )
      return(calc_temp(pdfs$pdf_u, pdfs$pdf_l, one_cond, n_bins))
    },
    drift_dm_obj = drift_dm_obj, n_bins = n_bins
  )

  all_cafs <- do.call("rbind", all_cafs)
  return(all_cafs)
}

#' @export
calc_cafs <- function(drift_dm_obj, type = "obs", n_bins = 5) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }
  if (!is.numeric(n_bins) | length(n_bins) != 1)
    stop("n_bins must a single numeric")
  if (n_bins <= 1) {
    stop("argument n_bins nust be larger than 1")
  }
  type <- match.arg(type, c("obs", "pred", "both"))

  if (type == "obs") {
    return(calc_cafs_obs(drift_dm_obj = drift_dm_obj, n_bins = n_bins))
  }

  if (type == "pred") {
    return(calc_cafs_pred(drift_dm_obj = drift_dm_obj, n_bins = n_bins))
  }

  if (type == "both") {
    obs <- calc_cafs_obs(drift_dm_obj = drift_dm_obj, n_bins = n_bins)
    if (!is.null(obs)) {
      obs <- cbind(Source = "obs", obs)
    }
    pred <- calc_cafs_pred(drift_dm_obj = drift_dm_obj, n_bins = n_bins)
    pred <- cbind(Source = "pred", pred)
    return(rbind(obs, pred))
  }
}




# ==== FUNCTIONS FOR CALCULATING QUANTILES
calc_quantiles_obs <- function(drift_dm_obj, probs) {
  if (is.null(drift_dm_obj$obs_data)) {
    warning(
      "Quantiles of observed values requested, but no data can be found.",
      " Please double-check your model"
    )
    return(NULL)
  }

  all_rts_corr <- drift_dm_obj$obs_data$rts_corr
  all_rts_err <- drift_dm_obj$obs_data$rts_err
  stopifnot(all(names(all_rts_corr) == names(all_rts_err)))

  # rts_corr and rts_err are a list of rts.
  # Calculates the quantiles using recursion
  quantile_temp <- function(list_rts_corr, list_rts_err) {
    stopifnot(length(list_rts_corr) == length(list_rts_err))
    stopifnot(names(list_rts_corr) == names(list_rts_err))

    if (length(list_rts_corr) == 1) {
      quants_rts_corr <- stats::quantile(list_rts_corr[[1]], probs = probs)
      quants_rts_err <- stats::quantile(list_rts_err[[1]], probs = probs)
      quants_rts_corr <- unname(quants_rts_corr)
      quants_rts_err <- unname(quants_rts_err)
      quants <- data.frame(
        Cond = names(list_rts_corr),
        Prob = probs,
        Quant_Corr = quants_rts_corr,
        Quant_Err = quants_rts_err
      )
      return(list(quants))
    }

    container <- c(
      quantile_temp(list_rts_corr[1], list_rts_err[1]),
      quantile_temp(list_rts_corr[-1], list_rts_err[-1])
    )
    return(container)
  }

  all_quants <- do.call("rbind", quantile_temp(all_rts_corr, all_rts_err))
  return(all_quants)
}

calc_quantiles_pred <- function(drift_dm_obj, probs) {
  calc_temp <- function(pdf_u, pdf_l, t_vec, probs, one_cond) {
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

  # create
  t_max <- drift_dm_obj$prms_solve[["t_max"]]
  nt <- drift_dm_obj$prms_solve[["nt"]]
  t_vec <- seq(0, t_max, length.out = nt + 1)

  all_quants <- lapply(drift_dm_obj$conds,
    function(one_cond, drift_dm_obj, t_vec, probs) {
      pdfs <- get_pdfs(drift_dm_obj,
        one_cond = one_cond,
        solver = drift_dm_obj$solver
      )
      return(calc_temp(pdfs$pdf_u, pdfs$pdf_l, t_vec, probs, one_cond))
    },
    drift_dm_obj = drift_dm_obj, t_vec = t_vec, probs = probs
  )

  all_quants <- do.call("rbind", all_quants)
  return(all_quants)
}


#' @export
calc_quantiles <- function(drift_dm_obj, type = "obs",
                           probs = seq(0.1, 0.9, 0.1)) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }
  if (!is.numeric(probs) | length(probs) < 2)
    stop("probs must a numeric vector of length > 1")

  if (min(probs) <= 0 | max(probs) >= 1) {
    stop("argument probs must be in the range ]0, 1[")
  }

  type <- match.arg(type, c("obs", "pred", "both"))

  if (type == "obs") {
    return(calc_quantiles_obs(drift_dm_obj = drift_dm_obj, probs = probs))
  }

  if (type == "pred") {
    return(calc_quantiles_pred(drift_dm_obj = drift_dm_obj, probs = probs))
  }

  if (type == "both") {
    obs <- calc_quantiles_obs(drift_dm_obj = drift_dm_obj, probs = probs)
    if (!is.null(obs)) {
      obs <- cbind(Source = "obs", obs)
    }
    pred <- calc_quantiles_pred(drift_dm_obj = drift_dm_obj, probs = probs)
    pred <- cbind(Source = "pred", pred)
    return(rbind(obs, pred))
  }
}
