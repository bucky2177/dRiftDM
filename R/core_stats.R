# FUNCTIONS FOR DENSITY/DISTRIBUTION --------------------------------------

#' Calculate and Combine Density Estimates for Observed and Predicted Data
#'
#' Internal helper functions that return a `data.frame` summarizing density
#' values for observed and predicted response times.
#'
#' @details
#' `calc_dens_obs()` computes empirical histograms and kernel density
#' estimates for a single condition based on observed RTs.
#'
#' `calc_dens()` serves as a general interface that combines observed and
#' predicted data into a single `data.frame`. Observed data (`rts_u` and
#' `rts_l`) is passed to `calc_dens_obs()`. Predicted data (`pdf_u` and
#' `pdf_l`) is wrapped into a `data.frame` that matches the structure returned
#' by `calc_dens_obs()`. If both are provided, observed and predicted data
#' are row-bound into a single `data.frame`.
#'
#' These functions are used internally to support `type = "density"` in
#' [dRiftDM::calc_stats()], providing a full distributional overview.
#'
#' @inheritParams calc_quantiles
#' @param discr a single numeric value defining the bin width for histogram and
#'   KDE estimation. Defaults to 0.015 (seconds).
#' @param t_max a single numeric value specifying the maximum RT to consider.
#'   Defaults to the smallest multiple of `discr` above the maximum RT. If
#'   `t_vec` is provided, `t_max` defaults to the maximum value of `t_vec`.
#' @param scaling_factor a single numeric value, multiplied with the PDFs.
#'   It is used to scale the corresponding probability mass
#'   proportional to the number of trials per condition. Defaults to `1.0`.
#'
#' @returns
#' A `stats_dm` object (via [dRiftDM::new_stats_dm()]) containing a
#' `data.frame` with columns:
#' - `Source`: indicates whether the row is from observed (`"obs"`) or
#'   predicted (`"pred"`) data.
#' - `Cond`: the condition label.
#' - `Time`: the time point corresponding to the density value.
#' - `Stat`: type of density summary—`"hist"` or `"kde"` (for observed data),
#'   or `"pdf"` (for predicted data).
#' - `Dens_<U>`: density value for the upper response.
#' - `Dens_<L>`: density value for the lower response.
#'
#' The `<U>` and `<L>` placeholders are determined by the `b_coding` argument.
#'
#' @keywords internal
calc_dens_obs <- function(
  rts_u,
  rts_l,
  one_cond,
  t_max = NULL,
  discr = NULL,
  scaling_factor = 1.0
) {
  # ensure that t_max and discr arguments are reasonable
  max_rt <- max(rts_u, rts_l)
  if (is.null(discr)) {
    discr <- 0.015
  }
  if (is.null(t_max)) {
    t_max <- max_rt
  }
  stopifnot(discr < t_max)

  # Align t_max with discr steps
  t_max <- ceiling(t_max / discr) * discr
  stopifnot(max_rt < t_max)

  # define the breaks (taking into account the maximum RT value)
  n <- round(t_max / discr + 1)
  breaks <- seq(from = 0, to = t_max, length.out = n)
  mids <- breaks[-length(breaks)] + diff(breaks) / 2

  # call hist() twice and extract the values
  dens_hist_u <- graphics::hist(rts_u, breaks = breaks, plot = FALSE)$density
  dens_hist_l <- graphics::hist(rts_l, breaks = breaks, plot = FALSE)$density

  # call density() twice and extract the values
  # the wrapper ensures that the function doesn't crash if one of the
  # rt vectors has less than two values
  kde_wrapper <- function(vals) {
    if (length(vals) <= 1) {
      return(rep(NaN, length(mids)))
    }
    d <- stats::density(vals, from = 0, to = t_max)
    d <- stats::approx(d$x, d$y, xout = mids, rule = 2)$y
    d <- d / sum(d * discr) # ensure it sums to 1
    return(d)
  }
  dens_kde_u <- kde_wrapper(rts_u)
  dens_kde_l <- kde_wrapper(rts_l)

  # calculate the relative contribution
  ratio <- length(rts_u) / (length(rts_u) + length(rts_l))

  # pack up as a data.frame, scale and return
  dens_stats <- data.frame(
    Cond = one_cond,
    Stat = rep(c("hist", "kde"), each = length(mids)),
    Time = rep(mids, 2),
    Dens_U = c(dens_hist_u, dens_kde_u) * ratio * scaling_factor,
    Dens_L = c(dens_hist_l, dens_kde_l) * (1 - ratio) * scaling_factor
  )

  return(dens_stats)
}


#' @rdname calc_dens_obs
calc_dens <- function(
  pdf_u = NULL,
  pdf_l = NULL,
  t_vec = NULL,
  t_max = NULL,
  discr = NULL,
  rts_u = NULL,
  rts_l = NULL,
  one_cond,
  b_coding,
  scaling_factor = 1.0
) {
  # parameter extraction
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

  # wrap up the pdfs when they are supplied ...
  result_pred <- NULL
  if (!is.null(pdf_u)) {
    stopifnot(length(pdf_u) == length(pdf_l))
    stopifnot(length(pdf_u) == length(t_vec))

    result_pred <- data.frame(
      Source = "pred",
      Cond = one_cond,
      Stat = "pdf",
      Time = t_vec,
      Dens_U = pdf_u * scaling_factor,
      Dens_L = pdf_l * scaling_factor
    )
  }

  # if rts are supplied, calculate the density statistics
  result_obs <- NULL
  if (!is.null(rts_u)) {
    # if there is no specified t_max value, check for temp options
    if (is.null(t_max)) {
      t_max <- stats.options("t_max") # returns NULL or some value
    }

    # if there is a time vector, extract t_max and use it (overrides any input
    # or options)
    if (!is.null(t_vec)) {
      t_max <- max(t_vec)
    }

    result_obs <- calc_dens_obs(
      rts_u = rts_u,
      rts_l = rts_l,
      one_cond = one_cond,
      t_max = t_max,
      discr = discr,
      scaling_factor = scaling_factor
    )
    result_obs <- cbind(Source = "obs", result_obs)
  }

  # maybe combine
  result <- rbind(result_obs, result_pred)

  # rename
  if (!is.null(result)) {
    colnames(result)[which(names(result) == "Dens_U")] <-
      paste("Dens", u_name, sep = "_")
    colnames(result)[which(names(result) == "Dens_L")] <-
      paste("Dens", l_name, sep = "_")
  }

  # make it dm_* class and pass back
  stats_dm_obj <- new_stats_dm(
    stat_df = result,
    type = "densities",
    b_coding = b_coding
  )
  return(stats_dm_obj)
}


# FUNCTIONS FOR BASIC SUMMARY STATISTICS ------------------------------------

#' Calculate Basic Mean, Standard Deviations, and Percentages for Observed and
#' Predicted Data
#'
#' Backend functions to compute the mean response times, standard deviations of
#' response times, and response proportions; for both observed RTs or the
#' predicted probability density functions.
#'
#' @param rts_u,rts_l numeric, vectors of response times for the upper and lower
#'  boundaries.
#' @param pdf_u,pdf_l numeric, vectors of probability density values for the
#'  upper and lower boundaries.
#' @param one_cond character, a label for the condition.
#' @param t_vec numeric vector, containing the time points corresponding to the
#'  probability density values.
#' @param dt a single numeric, providing the step size in `t_vec`.
#' @param skip_if_contr_low a single numeric, threshold below which probability
#' densities are ignored (default is obtained from
#' [dRiftDM::drift_dm_skip_if_contr_low()]).
#'
#' @returns A [data.frame] with columns:
#'   - `Cond`: Condition label.
#'   - `Mean_U`: Mean response time for the upper boundary
#'   - `Mean_L`: Mean response time for the lower boundary
#'   - `SD_U`: Mean response time for the upper boundary
#'   - `SD_L`: Mean response time for the lower boundary
#'   - `P_U`: Proportion of upper-boundary responses.
#'
#' @details
#' - For observed data, calculates mean RTs, standard deviations of RTs, and
#'   the proportion of upper responses.
#' - The same statistics are calculated for the probability density values (via
#'   simple numerical integration)
#'
#' @keywords internal
calc_basic_stats_obs <- function(rts_u, rts_l, one_cond) {
  # calculate the means
  mean_u <- mean(rts_u)
  mean_l <- mean(rts_l)

  # calculate the sds
  sd_u <- stats::sd(rts_u)
  sd_l <- stats::sd(rts_l)

  # calculate the percentages (e.g., error rates)
  ratio <- length(rts_u) / (length(rts_u) + length(rts_l))

  # pack up as a data.frame and return
  basic_stats <- data.frame(
    Cond = one_cond,
    Mean_U = mean_u,
    Mean_L = mean_l,
    SD_U = sd_u,
    SD_L = sd_l,
    P_U = ratio
  )
  return(basic_stats)
}


#' @rdname calc_basic_stats_obs
calc_basic_stats_pred <- function(
  pdf_u,
  pdf_l,
  one_cond,
  t_vec,
  dt,
  skip_if_contr_low = NULL
) {
  stopifnot(length(pdf_u) == length(pdf_l))
  stopifnot(length(pdf_u) == length(t_vec))
  stopifnot(!is.null(dt))

  if (is.null(skip_if_contr_low)) {
    skip_if_contr_low <- drift_dm_skip_if_contr_low()
  }

  n_steps <- length(pdf_u)

  # first, integrate to cdfs
  cdf_u <- cumtrapz(x = t_vec, y = pdf_u)
  cdf_l <- cumtrapz(x = t_vec, y = pdf_l)

  # to calculate the ratio of the probability mass
  sum_pdf_u <- cdf_u[n_steps]
  sum_pdf_l <- cdf_l[n_steps]
  ratio <- sum_pdf_u / (sum_pdf_l + sum_pdf_u)

  # determine if the contribution is relevant
  if (sum_pdf_u < skip_if_contr_low) {
    pdf_u <- rep(NA_real_, length(pdf_u))
  }
  if (sum_pdf_l < skip_if_contr_low) {
    pdf_l <- rep(NA_real_, length(pdf_l))
  }

  # then scale each pdf
  pdf_u <- pdf_u / sum_pdf_u
  pdf_l <- pdf_l / sum_pdf_l

  # then calculate the mean
  tmp_u <- cumtrapz(x = t_vec, y = t_vec * pdf_u)
  mean_u <- tmp_u[n_steps]
  tmp_l <- cumtrapz(x = t_vec, y = t_vec * pdf_l)
  mean_l <- tmp_l[n_steps]

  # and the standard deviation (V(X) = E(X^2) - E(X)^2)
  tmp_u <- cumtrapz(x = t_vec, y = t_vec^2 * pdf_u)
  sd_u <- sqrt(tmp_u[n_steps] - mean_u^2)
  tmp_l <- cumtrapz(x = t_vec, y = t_vec^2 * pdf_l)
  sd_l <- sqrt(tmp_l[n_steps] - mean_l^2)

  # pass back
  basic_stats <- data.frame(
    Cond = one_cond,
    Mean_U = mean_u,
    Mean_L = mean_l,
    SD_U = sd_u,
    SD_L = sd_l,
    P_U = ratio
  )
  return(basic_stats)
}


#' Calculate Basic Statistics for Response Times or Probability Densities
#'
#' Function that calls the underlying functions
#' [dRiftDM::calc_basic_stats_obs] and [dRiftDM::calc_basic_stats_pred].
#' Handles input checks and data wrangling.
#'
#' @inheritParams calc_basic_stats_obs
#' @param b_coding list, used for accessing the upper boundary label, determines
#' the corresponding column of the returned data.frame (e.g., P_`corr`).
#'
#' @return A [data.frame] with columns:
#'   - `Source`: Indicates whether the statistics refer to observed (`obs`) or
#'    predicted (`pred`) data.
#'   - `Cond`: A condition label.
#'   - `Mean_<u_label>`: Mean response time for the upper boundary.
#'   - `Mean_<l_label>`: Mean response time for the lower boundary.
#'   - `P_<u_label>`: Proportion of upper-boundary responses.
#'
#'
#' @details
#' - If `pdf_u` and `pdf_l` are provided, the function computes statistics for
#'   the probability densities.
#' - If `rts_u` and `rts_l` are provided, the function computes statistics for
#'   the observed RTs.
#' - If both sets of inputs are provided, both types of statistics are computed
#'   and returned.
#'
#' @seealso [dRiftDM::calc_basic_stats_obs], [dRiftDM::calc_basic_stats_pred],
#' [dRiftDM::new_stats_dm]
#'
#' @keywords internal
calc_basic_stats <- function(
  pdf_u = NULL,
  pdf_l = NULL,
  rts_u = NULL,
  rts_l = NULL,
  one_cond,
  b_coding,
  t_vec = NULL,
  dt = NULL,
  skip_if_contr_low = NULL
) {
  # boundary settings and checks
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
    result_pred <- calc_basic_stats_pred(
      pdf_u = pdf_u,
      pdf_l = pdf_l,
      one_cond = one_cond,
      t_vec = t_vec,
      dt = dt,
      skip_if_contr_low = skip_if_contr_low
    )
    result_pred <- cbind(Source = "pred", result_pred)
  }

  result_obs <- NULL
  if (!is.null(rts_u)) {
    result_obs <- calc_basic_stats_obs(
      rts_u = rts_u,
      rts_l = rts_l,
      one_cond = one_cond
    )
    result_obs <- cbind(Source = "obs", result_obs)
  }

  # combine and format names
  result <- rbind(result_obs, result_pred)

  if (!is.null(result)) {
    new_names_u <- gsub(
      pattern = "_U$",
      replacement = paste0("_", u_name),
      x = grep("_U$", names(result), value = T)
    )
    colnames(result)[grepl(pattern = "_U$", x = names(result))] <-
      new_names_u

    new_names_l <- gsub(
      pattern = "_L$",
      replacement = paste0("_", l_name),
      x = grep("_L$", names(result), value = T)
    )
    colnames(result)[grepl(pattern = "_L$", x = names(result))] <-
      new_names_l
  }

  # make it a stats_dm class and pass back
  stats_dm_obj <- new_stats_dm(
    stat_df = result,
    type = "basic_stats",
    b_coding = b_coding
  )
  return(stats_dm_obj)
}


# FUNCTIONS FOR CALCULATING THE CAF ---------------------------------------

#' Calculate CAFs
#'
#' Backend functions to calculate conditional accuracy functions for RT
#' vectors or pdfs
#'
#' @param rts_u,rts_l vectors of RTs for the upper and lower boundary
#' @param pdf_u,pdf_l density values for the upper and lower boundary
#' @param t_vec the time space
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
  tryCatch(
    {
      bins <- cut(rts, breaks = borders, labels = FALSE, include.lowest = TRUE)
      stopifnot(identical(sort(unique(bins)), 1:n_bins))
    },
    error = function(e) {
      stop(
        "Couldn't form ",
        n_bins,
        " bins from ",
        length(rts),
        " RTs ",
        "(condition: ",
        one_cond,
        ")"
      ) # AIC, BIC -> muffle this warning
    }
  )

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
calc_cafs_pred <- function(pdf_u, pdf_l, t_vec, one_cond, n_bins) {
  stopifnot(length(pdf_u) == length(pdf_l))
  stopifnot(length(pdf_u) == length(t_vec))

  # make a cdf
  cdf <- cumtrapz(x = t_vec, y = pdf_u + pdf_l)
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
  stopifnot(identical(unique(bins), 1:n_bins))

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
#' @inheritParams calc_cafs_obs
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
calc_cafs <- function(
  pdf_u = NULL,
  pdf_l = NULL,
  t_vec = NULL,
  rts_u = NULL,
  rts_l = NULL,
  one_cond,
  n_bins = NULL,
  b_coding
) {
  # default settings and parameter extraction
  if (is.null(n_bins)) {
    n_bins <- drift_dm_default_n_bins()
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
      pdf_u = pdf_u,
      pdf_l = pdf_l,
      t_vec = t_vec,
      one_cond = one_cond,
      n_bins = n_bins
    )
    result_pred <- cbind(Source = "pred", result_pred)
    colnames(result_pred)[which(names(result_pred) == "P_U")] <-
      paste("P", u_name, sep = "_")
  }

  result_obs <- NULL
  if (!is.null(rts_u)) {
    result_obs <- calc_cafs_obs(
      rts_u = rts_u,
      rts_l = rts_l,
      one_cond = one_cond,
      n_bins = n_bins
    )
    result_obs <- cbind(Source = "obs", result_obs)
    colnames(result_obs)[which(names(result_obs) == "P_U")] <-
      paste("P", u_name, sep = "_")
  }

  # combine
  result <- rbind(result_obs, result_pred)

  # make it stats_dm class and pass back
  stats_dm_obj <- new_stats_dm(
    stat_df = result,
    type = "cafs",
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
#' @param dt the step size corresponding to the time space
#'
#' @param skip_if_contr_low numeric. If the contribution of the upper
#' or lower PDF to the overall PDF is too low, return NAs for
#' this PDF (see also [dRiftDM::drift_dm_skip_if_contr_low()]).
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
calc_quantiles_pred <- function(
  pdf_u,
  pdf_l,
  t_vec,
  one_cond,
  probs,
  dt,
  skip_if_contr_low = NULL
) {
  stopifnot(length(pdf_u) == length(pdf_l))
  stopifnot(length(pdf_u) == length(t_vec))
  stopifnot(!is.null(dt))

  if (is.null(skip_if_contr_low)) {
    skip_if_contr_low <- drift_dm_skip_if_contr_low()
  }

  quants <-
    apply(
      cbind(pdf_u, pdf_l),
      MARGIN = 2,
      function(a_pdf, t_vec, probs) {
        cdf <- cumtrapz(x = t_vec, y = a_pdf)
        cdf <- cdf / max(cdf)
        return(stats::approx(x = cdf, y = t_vec, xout = probs, ties = "mean")$y)
      },
      t_vec = t_vec,
      probs = probs
    )

  colnames(quants) <- c("Quant_U", "Quant_L")

  quants <- as.data.frame(quants)
  quants <- cbind(Cond = one_cond, Prob = probs, quants)

  sum_pdf_l <- sum(pdf_l)
  sum_pdf_u <- sum(pdf_u)

  if (sum_pdf_u * dt < skip_if_contr_low) {
    quants["Quant_U"] <- NA_real_
  }
  if (sum_pdf_l * dt < skip_if_contr_low) {
    quants["Quant_L"] <- NA_real_
  }

  return(quants)
}


#' Calculate Quantiles
#'
#' Function that calls the underlying quantile calculation functions
#' [dRiftDM::calc_quantiles_obs()] and [dRiftDM::calc_quantiles_pred()]. Does
#' input checks and the data wrangling
#'
#' @inheritParams calc_quantiles_obs
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
calc_quantiles <- function(
  pdf_u = NULL,
  pdf_l = NULL,
  t_vec = NULL,
  dt = NULL,
  rts_u = NULL,
  rts_l = NULL,
  one_cond,
  probs = NULL,
  b_coding,
  skip_if_contr_low = NULL
) {
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
      pdf_u = pdf_u,
      pdf_l = pdf_l,
      t_vec = t_vec,
      dt = dt,
      one_cond = one_cond,
      probs = probs,
      skip_if_contr_low = skip_if_contr_low
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
      rts_u = rts_u,
      rts_l = rts_l,
      one_cond = one_cond,
      probs = probs
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
    stat_df = result,
    type = "quantiles",
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
calc_delta_funs <- function(
  quantiles_dat,
  minuends = NULL,
  subtrahends = NULL,
  dvs = NULL,
  b_coding
) {
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
      "\n\tprovided: ",
      paste(colnames(quantiles_dat), collapse = " "),
      "\n\tnecessary: ",
      paste(nec_columns, collapse = " ")
    )
  }

  # input checks on minuends/subtrahends
  if (is.null(minuends)) {
    stop("calc_delta_funs was called but the argument minuends not provided")
  }
  if (is.null(subtrahends)) {
    stop("calc_delta_funs was called but the argument subtrahends not provided")
  }

  conds <- unique(quantiles_dat$Cond)

  n_m <- length(minuends)
  tmp_m <- minuends
  minuends <- match.arg(minuends, conds, several.ok = TRUE)
  if (n_m > length(minuends)) {
    warning(
      "dropping unmatched conditions in minuends: ",
      paste(setdiff(tmp_m, minuends), collapse = ", ")
    )
  }

  n_s <- length(subtrahends)
  tmp_s <- subtrahends
  subtrahends <- match.arg(subtrahends, conds, several.ok = TRUE)
  if (n_s > length(subtrahends)) {
    warning(
      "dropping unmatched conditions in subtrahends: ",
      paste(setdiff(tmp_s, subtrahends), collapse = ", ")
    )
  }
  if (length(subtrahends) != length(minuends)) {
    stop("different length of minuends and subtrahends")
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
  n_cond <- length(conds)
  if (nrow(quantiles_dat) != n_probs * n_source * n_cond) {
    stop(
      "quantiles_dat doesn't uniquely code rows solely by Probs, Source, ",
      "and Cond"
    )
  }
  quantiles_dat <- stats::reshape(
    quantiles_dat,
    idvar = c("Source", "Prob"),
    timevar = "Cond",
    direction = "wide",
    sep = "_"
  )

  # calculate delta functions
  if (length(dvs) == 1) {
    delta_names <- paste(
      "Delta",
      paste(minuends, subtrahends, sep = "_"),
      sep = "_"
    )
    avg_names <- paste(
      "Avg",
      paste(minuends, subtrahends, sep = "_"),
      sep = "_"
    )
  } else {
    delta_names <- paste("Delta", gsub("^Quant_", "", dvs), sep = "_")
    avg_names <- paste("Avg", gsub("^Quant_", "", dvs), sep = "_")
    delta_names <- paste(
      delta_names,
      paste(minuends, subtrahends, sep = "_"),
      sep = "_"
    )
    avg_names <- paste(
      avg_names,
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
    quantiles_dat[[avg_names[i]]] <- 0.5 *
      vals_minuends +
      0.5 * vals_subtrahends
  }

  # make it stats_dm class and pass back
  # since delta_funs depends on quantiles, this will remove the quantiles class
  stats_dm_obj <- new_stats_dm(
    stat_df = quantiles_dat,
    type = "delta_funs",
    b_coding = b_coding
  )
  return(stats_dm_obj)
}


# FUNCTIONS FOR FIT STATISTICS --------------------------------------------

#' Calculate Fit Statistics
#'
#' Computes/Summarizes multiple fit statistics, inclduing Log-Likelihood,
#' the Negative Log-Likelihood, the Akaike Information Criterion (AIC), the
#' Bayesian Information Criterion (BIC), and the Root-Mean Squared-Error (RMSE)
#' statistic.
#'
#' @param drift_dm_obj an object of type [dRiftDM::drift_dm]
#' @param k a single numeric, scaling the penality of [stats::AIC])
#' @param ... additional arguments passed forward. Options are `probs`, `n_bins`,
#' and `weight_err` for calculating the RMSE.
#'
#' @return A custom object of class `stats_dm`
#' (c("fit_stats", "stats_dm", "data.frame")). The columns are:
#' * `Log_Like`: the log-likelihood value
#' * `Neg_Log_Like`: the negative log-likelihood value
#' * `AIC`: the calculated AIC value
#' * `BIC`: the calculated BIC value
#' * `RMSE_s`: the root-mean-squared error (for RTs in seconds)
#' * `RMSE_ms`: the root-mean-squared error (for RTs in milliseconds)
#' If a respective statistic cannot be calculated, the respective column
#' contains `NA`.
#'
#' @seealso [dRiftDM::new_stats_dm()], [dRiftDM::logLik.drift_dm]
#'
#' @keywords internal
calc_fit_stats <- function(drift_dm_obj, k = 2, ...) {
  dots <- list(...)

  # extract necessary information
  df <- get_number_prms(drift_dm_obj$flex_prms_obj)
  n <- nobs(drift_dm_obj)

  # get the pdfs and the time domain
  pdfs_t_vec <- pdfs(drift_dm_obj)

  # calculate the log_likelihood, aic and bic
  log_like <- calc_log_like(
    pdfs_t_vec$pdfs,
    pdfs_t_vec$t_vec,
    obs_data = drift_dm_obj$obs_data
  )
  if (is.null(log_like)) {
    log_like <- NA_real_
  }
  aic <- -2 * log_like + k * df
  bic <- -2 * log_like + df * log(n)
  neg_log_like <- -1.0 * log_like

  # calculate rmse (try to calculate the aggregated statistics)
  if (is.null(drift_dm_obj$stats_agg)) {
    tryCatch(
      {
        drift_dm_obj <- update_stats_agg(
          drift_dm_obj,
          which_cost_function = "rmse",
          probs = dots$probs,
          n_bins = dots$n_bins
        )
      },
      error = function(e) {
        warning(
          "Calculating aggregated stats failed: ",
          conditionMessage(e)
        )
        return(NULL)
      }
    )
  } # -> stats_agg is still NULL if this fails -> rmse will be null
  weight_err <- dots$weight_err %||% 1.5
  rmse <- calc_rmse_eval(
    pdfs_t_vec$pdfs,
    t_vec = pdfs_t_vec$t_vec,
    dt = prms_solve(drift_dm_obj)["dt"],
    stats_agg = drift_dm_obj$stats_agg,
    stats_agg_info = drift_dm_obj$stats_agg_info,
    weight_err = weight_err
  )
  if (is.null(rmse) || is.infinite(rmse)) {
    rmse <- NA_real_
  }
  rmse_ms <- rmse * 1000

  # bind everything
  result <- data.frame(
    Log_Like = log_like,
    Neg_Log_Like = neg_log_like,
    AIC = aic,
    BIC = bic,
    RMSE_s = rmse,
    RMSE_ms = rmse_ms
  )

  # make it a stats_dm object and return
  stats_obj <- new_stats_dm(stat_df = result, type = "fit_stats")
  return(stats_obj)
}


# ACCESS FUNCTION FOR EACH STATISTIC --------------------------------------

#' Calculate Statistics for Model Prediction and/or Observed Data
#'
#' This function derives statistics that can be calculated for model
#' predictions and/or observed data. However, it does not calculate it, but
#' rather calls the respective backend functions.
#' Supported statistics currently include:
#' - Basic Summary Statistics (i.e., means and response percentages
#'   [dRiftDM::calc_basic_stats()])
#' - Conditional Accuracy Functions (CAFs; [dRiftDM::calc_cafs()])
#' - Quantiles ([dRiftDM::calc_quantiles()])
#' - Delta Functions ([dRiftDM::calc_delta_funs()]).
#' - Density Estimates ([dRiftDM::calc_dens()]).
#'
#' @param type character string, specifying the type of statistic to calculate.
#'   Available options are `"basic_stats"`, `"cafs"`, `"quantiles"`,
#'   `"delta_funs"`, and `"densities"`.
#' @param b_coding list for the boundary coding (see [dRiftDM::b_coding]).
#' @param conds character vector, specifying the conditions to include in
#' calculations (used for labeling and subsetting the model PDFs and the
#' observed data).
#' @param ... Additional parameters passed on to the specific statistic
#' calculation function (see Details).
#' @param scale_mass a single logical, only relevant for density estimation.
#' If `TRUE`, PDF masses are scaled proportional to the number of trials per
#' condition.
#'
#' @details
#'
#' When calling this function the arguments `all_rts_u`/`all_rts_l` and/or
#' `all_pdfs` must always be specified (see
#' [dRiftDM::re_evaluate_model], [dRiftDM::obs_data]). Otherwise, the backend
#' functions won't work properly. Further arguments are:
#'
#' - for CAFS: `n_bins` controls the number of bins, with a default of 5.
#' - for Quantiles and Delta Functions: `probs` controls the quantiles to
#' calculate. Default is `seq(0.1, 0.9, 0.1)`
#' (see [dRiftDM::drift_dm_default_probs()]).
#' - for basic summary satistics, Quantiles, and Delta Function:
#' `skip_if_contr_low` controls if quantiles and means are calculated for PDFs
#' with very small contribution (see also
#' [dRiftDM::drift_dm_skip_if_contr_low()]).
#' - for densities: `discr` controls the bin width for the observed data.
#' Default is 0.015 seconds
#'
#' This function gets called by [dRiftDM::calc_stats()]
#'
#'
#' @return A data frame with the calculated statistic across `conds`
#'  (ordered according to `Source`).
#'
#' @keywords internal
calc_stats_pred_obs <- function(
  type,
  b_coding,
  conds,
  ...,
  scale_mass = FALSE
) {
  dotdot <- list(...)

  if (!is.character(type) | length(type) != 1) {
    stop("type must be a single character string of length 1")
  }
  if (!is.logical(scale_mass) | length(scale_mass) != 1) {
    stop("scale_mass must be a single logical value of length 1")
  }

  # only these types are available for both observed and predicted data
  type <- match.arg(type, drift_dm_stats_types("sum_dist"))

  # iterate through the requested types and calculate the stats
  if (type == "basic_stats") {
    result <-
      lapply(conds, function(one_cond) {
        pdf_u <- dotdot$all_pdfs[[one_cond]]$pdf_u
        pdf_l <- dotdot$all_pdfs[[one_cond]]$pdf_l
        rts_u <- dotdot$all_rts_u[[one_cond]]
        rts_l <- dotdot$all_rts_l[[one_cond]]

        calc_basic_stats(
          pdf_u = pdf_u,
          pdf_l = pdf_l,
          rts_u = rts_u,
          rts_l = rts_l,
          one_cond = one_cond,
          b_coding = b_coding,
          t_vec = dotdot$t_vec,
          dt = dotdot$dt,
          skip_if_contr_low = dotdot$skip_if_contr_low
        )
      })
    result <- do.call("rbind", result)
  }
  if (type == "quantiles") {
    result <-
      lapply(conds, function(one_cond) {
        pdf_u <- dotdot$all_pdfs[[one_cond]]$pdf_u
        pdf_l <- dotdot$all_pdfs[[one_cond]]$pdf_l
        rts_u <- dotdot$all_rts_u[[one_cond]]
        rts_l <- dotdot$all_rts_l[[one_cond]]

        calc_quantiles(
          pdf_u = pdf_u,
          pdf_l = pdf_l,
          t_vec = dotdot$t_vec,
          dt = dotdot$dt,
          rts_u = rts_u,
          rts_l = rts_l,
          one_cond = one_cond,
          probs = dotdot$probs,
          b_coding = b_coding,
          skip_if_contr_low = dotdot$skip_if_contr_low
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
          pdf_u = pdf_u,
          pdf_l = pdf_l,
          t_vec = dotdot$t_vec,
          rts_u = rts_u,
          rts_l = rts_l,
          one_cond = one_cond,
          n_bins = dotdot$n_bins,
          b_coding = b_coding
        )
      })
    result <- do.call("rbind", result)
  }
  if (type == "delta_funs") {
    interim <- calc_stats_pred_obs(
      type = "quantiles",
      b_coding = b_coding,
      conds = conds,
      ...
    )
    result <- calc_delta_funs(
      quantiles_dat = interim,
      minuends = dotdot$minuends,
      subtrahends = dotdot$subtrahends,
      dvs = dotdot$dvs,
      b_coding = b_coding
    )
  }
  if (type == "densities") {
    # determine the scaling factors
    scaling_factors <- stats::setNames(rep(1.0, length(conds)), conds)
    if (scale_mass) {
      n_per_cond <- sapply(conds, \(one_cond) {
        rts_u <- dotdot$all_rts_u[[one_cond]]
        rts_l <- dotdot$all_rts_l[[one_cond]]
        return(length(rts_u) + length(rts_l))
      })
      # if there are no trials, fall back to scaling of 1.0
      if (any(n_per_cond == 0)) {
        scaling_factors <- stats::setNames(rep(1.0, length(conds)), conds)
      } else {
        scaling_factors <- n_per_cond / (sum(n_per_cond) / length(n_per_cond))
      }
    }

    # then iterate again over all conditions
    result <-
      lapply(conds, function(one_cond) {
        pdf_u <- dotdot$all_pdfs[[one_cond]]$pdf_u
        pdf_l <- dotdot$all_pdfs[[one_cond]]$pdf_l
        rts_u <- dotdot$all_rts_u[[one_cond]]
        rts_l <- dotdot$all_rts_l[[one_cond]]
        scaling_factor <- scaling_factors[[one_cond]]

        calc_dens(
          pdf_u = pdf_u,
          pdf_l = pdf_l,
          t_vec = dotdot$t_vec,
          t_max = dotdot$t_max,
          discr = dotdot$discr,
          rts_u = rts_u,
          rts_l = rts_l,
          one_cond = one_cond,
          b_coding = b_coding,
          scaling_factor = scaling_factor
        )
      })
    result <- do.call("rbind", result)
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
#' basic statistics on mean and standard deviation, Conditional Accuracy
#' Functions (CAFs), Quantiles, Delta Functions, and fit statistics. Results can
#' be aggregated across individuals.
#'
#' @param object an object for which statistics are calculated. This can be a
#' [data.frame] of observed data, a [dRiftDM::drift_dm] object, a
#' `fits_ids_dm` object, or a `fits_agg_dm` object (see
#'  [dRiftDM::estimate_dm()]).
#' @param type a character vector, specifying the statistics to calculate.
#' Supported values include `"basic_stats"`, `"cafs"`, `"quantiles"`,
#' `"delta_funs"`, `"densities"`, and `"fit_stats"`.
#' @param ... additional arguments passed to the respective method and the
#' underlying calculation functions (see Details for mandatory arguments).
#' @param conds optional character vector specifying conditions to include.
#' Conditions must match those found in the `object`.
#' @param resample logical. If `TRUE`, then data is (re-)sampled to create
#' an uncertainty estimate for the requested summary statistic. See Details for
#' more information. Default is `FALSE`. Note that resampling does not work with
#' `type = "fit_stats"`.
#' @param progress integer, indicating if information about the progress
#'  should be displayed. 0 -> no information, 1 -> a progress bar. Default is 1.
#' @param level a single character string, indicating at which "level" the
#' statistic should be calculated. Options are `"group"` or `"individual"`. If
#' `"individual"`, the returned `stats_dm` object contains an `"ID"` column.
#' @param b_coding a list for boundary coding (see [dRiftDM::b_coding]). Only
#' relevant when `object` is a [data.frame]. For other `object` types, the
#' `b_coding` of the `object` is used.
#' @param round_digits integer, controls the number of digits shown.
#'  Default is 3.
#' @param messaging logical, if `FALSE`, no message is provided.
#' @param print_rows integer, controls the number of rows shown.
#' @param some logical. If `TRUE`, a subset of randomly sampled rows is shown.
#' @param show_header logical. If `TRUE`, a header specifying the type of
#'  statistic will be displayed.
#' @param show_note logical. If `TRUE`, a footnote  is displayed indicating
#' that the underlying [data.frame] can be accessed as usual.
#' @param x an object of type `stats_dm` or `stats_dm_list`, as returned by
#' the function `calc_stats()`.
#'
#' @details
#' `calc_stats` is a generic function to handle the calculation of different
#' statistics/metrics for the supported object types. Per default, it returns
#' the requested statistics/metrics.
#'
#' ## List of Supported Statistics
#'
#' **Basic Statistics**
#'
#' With "basic statistics", we refer to a summary of the mean and standard
#' deviation of response times, including a proportion of response choices.
#'
#'
#' **Conditional Accuracy Function (CAFs)**
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
#' **Quantiles**
#'
#'  For observed response times, the function [stats::quantile] is used with
#'  default settings.
#'
#'  Which quantiles are calcuated can be controlled by providing the
#'  probabilites, `probs`, with values in \eqn{[0, 1]}. Default is
#'  `seq(0.1, 0.9, 0.1)`.
#'
#' **Delta Functions**
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
#'  - specifying `probs` is possible (see Quantiles)
#'
#' **Densities**
#'
#' With "densities", we refer to a summary of the distribution of observed
#' or predicted data. For observed data, histogram values and kernel density
#' estimates are provided. For predicted data, the model's predicted PDFs are
#' provided.
#'
#'  Optional arguments are:
#'  - `discr`: numeric, the band-width when calculating the histogram or the
#'  kernel density estimates. Defaults to `0.015` seconds
#'  - `t_max`: numeric, the maximum time window when calculating the distribution
#'  summaries of observe data. Defaults to the longest RT (for observed data)
#'  or the maximum of the time domain of a model (which is the preferred choice,
#'  if possible). If necessary, `t_max` is slightly adjusted to match with
#'  `discr`.
#'  - `scale_mass`: logical, only relevant if observed data is available. If
#'  `TRUE`, density masses are scaled proportional to the number of trials per
#'  condition.
#'
#' **Fit Statistics**
#'
#' Calculates the Log-Likelihood, Akaike and Bayesian Information Criteria,
#' and root-mean squared-error statistic.
#'
#' Optional arguments are:
#' - `k`: numeric, for penalizing the AIC statistic (see also [stats::AIC]
#'   and [dRiftDM::AIC.fits_ids_dm]).
#' - `n_bins`, `probs`: numeric vectors, see the section on CAFs and Quantiles
#'   above
#' - `weight_err`: numeric scalar, determines how CAFs and quantiles are
#'   weighted. Default is `1.5`.
#'
#'
#' ## Resampling
#'
#' When `resampling = TRUE`, an uncertainty interval is provided via simulation.
#' The default number of iterations is `R = 100`, which can be changed by
#' passing the optional argument `R`.
#'
#' If resampling is requested, the returned `stats_dm` object contains the
#' column `"Estimate"`, coding the interval. The interval width is controlled
#' via the optional argument `interval_level`, a single numeric value between
#' 0 and 1 (default: `0.95`). The interpretation of this interval depends on
#' the specific situation (see below).
#'
#' **Resampling at the Individual Level**
#'
#' If `object` is a `drift_dm` object (i.e., a single model created by
#' [dRiftDM::drift_dm()]), synthetic data are simulated under the model, and
#' for each synthetic data set the requested statistic is calculated. The
#' interval then reflects the range of these simulated statistics. To determine
#' the number of trials for each synthetic data set, dRiftDM either uses the
#' observed data attached to the model (if available) or the optional argument
#' `n_sim` (passed to [dRiftDM::simulate_data()]). Note that `n_sim` must be
#' provided if no observed data are available, and that `n_sim` always has
#' priority.
#'
#' If `object` is a `drift_dm` object with attached observed data, resampling
#' is also performed for the observed data. In this case, trials are
#' bootstrapped, and for each bootstrap sample the requested statistic is
#' calculated.
#'
#' If `object` is a `data.frame`, `fits_agg_dm`, or `fits_ids_dm` object,
#' resampling is performed for each individual if `level = "individual"`. For
#' both models and observed data, synthetic or bootstrapped data sets are
#' generated as described above.
#'
#' **Resampling at the Group Level**
#'
#' Group-level resampling is possible only if `object` is a `data.frame`
#' (with an `"ID"` column), `fits_agg_dm`, or `fits_ids_dm` object. To request
#' this, set `level = "group"`. Participants are then bootstrapped, and
#' for each bootstrapped sample the aggregated statistic is calculated.
#'
#' **Interpretation of Intervals**
#'
#' For `level = "group"`, intervals represent bootstrapped confidence intervals
#' For `level = "individual"`, intervals represent the variability in the
#' statistic when data for a single participant are resampled or simulated
#' under the model.
#'
#' **Note**
#'
#' For objects of type `fits_agg_dm`, which contain a mixture of group- and
#' individual-level information, the `level` argument only affects resampling
#' for the observed data. For the model itself, resampling is always performed
#' under the fitted model, in the same way as for a `drift_dm` object.
#'
#'
#' @returns
#' If `type` is a single character string, then a subclass of [data.frame] is
#' returned, containing the respective statistic. Objects of type `sum_dist`
#' will have an additional attribute storing the boundary encoding (see also
#' [dRiftDM::b_coding]). The reason for returning subclasses of [data.frame] is
#' to provide custom `plot()` methods (e.g., [dRiftDM::plot.cafs]). To get rid
#' of the subclass label and additional attributes (i.e., to get just the plain
#' underlying [data.frame], users can use [dRiftDM::unpack_obj()]).
#'
#' If `type` contains multiple character strings (i.e., is a character vector) a
#' subclass of [list] with the calculated statistics is returned. The list will
#' be of type `stats_dm_list` (to easily create multiple panels using the
#' respective [dRiftDM::plot.stats_dm_list()] method).
#'
#' The print methods `print.stats_dm()` and `print.stats_dm_list()` each
#' invisibly return the supplied object `x`.
#'
#' @note
#'
#' When a model's predicted density function integrates to a value of less than
#' [dRiftDM::drift_dm_skip_if_contr_low()], means and quantiles return the
#' values `NA`. Users can alter this by explicitly passing the argument
#' `skip_if_contr_low` when calling `calc_stats()` (e.g.,
#' `calc_stats(..., skip_if_contr_low = -Inf)`)
#'
#'
#' @examples
#' # Example 1: Calculate CAFs and Quantiles from a model ---------------------
#' # get a model for demonstration purpose
#' a_model <- ssp_dm()
#' # and then calculate cafs and quantiles
#' some_stats <- calc_stats(a_model, type = c("cafs", "quantiles"))
#' print(some_stats)
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
#' print(some_stats, print_rows = 5)
#'
#'
#' # Example 3: Calculate Quantiles from a fits_ids_dm object -----------------
#' # get an auxiliary fits_ids_dm object
#' all_fits <- get_example_fits("fits_ids_dm")
#' some_stats <- calc_stats(all_fits, type = "quantiles")
#' print(some_stats, print_rows = 5) # note the ID column
#'
#' # one can also request that the statistics are averaged across individuals
#' print(
#'   calc_stats(all_fits, type = "quantiles", average = TRUE)
#' )
#'
#' @export
calc_stats <- function(object, type, ...) {
  # to clean up any temporary options, exploited during the calculation of
  # the statistics
  withr::defer(stats.options(NULL))

  # now pass forward or loop
  if (length(type) > 1) {
    # re_evaluate if necessary (to avoid doing this multiple times)
    if (inherits(object, "drift_dm") && is.null(object$pdfs)) {
      object <- re_evaluate_model(object)
    }
    # then call the function recursively
    all_stats <- sapply(
      type,
      function(one_type) {
        calc_stats(object = object, type = one_type, ...)
      },
      simplify = FALSE,
      USE.NAMES = TRUE
    )

    class(all_stats) <- c("stats_dm_list")
    return(all_stats)
  }

  UseMethod("calc_stats")
}


#' @rdname calc_stats
#' @export
calc_stats.data.frame <- function(
  object,
  type,
  ...,
  conds = NULL,
  resample = FALSE,
  progress = 1,
  level = "individual",
  b_coding = NULL
) {
  obs_data <- object
  dots <- list(...)

  # input checks
  type <- match.arg(type, drift_dm_stats_types("sum_dist"))
  if (!is.logical(resample) || length(resample) != 1) {
    stop("resample must be a single logical")
  }
  if (!(progress %in% c(0, 1))) {
    stop("progress must be 0 or 1")
  }
  level <- match.arg(level, c("individual", "group"))

  # deprecation warning about split_by_ID and average
  if (!is.null(dots$split_by_ID)) {
    lifecycle::deprecate_warn(
      when = "0.3.0",
      what = "calc_stats.data.frame(split_by_ID = )",
      with = "calc_stats.data.frame(level =)"
    )
    if (dots$split_by_ID) {
      level <- "individual"
    } else {
      level <- "group"
    }
    dots$split_by_ID <- NULL
    return(
      do.call(
        calc_stats.data.frame,
        args = c(
          dots,
          list(
            object = object,
            type = type,
            conds = conds,
            resample = resample,
            progress = progress,
            level = level,
            b_coding = b_coding
          )
        )
      )
    )
  }

  if (!is.null(dots$average)) {
    lifecycle::deprecate_warn(
      when = "0.3.0",
      what = "calc_stats.data.frame(average = )",
      with = "calc_stats.data.frame(level =)"
    )
    if (dots$average) {
      level <- "group"
    } else {
      level <- "individual"
    }
    dots$average <- NULL
    return(
      do.call(
        calc_stats.data.frame,
        args = c(
          dots,
          list(
            object = object,
            type = type,
            conds = conds,
            resample = resample,
            progress = progress,
            level = level,
            b_coding = b_coding
          )
        )
      )
    )
  }

  # get b_coding and check the data
  if (is.null(b_coding)) {
    b_coding <- drift_dm_default_b_coding()
  }

  obs_data <- check_reduce_raw_data(
    obs_data = obs_data,
    b_coding_column = b_coding$column,
    u_value = b_coding$u_name_value,
    l_value = b_coding$l_name_value
  )

  # get the condition labels
  data_conds <- unique(obs_data$Cond)
  if (is.null(conds)) {
    conds <- data_conds
  } else {
    conds <- match.arg(arg = conds, choices = data_conds, several.ok = TRUE)
  }

  # temporarily set t_max as an option, if it was not specified,
  # this ensures a consistent size of outputs (like in density)
  stats.options(t_max = max(obs_data$RT))

  #  calculate for each ID (if possible)
  if (level == "individual" && ("ID" %in% colnames(obs_data))) {
    # temporarily set t_max as an option, if it was not specified,
    # this ensures a consistent size of outputs (like in density)
    stats.options(t_max = max(obs_data$RT))

    # split data up
    list_obs_data <- split(x = obs_data, f = obs_data$ID)

    # create a progress bar
    if (progress == 1) {
      n_iter <- length(list_obs_data)
      pb <- progress::progress_bar$new(
        format = "calculating [:bar] :percent; done in: :eta",
        total = n_iter,
        clear = FALSE,
        width = 60
      )
      pb$tick(0)
    }

    # iterate over the list of individual data
    all_results <- lapply(names(list_obs_data), function(id) {
      sub_dat <- list_obs_data[[id]]
      sub_dat <- sub_dat[setdiff(names(sub_dat), "ID")]
      stat <- calc_stats.data.frame(
        object = sub_dat,
        type = type,
        ...,
        conds = conds,
        resample = resample,
        progress = 0,
        level = "individual",
        b_coding = b_coding
      )
      stat_id <- cbind(ID = try_cast_integer(id), stat)
      stat_id <- copy_class_attributes(old = stat, new = stat_id)
      if (progress == 1) {
        pb$tick()
      }
      return(stat_id)
    })
    results <- do.call("rbind", all_results) # preserves class and attributes
    results <- results[order(results$ID), ]
    rownames(results) <- NULL
    return(results)
  }

  # if level == "group", try to return at the group-level
  # -> i.e., average across ID column or bootstrap entire sample
  if (level == "group") {
    if (!("ID" %in% colnames(obs_data))) {
      stop(
        "Statistics at the group level can only be calculated if the data ",
        "set contains an 'ID' column"
      )
    }

    # if resample is requested, call the underlying bootstrap function and return
    if (resample) {
      result <- stats_resample_dm(
        object = obs_data,
        type = type,
        conds = conds,
        ...,
        b_coding = b_coding,
        progress = progress,
        level = level
      )
      return(result)
    }

    # otherwise calculate the statistics for each individual and then average
    # across it
    result <- calc_stats.data.frame(
      object = obs_data,
      type = type,
      ...,
      conds = conds,
      resample = FALSE,
      progress = progress,
      level = "individual",
      b_coding = b_coding
    )
    return(aggregate_stats(result))
  }

  # finally, if individual and/or no ID column, assume obs_data is from one
  # individual
  if (resample) {
    result <- stats_resample_dm(
      object = obs_data,
      type = type,
      conds = conds,
      ...,
      b_coding = b_coding,
      progress = progress,
      level = level
    )
    return(result)
  }

  rts <- obs_data_to_rt_lists(obs_data = obs_data, b_coding = b_coding)

  # call the internal function
  result <- calc_stats_pred_obs(
    type = type,
    b_coding = b_coding,
    conds = conds,
    all_rts_u = rts$rts_u,
    all_rts_l = rts$rts_l,
    ...
  )

  return(result)
}


#' @rdname calc_stats
#' @export
calc_stats.drift_dm <- function(
  object,
  type,
  ...,
  conds = NULL,
  resample = FALSE
) {
  drift_dm_obj <- object
  type <- match.arg(type, drift_dm_stats_types("drift_dm"))
  dots <- list(...)

  # input check on resample and fit_stats
  if (type == "fit_stats" && resample) {
    warning(
      "`resampling` not available for `type = 'fit_stats'`; ",
      "setting `resampling = FALSE`"
    )
    resample <- FALSE
  }

  # if fit_stats requested, return it
  if (type == "fit_stats") {
    result <- calc_fit_stats(drift_dm_obj, ...)
    return(result)
  }

  # otherwise, continue with summary stats
  # get the conds
  model_conds <- conds(drift_dm_obj)
  if (is.null(conds)) {
    conds <- model_conds
  } else {
    conds <- match.arg(arg = conds, choices = model_conds, several.ok = TRUE)
  }

  # get b_coding
  b_coding <- attr(drift_dm_obj, "b_coding")

  # get time space
  t_max <- drift_dm_obj$prms_solve[["t_max"]]
  nt <- drift_dm_obj$prms_solve[["nt"]]
  t_vec <- seq(0, t_max, length.out = nt + 1)

  # get pdfs for quick reference
  if (is.null(drift_dm_obj$pdfs)) {
    drift_dm_obj <- re_evaluate_model(
      drift_dm_obj = drift_dm_obj,
      eval_model = TRUE
    )
  }

  # temporarily set t_max as an option, if it was not specified,
  # this ensures a consistent size of outputs (like in density)
  stats.options(t_max = t_max)

  # if resample is requested, call the underlying bootstrap function and return
  # -> stats_resample_dm returns a stats_dm object of type sum_dist, with
  # the additional column "Estimate" that codes a lower and upper boundary, as
  # well as the original statistic
  if (resample) {
    out <- stats_resample_dm(
      object = drift_dm_obj,
      conds = conds,
      type = type,
      b_coding = b_coding,
      ...
    )
    return(out)
  }

  # otherwise continue with a call to calc_stats_pred_obs
  all_pdfs <- drift_dm_obj$pdfs
  dt <- drift_dm_obj$prms_solve[["dt"]]
  result <- calc_stats_pred_obs(
    type = type,
    b_coding = b_coding,
    conds = conds,
    all_pdfs = all_pdfs,
    all_rts_u = drift_dm_obj$obs_data$rts_u,
    all_rts_l = drift_dm_obj$obs_data$rts_l,
    t_vec = t_vec,
    dt = dt,
    ...
  )

  return(result)
}


#' @rdname calc_stats
#' @export
calc_stats.fits_ids_dm <- function(
  object,
  type,
  ...,
  conds = NULL,
  resample = FALSE,
  progress = 1,
  level = "individual"
) {
  fits_ids <- object
  dots <- list(...)

  # input checks
  model_conds <- conds(fits_ids)
  if (is.null(conds)) {
    conds <- model_conds
  } else {
    conds <- match.arg(arg = conds, choices = model_conds, several.ok = TRUE)
  }

  if (!(progress %in% c(0, 1))) {
    stop("progress must be 0 or 1")
  }
  level <- match.arg(level, c("individual", "group"))

  if (type == "fit_stats" && resample) {
    warning(
      "`resampling` not available for `type = 'fit_stats'`; ",
      "setting `resampling = FALSE`"
    )
    resample <- FALSE
  }

  # deprecation warning about average
  if (!is.null(dots$average)) {
    lifecycle::deprecate_warn(
      when = "0.3.0",
      what = "calc_stats.data.frame(average = )",
      with = "calc_stats.data.frame(level =)"
    )
    if (dots$average) {
      level <- "group"
    } else {
      level <- "individual"
    }
    dots$average <- NULL
  }

  # temporarily set t_max as an option, if it was not specified,
  # this ensures a consistent size of outputs (like in density)
  stats.options(t_max = prms_solve(fits_ids)[["t_max"]])

  # if resample at the group level (i.e., resample individuals),
  # call the underlying method
  if (resample && level == "group") {
    args <- list(
      object = fits_ids,
      conds = conds,
      type = type,
      b_coding = b_coding(fits_ids),
      progress = progress,
      level = level
    )
    args <- c(args, dots)
    result <- do.call(stats_resample_dm, args)
    return(result)
  }

  # otherwise get the data of each individual
  # create a progress bar
  if (progress >= 1) {
    n_iter <- length(fits_ids$all_fits)
    pb <- progress::progress_bar$new(
      format = "calculating [:bar] :percent; done in: :eta",
      total = n_iter,
      clear = FALSE,
      width = 60
    )
    pb$tick(0)
  }

  # call statistic across individuals
  all_results <- lapply(names(fits_ids$all_fits), function(id) {
    stat <- calc_stats.drift_dm(
      object = fits_ids$all_fits[[id]],
      type = type,
      ...,
      conds = conds,
      resample = resample
    )
    stat_id <- cbind(ID = try_cast_integer(id), stat)
    stat_id <- copy_class_attributes(old = stat, new = stat_id)
    if (progress == 1) {
      pb$tick()
    }
    return(stat_id)
  })
  results <- do.call("rbind", all_results) # preserves class and attributes
  results <- results[order(results$ID), ]
  rownames(results) <- NULL

  # if group-level is requested, average across IDS
  if (level == "group") {
    results <- aggregate_stats(results)
  }

  return(results)
}


#' @rdname calc_stats
#' @export
calc_stats.fits_agg_dm <- function(
  object,
  type,
  ...,
  conds = NULL,
  resample = FALSE,
  progress = 1,
  level = "group",
  messaging = TRUE
) {
  fits_agg <- object
  dots <- list(...)
  b_coding <- b_coding(fits_agg)
  obs_data <- obs_data(fits_agg)

  # input checks
  model_conds <- conds(fits_agg)
  if (is.null(conds)) {
    conds <- model_conds
  } else {
    conds <- match.arg(arg = conds, choices = model_conds, several.ok = TRUE)
  }

  if (!(progress %in% c(0, 1))) {
    stop("progress must be 0 or 1")
  }
  level <- match.arg(level, c("individual", "group"))

  if (type == "fit_stats" && resample) {
    warning(
      "`resampling` not available for `type = 'fit_stats'`; ",
      "setting `resampling = FALSE`"
    )
    resample <- FALSE
  }

  # deprecation warning about average
  if (!is.null(dots$average)) {
    lifecycle::deprecate_warn(
      when = "0.3.0",
      what = "calc_stats.data.frame(average = )",
      with = "calc_stats.data.frame(level =)"
    )
    if (dots$average) {
      level <- "group"
    } else {
      level <- "individual"
    }
    dots$average <- NULL
  }

  # temporarily set t_max as an option, if it was not specified,
  # this ensures a consistent size of outputs (like in density)
  stats.options(t_max = prms_solve(fits_agg)[["t_max"]])

  # if resample is requested, handle the observed and predicted data
  # separately
  if (resample) {
    # observed resample results (via calc_stats to provide sampling for each
    # individual)
    args <- list(
      object = obs_data,
      type = type,
      conds = conds,
      resample = TRUE,
      progress = progress,
      level = level,
      b_coding = b_coding
    )
    results_obs <- do.call(calc_stats.data.frame, args = c(args, dots))

    # resample predicted
    if (is.null(dots$n_sim)) {
      dots$n_sim <- summary(fits_agg)$obs_data$avg_trials
      if (messaging) {
        message(
          "Generating simulations under the model using the average trial ",
          "number across participants."
        )
      }
    } else {
      if (messaging) {
        message(
          "Generating simulations under the model using the trial numbers ",
          "specified by 'n_sim'."
        )
      }
    }
    args <- list(
      object = fits_agg$drift_dm_obj,
      conds = conds,
      type = type,
      b_coding = b_coding
    )
    results_pred <- do.call(stats_resample_dm.drift_dm, args = c(args, dots))
    if (level == "individual") {
      results_pred <- cbind(ID = NA_real_, results_pred) # to ensure rbind works
    }

    # row bind and return
    return(rbind(results_obs, results_pred))
  }

  # if no resampling required, then get the standard model stats
  stats_pred <- calc_stats.drift_dm(
    object = fits_agg$drift_dm_obj,
    type = type,
    ...,
    conds = conds,
    resample = FALSE
  )
  # add a fake ID column if individual was requested and observed data exists
  if (level == "individual") {
    stats_pred <- cbind(ID = NA_real_, stats_pred)
  }

  # if statistics are requested that can not be used for observed data stop here
  check <- setdiff(drift_dm_stats_types(), drift_dm_stats_types("data.frame"))
  if (type %in% check) {
    return(stats_pred)
  }

  # get the observed stats
  args <- list(
    object = obs_data,
    type = type,
    conds = conds,
    resample = FALSE,
    progress = progress,
    level = level,
    b_coding = b_coding
  )
  stats_obs <- do.call(calc_stats.data.frame, args = c(args, dots))

  # combine and pass back
  results <- rbind(stats_obs, stats_pred)

  return(results)
}


# RESAMPLE -----------------------------------------------------------------

#' Resample Observed and Predicted Statistics for Interval Estimation
#'
#' Internal methods to generate bootstrap-like intervals for
#' descriptive statistics derived from either observed data or model
#' predictions. These methods support both `drift_dm` objects and
#' data.frames containing a single participant's observed data.
#'
#'
#' @param object a `drift_dm` object (for model-based resampling) or a
#'   [data.frame] with observed data for a single participant.
#'   `drift_dm_stats_types("sum_dists")`, such as `"quantiles"` or `"cafs"`.
#' @param conds a character vector indicating the condition(s) for which the
#'   statistics should be resampled.
#' @param type a character string, specifying the `type` of statistic to
#'   calculate
#' @param R an integer, number of replications (default is 100).
#' @param interval_level a numeric between 0 and 1, controlling the width of the
#' interval (default is 0.95).
#' @param b_coding a list, specifying the boundary coding, required
#'   when calculating the statistics.
#' @param n_sim an optional vector, providing the trial numbers for simulating
#' synthetic data under the model. Only relevant when no observed data is
#' attached to the model via the `obs_data` entry of the model.
#' @param progress an integer, specifying if a progress bar shall be displayed
#'  (`1`) or not (`0`).
#' @param level a character string, specifying at which level resampling shall
#'  take place. `"individual"` will lead to resampling of an individual's data.
#'  `"group"` will lead to resampling of the entire participant.
#'
#' @param ... additional arguments passed to
#' [dRiftDM::stats_resample_wrapper()] and [dRiftDM::simulate_data()]. Must
#' contain `type` and `b_coding`
#'
#' @details
#'
#' The `stats_resample_dm()` generic dispatches to class-specific methods.
#' For `drift_dm` objects, it generates synthetic data sets under the model.
#' For raw data, it resamples observations with replacement (i.e, it performs
#' a bootstrap). In both cases, statistics from the resampled/generated data are
#' used to compute intervals for the requested statistic.
#'
#' Resampling is done for each condition separately.
#'
#' The function `stats_resample_dm()` is called within [dRiftDM::calc_stats()].
#'
#' @return
#' A `stats_dm` object with added column `Estimate` indicating whether the row
#' represents the lower interval bound, the original value (`"orig"`), or the
#' upper interval bound. The interval level can be controlled via the
#' `interval_level` argument.
#'
#' @keywords internal
stats_resample_dm <- function(
  object,
  conds,
  type,
  b_coding,
  ...,
  R,
  interval_level
) {
  UseMethod("stats_resample_dm")
}

#' @rdname stats_resample_dm
#' @export
stats_resample_dm.drift_dm <- function(
  object,
  conds,
  type,
  b_coding,
  ...,
  R = 100,
  interval_level = 0.95,
  n_sim = NULL
) {
  drift_dm_obj <- object
  dotdot <- list(...)

  obs_data <- obs_data(drift_dm_obj, messaging = FALSE)
  if (is.null(obs_data)) {
    if (is.null(n_sim)) {
      stop(
        "No data found. In this case, please specify `n_sim` (i.e., the ",
        "number of trials for the synthetic data"
      )
    }
  }

  # now get the results for the observed data (if possible)
  result_obs <- NULL
  obs_data <- obs_data[obs_data$Cond %in% conds, ]
  if (!is.null(obs_data)) {
    result_obs <- stats_resample_dm.data.frame(
      object = obs_data,
      conds = conds,
      type = type,
      b_coding = b_coding,
      ...,
      R = R,
      interval_level = interval_level,
      progress = 0,
      level = "individual"
    )
  }

  # now get the results for the predicted data
  obs_data(drift_dm_obj) <- NULL

  # re_evaluate to get the pdfs if necessary
  if (is.null(drift_dm_obj$pdfs)) {
    drift_dm_obj <- re_evaluate_model(
      drift_dm_obj = drift_dm_obj,
      eval_model = TRUE
    )
  }

  # get the synthetic data (in a format suitable for stats_resample_wrapper)
  if (is.null(n_sim)) {
    n_sim <- sapply(conds, \(one_cond) {
      nrow(obs_data[obs_data$Cond == one_cond, ])
    })
  }

  get_synth_data <- function() {
    one_synth_data <- simulate_data.drift_dm(
      object = drift_dm_obj,
      n = n_sim,
      conds = conds,
      round_to = dotdot$round_to
    )
    split(one_synth_data, one_synth_data$Cond)
  }

  synth_data <- replicate(n = R, expr = get_synth_data(), simplify = FALSE)

  # calculate the statistics for each synthetic data set
  resample_list <- do_resampling(
    lapply(synth_data, function(one_dat_split) {
      stats_resample_wrapper(
        one_dat_split,
        type = type,
        b_coding = b_coding,
        ...
      )
    })
  )
  resample_list <- lapply(resample_list, \(x) {
    x$Source <- "pred"
    x
  })

  # get the original statistic
  tmp <- drift_dm_obj
  tmp$obs_data <- NULL
  original <- calc_stats.drift_dm(
    object = tmp,
    type = type,
    ...,
    conds = conds,
    resample = FALSE
  )

  # get the borders and Estimate column
  result_pred <- resample_assemble(
    resample_list,
    level = interval_level,
    original = original
  )

  # finally, bind everything together and pass back
  result <- rbind(result_obs, result_pred)
  return(result)
}


#' @rdname stats_resample_dm
#' @export
stats_resample_dm.data.frame <- function(
  object,
  conds,
  type,
  b_coding,
  ...,
  R = 100,
  interval_level = 0.95,
  progress = 0,
  level
) {
  obs_data <- object

  # level == "individual" treat the observed data as if it is from a single
  # participant
  if (level == "individual") {
    stopifnot(!("ID" %in% names(obs_data))) # mustn't have an ID column

    # reduce and split the data by condition
    obs_data <- obs_data[obs_data$Cond %in% conds, ]
    obs_data_split <- split(obs_data, obs_data$Cond)

    # helper function: creates idxs for each condition, returns a list of indices
    # for each condition
    get_idxs <- function() {
      n_obs <- lapply(obs_data_split, nrow)
      lapply(n_obs, \(x) sample(seq_len(x), size = x, replace = TRUE))
    }

    idx_list <- replicate(n = R, expr = get_idxs(), simplify = FALSE)

    # call the stats_resample_wrapper function for each set of indices
    resample_list <- do_resampling(
      lapply(idx_list, function(one_set_idxs) {
        stats_resample_wrapper(
          obs_data_split,
          one_set_idxs,
          type = type,
          b_coding = b_coding,
          ...
        )
      })
    )

    # get the original statistic
    original <- stats_resample_wrapper(
      obs_data_split,
      type = type,
      b_coding = b_coding,
      ...
    )
  } else if (level == "group") {
    stopifnot(("ID" %in% names(obs_data))) # must have an ID column

    # reduce to relevant conditions
    obs_data <- obs_data[obs_data$Cond %in% conds, ]

    # get the original statistic (aggregated)
    original <- calc_stats.data.frame(
      object = obs_data,
      type = type,
      ...,
      conds = conds,
      resample = FALSE,
      progress = 0,
      level = level,
      b_coding = b_coding
    )

    # get the statistics per participant
    stats_id <- calc_stats.data.frame(
      object = obs_data,
      type = type,
      ...,
      conds = conds,
      resample = FALSE,
      progress = 0,
      level = "individual",
      b_coding = b_coding
    )

    # split by subject
    stats_id_split <- split(stats_id, stats_id$ID)

    # create indices of participants across replications
    idxs <- names(stats_id_split)

    idx_list <- replicate(
      n = R,
      expr = sample(x = idxs, size = length(idxs), replace = TRUE),
      simplify = FALSE
    )

    # progress bar output
    if (progress == 1) {
      n_iter <- length(idx_list)
      pb <- progress::progress_bar$new(
        format = "calculating observed [:bar] :percent; done in: :eta",
        total = n_iter,
        clear = FALSE,
        width = 60
      )
      pb$tick(0)
    }

    # then bootstrap the statistics per subject
    resample_list <- lapply(idx_list, function(one_set_idxs) {
      stopifnot(is.character(one_set_idxs))
      boot_stats <- stats_id_split[one_set_idxs]
      boot_stats <- do.call(rbind, boot_stats)
      agg_stats <- aggregate_stats(boot_stats)
      if (progress == 1) {
        pb$tick()
      }
      return(agg_stats)
    })
  }

  # get the level borders and Estimate column
  result <- resample_assemble(
    resample_list,
    level = interval_level,
    original = original
  )

  return(result)
}


#' @rdname stats_resample_dm
#' @export
stats_resample_dm.fits_ids_dm <- function(
  object,
  conds,
  type,
  b_coding,
  ...,
  R = 100,
  interval_level = 0.95,
  progress = 0,
  level
) {
  fits_ids <- object
  # resampling at the individual level is solved by calling
  # calc_stats.drift_dm repeatedly from calc_stats.fits_ids_dm
  stopifnot(level == "group")

  # now get the results for the observed data
  obs_data <- fits_ids$drift_dm_fit_info$obs_data_ids
  result_obs <- stats_resample_dm.data.frame(
    object = obs_data,
    conds = conds,
    type = type,
    b_coding = b_coding,
    ...,
    R = R,
    interval_level = interval_level,
    progress = progress,
    level = level
  )

  # now get the results for the predicted data
  # detach observed data to avoid calculating this as well
  fits_ids$all_fits <- lapply(fits_ids$all_fits, \(x) {
    obs_data(x) <- NULL
    x
  })
  all_fits <- fits_ids$all_fits

  # get the original statistic
  original <- calc_stats.fits_ids_dm(
    object = fits_ids,
    type = type,
    ...,
    conds = conds,
    resample = FALSE,
    progress = 0,
    level = level
  )

  # get the individual statistics (already splitted)
  stats_id_split <- lapply(all_fits, \(one_fit_obj) {
    calc_stats.drift_dm(
      object = one_fit_obj,
      type = type,
      ...,
      conds = conds,
      resample = FALSE
    )
  })

  # create indices of participants across replications
  idxs <- names(stats_id_split)
  stopifnot(!is.null(idxs))

  idx_list <- replicate(
    n = R,
    expr = sample(x = idxs, size = length(idxs), replace = TRUE),
    simplify = FALSE
  )

  # progress bar output
  if (progress == 1) {
    n_iter <- length(idx_list)
    pb <- progress::progress_bar$new(
      format = "calculating predicted [:bar] :percent; done in: :eta",
      total = n_iter,
      clear = FALSE,
      width = 60
    )
    pb$tick(0)
  }

  # then bootstrap the statistics per subject
  resample_list <- lapply(idx_list, function(one_set_idxs) {
    stopifnot(is.character(one_set_idxs))
    boot_stats <- stats_id_split[one_set_idxs]
    boot_stats <- do.call(rbind, boot_stats)
    agg_stats <- aggregate_stats(boot_stats)
    if (progress == 1) {
      pb$tick()
    }
    return(agg_stats)
  })

  # get the level borders and Estimate column
  result_pred <- resample_assemble(
    resample_list,
    level = interval_level,
    original = original
  )

  result <- rbind(result_obs, result_pred)
  return(result)
}


# RESAMPLE HELPERS --------------------------------------------------------

#' Temporarily suppress new stats generation during resampling
#'
#' Internal helper that sets the `skip_new_stats_dm` and
#' `skip_validate_stats_dm` options to `TRUE` before
#' evaluating an expression, and resets them to `NULL` afterward (see also
#' [dRiftDM::stats.options()]). Intended to
#' prevent the (unncessary) creation/checking of stats_dm objects during
#' resampling.
#'
#' @param x An expression to evaluate.
#' @return The result of evaluated `x`.
#'
#' @keywords internal
do_resampling <- function(x) {
  stats.options(skip_new_stats_dm = TRUE)
  stats.options(skip_validate_stats_dm = TRUE)
  withr::defer(expr = {
    stats.options(skip_new_stats_dm = NULL)
    stats.options(skip_validate_stats_dm = NULL)
  })
  force(x)
}


#' Internal Helpers for Resampling of Summary Statistics
#'
#' These functions support the construction of intervals for
#' descriptive statistics computed from observed or simulated
#' data. They are used internally by [dRiftDM::stats_resample_dm()] methods.
#'
#'
#' @param obs_data_split a named list of [data.frame]s, containing a single set
#' of observed data, splitted by condition
#' @param one_set_idxs a named list of numeric vectors. Each entry contains
#' indices to shuffle the [data.frame]s in `obs_data_split`. Default `NULL`
#' keeps `obs_data_split` as is.
#' @param type a character, passed to [dRiftDM::calc_stats_pred_obs()].
#' @param b_coding a list with boundary coding information,
#' , required to wrangle rts to match with [dRiftDM::calc_stats_pred_obs()].
#' @param ... additional arguments passed to [dRiftDM::calc_stats_pred_obs()].
#' @param resample_list a list of statistics returned by calls to
#'   `stats_resample_wrapper()`.
#' @param level a numeric between 0 and 1, controlling the width of the interval.
#' @param original a `stats_dm` object representing the statistic computed
#'   from the original data set or model prediction.
#'
#'
#' @details
#' `stats_resample_wrapper()` wraps a call to `calc_stats_pred_obs()` for use
#' in resampling.
#'
#' `resample_assemble()` takes a list of resampled statistics and the
#' original statistic, and computes lower and upper bounds based
#' on the requested level. It returns a `stats_dm` object with an
#' added `Estimate` column.
#'
#'
#' @return
#' - `stats_resample_wrapper()` returns a single `stats_dm` object for one
#'   sample.
#' - `resample_assemble()` returns a `stats_dm` object containing the lower
#'   and upper interval bounds along with the original estimate.
#'
#' @keywords internal
#' @name resample_helpers
stats_resample_wrapper <- function(
  obs_data_split,
  one_set_idxs = NULL,
  type,
  b_coding,
  ...
) {
  if (!(type %in% drift_dm_stats_types("sum_dist"))) {
    stop("the requested statistic type ('", type, "') can't be resampled")
  }

  conds <- names(obs_data_split)
  stopifnot(!is.null(conds))

  # shuffle within each condition (if requested)
  if (!is.null(one_set_idxs)) {
    stopifnot(setequal(names(one_set_idxs), conds))
    obs_data_split <- sapply(
      conds,
      \(one_cond) {
        obs_data_split[[one_cond]][one_set_idxs[[one_cond]], ]
      },
      simplify = FALSE,
      USE.NAMES = TRUE
    )
  }

  # get the rts lists (identical to obs_data_to_rt_lists)
  b_column <- b_coding$column
  u_name_value <- b_coding$u_name_value
  l_name_value <- b_coding$l_name_value

  rts_u <- list()
  rts_l <- list()
  for (one_cond in conds) {
    sub_dat <- obs_data_split[[one_cond]]
    rts_u[[one_cond]] <- sub_dat$RT[sub_dat[[b_column]] == u_name_value]
    rts_l[[one_cond]] <- sub_dat$RT[sub_dat[[b_column]] == l_name_value]
  }

  # call the statistic function
  out <- calc_stats_pred_obs(
    type = type,
    b_coding = b_coding,
    conds = conds,
    all_rts_u = rts_u,
    all_rts_l = rts_l,
    ...
  )
  return(out)
}

#' @rdname resample_helpers
resample_assemble <- function(resample_list, level, original) {
  stopifnot(!("ID" %in% names(original)))

  # figure out the column names etc.
  stat_type <- class(original)[1]
  first_boot <- resample_list[[1]]
  all_cols <- names(first_boot)
  stopifnot(all_cols == names(original))
  relevant_cols <- character(0)
  if (stat_type == "basic_stats") {
    relevant_cols <- all_cols[grepl("^Mean_|^SD_|^P_", all_cols)]
  } else if (stat_type == "quantiles") {
    relevant_cols <- all_cols[grepl("^Quant_", all_cols)]
  } else if (stat_type == "cafs") {
    relevant_cols <- all_cols[grepl("^P_", all_cols)]
  } else if (stat_type == "delta_funs") {
    relevant_cols <- all_cols[grepl("^Quant_|^Avg_|^Delta_", all_cols)]
  } else if (stat_type == "densities") {
    relevant_cols <- all_cols[grepl("^Dens_", all_cols)]
  }
  stopifnot(length(relevant_cols) >= 1)

  alpha <- 1 - level
  level_lower <- alpha / 2
  level_upper <- level + alpha / 2

  calc_interval <- function(matrix) {
    stopifnot(is.matrix(matrix))
    # rows -> statistic realizations
    # cols -> replications
    out <- apply(
      X = matrix,
      MARGIN = 1,
      FUN = stats::quantile,
      probs = c(level_lower, level_upper),
      na.rm = TRUE
    )
    # returns probs as rows and statistic realizations as cols
    return(out)
  }

  n_rows <- nrow(first_boot)
  interval <- lapply(seq_along(relevant_cols), \(i) {
    matrix <- vapply(
      resample_list,
      \(one_entry) {
        return(one_entry[relevant_cols][[i]])
      },
      FUN.VALUE = numeric(n_rows)
    )
    return(calc_interval(matrix))
  })

  # now assemble

  # find the remaining column names and figure out where to insert the
  # new "Estimate" column
  all_other_cols <- setdiff(all_cols, relevant_cols)
  to_create <- c("lower", "orig", "upper")
  if ("Cond" %in% all_other_cols) {
    pos <- which(all_other_cols == "Cond")
  } else {
    pos <- which(all_other_cols == "Source")
  }
  stopifnot(length(pos) >= 1)
  stopifnot(pos >= 1)

  # create the lower, original, and upper data.frame
  result <- lapply(to_create, \(what) {
    estimate <- switch(
      what,
      lower = paste(level_lower * 100, "%", sep = ""),
      orig = "orig",
      upper = paste(level_upper * 100, "%", sep = "")
    )
    dv_cols <- switch(
      what,
      lower = lapply(interval, `[`, 1, ),
      orig = original[relevant_cols],
      upper = lapply(interval, `[`, 2, )
    )
    template <- switch(
      what,
      lower = first_boot,
      upper = first_boot,
      orig = original
    )
    df <- template[all_other_cols]
    if (pos == ncol(df)) {
      df <- cbind(df, Estimate = estimate)
    } else {
      df <- cbind(df[1:pos], Estimate = estimate, df[(pos + 1):ncol(df)])
    }
    df[relevant_cols] <- dv_cols
    return(df)
  })

  # bind together and return (as a stats_dm object)
  result <- do.call("rbind", result)
  if ("Cond" %in% names(result)) {
    result <- result[order(result$Source, result$Cond), ]
  } else {
    result <- result[order(result$Source), ]
  }
  rownames(result) <- NULL
  # ensures that the source is always clear (when resampling under the model,
  # the bootstrapped data sets are treated as observed data)
  result$Source <- unique(original$Source)
  result <- copy_class_attributes(old = original, new = result)
  validate_stats_dm(result)

  return(result)
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
#' `stat_df`. Valid options include `"basic_stats"`, `"cafs"`, `"quantiles"`,
#' `"delta_funs"`, and `"fit_stats"`.
#' @param ... Additional arguments passed to set attributes. For `"cafs"`,
#' `"quantiles"`, and `"delta_funs"`, a `b_coding` attribute is required.
#'
#' @details
#' `new_stats_dm` sets up the `stat_df` object by assigning it the class
#' `stats_dm`, along with additional classes based on the specified `type`.
#' For "basic_stats", "cafs", "quantiles", "delta_funs", this will be
#' c(`type`, "sum_dist", "stats_dm", "data.frame")". For "fit_stats", this
#' will be c(`type`, "stats_dm", "data.frame")".
#'
#' For basic stats, Conditional Accuracy Functions (CAFs), Quantiles, and
#' Delta Functions, the function requires a `b_coding` argument, which specifies
#' boundary coding details and is set as an attribute.
#'
#' The function performs validation through [dRiftDM::validate_stats_dm] to
#' ensure that the `stats_dm` object is well formatted.
#'
#' @return An object of class `stats_dm`, with additional classes and attributes
#' depending on `type`.
#'
#' @keywords internal
new_stats_dm <- function(stat_df, type, ...) {
  # if the option-flag skip_new_stats_dm was set, don't create an object
  # of this type (done for performance reasons during bootstrapping)
  if (isTRUE(stats.options("skip_new_stats_dm"))) {
    return(stat_df)
  }

  # input checks
  stopifnot(is.data.frame(stat_df))

  # type of stats
  type <- match.arg(arg = type, choices = drift_dm_stats_types())

  # turn all NaNs to NA (to have a consistent output for missing values
  stat_df[] <- lapply(stat_df, function(x) {
    x[is.nan(x)] <- NA_real_
    return(x)
  })

  # define the stat_df object as an object of class stats_dm
  class(stat_df) <- c("stats_dm", "data.frame")

  # if it is a summary statistic
  # add b_coding and more info about object
  dots <- list(...)
  if (type %in% drift_dm_stats_types("sum_dist")) {
    b_coding <- dots$b_coding
    stopifnot(!is.null(b_coding))
    attr(stat_df, "b_coding") <- b_coding
    class(stat_df) <- c(type, "sum_dist", class(stat_df))
  } else {
    # else just add the object info
    class(stat_df) <- c(type, class(stat_df))
  }

  # check if everything is well
  stat_df <- validate_stats_dm(stat_df)
  return(stat_df)
}


# VALIDATE stats_dm objects -----------------------------------------------

#' Validate a stats_dm Object
#'
#' @description
#' `validate_stats_dm` is an internal (i.e., not exported) generic function to
#' ensure that `stats_dm` objects, as well as their specific subclasses
#' (`basic_stats`, `cafs`, `quantiles`, `delta_funs`, `sum_dist`, and
#' `fit_stats`), meet the necessary structural and column requirements. Each
#' method performs class-specific validation checks.
#'
#' @param stat_df A `data.frame` of class `stats_dm`, `basic_stats`, `cafs`,
#' `quantiles`, `delta_funs`, `sum_dist`, or `fit_stats` containing the
#' calculated statistics to be validated.
#'
#' @details
#' The validation process checks for required columns and structure based on the
#' class of `stat_df`. Each class has specific requirements:
#'
#' - **`validate_stats_dm.stats_dm`:** Ensures `stat_df` is a `data.frame`.
#' - **`validate_stats_dm.basic_stats`:** Checks for the presence of `"Cond"`,
#'   exactly two columns with prefix `"Mean_`, and exactly one column prefixed
#'   with `"P_"`
#' - **`validate_stats_dm.cafs`:** Checks for the presence of `"Bin"`, `"Cond"`,
#'   and exactly one column prefixed with `"P_"`
#' - **`validate_stats_dm.quantiles`:** Requires `"Prob"`, `"Cond"`, and exactly
#'   two columns prefixed with `"Quant_"`
#' - **`validate_stats_dm.delta_funs`:** Ensures `"Prob"` exists, at least two
#'   columns prefixed with `"Quant_"`, and at least one column  each `Avg_`
#'   and `Delta_`
#' - **`validate_stats_dm.delta_funs`:** Ensures `"Cond"`, `"Time"`, and
#'   `"Stat"` exists, and at least two column with `"Dens_"`.
#' - **`validate_stats_dm.sum_dist`:** Checks for a `"Source"` column. Here,
#'  it is also checked whether cell combinations appear equally often.
#' - **`validate_stats_dm.fit_stats`:** Checks for if the fit statistics
#'   summarize a log-likelihood cost function or the RMSE statistic. In the
#'   former case, the columns `"Log_Like"`, `"Neg_Log_Like"`, `"AIC"`, and `"BIC"`
#'   are expected. In the latter case, the columns `"RMSE_ms"` and `"RMSE_s"`
#'   are expected.
#'
#'
#' @return Returns the unmodified `stat_df` for convenience.
#'
#' @keywords internal
validate_stats_dm <- function(stat_df) {
  if (isTRUE(stats.options("skip_validate_stats_dm"))) {
    return(stat_df)
  }
  UseMethod("validate_stats_dm")
}


#' @rdname validate_stats_dm
#' @export
validate_stats_dm.basic_stats <- function(stat_df) {
  NextMethod() # to validate stats_dm objects

  if (!("Cond" %in% colnames(stat_df))) {
    stop("no column 'Cond' in stats_dm")
  }

  if (sum("Mean_" == substr(colnames(stat_df), 1, 5)) != 2) {
    stop("couldn't find two Mean_ columns")
  }

  if (sum("SD_" == substr(colnames(stat_df), 1, 3)) != 2) {
    stop("couldn't find two SD_ columns")
  }

  if (sum("P_" == substr(colnames(stat_df), 1, 2)) != 1) {
    stop("no unique column 'P_' in stats_dm")
  }

  return(stat_df)
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
validate_stats_dm.densities <- function(stat_df) {
  NextMethod() # to validate sum_dist objects

  if (!("Cond" %in% colnames(stat_df))) {
    stop("no column 'Cond' in stats_dm")
  }

  if (!("Stat" %in% colnames(stat_df))) {
    stop("no column 'Stat' in stats_dm")
  }

  if (!("Time" %in% colnames(stat_df))) {
    stop("no column 'Time' in stats_dm")
  }

  if (sum("Dens_" == substr(colnames(stat_df), 1, 5)) != 2) {
    stop("couldn't find two Dens_ columns")
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

  # check for equal count data (if available)
  check_equal_n <- c(
    "ID",
    "Source",
    "Cond",
    "Prob",
    "Bin",
    "Estimate",
    "Stat",
    "Time"
  )
  check_equal_n <- intersect(names(stat_df), check_equal_n)
  counts <- table(
    stat_df[, check_equal_n],
    useNA = "ifany"
  )
  counts <- as.data.frame(counts)
  counts <- counts[counts$Freq > 0, , drop = FALSE]
  check <- length(unique(counts$Freq)) == 1L
  if (!check) {
    stop("The number of data points across factor levels is not equal")
  }

  return(stat_df)
}


#' @rdname validate_stats_dm
#' @export
validate_stats_dm.fit_stats <- function(stat_df) {
  NextMethod() # to validate stats_dm objects

  cols <- colnames(stat_df)
  exp_col_names <- c(
    "Log_Like",
    "Neg_Log_Like",
    "AIC",
    "BIC",
    "RMSE_s",
    "RMSE_ms"
  )

  if (!identical(exp_col_names, cols)) {
    stop(
      "stats_dm object does not have the expected column names.\n",
      "Expected: ",
      paste(exp_col_names, collapse = ", "),
      "\n",
      "Found: ",
      paste(cols, collapse = ", ")
    )
  }

  return(stat_df)
}


#' @rdname validate_stats_dm
#' @export
validate_stats_dm.stats_dm <- function(stat_df) {
  if (!is.data.frame(stat_df)) {
    stop("stats_dm object to validate is not of type data.frame")
  }
  if (nrow(stat_df) == 0) {
    stop("stats_dm object to validate has zero rows")
  }
  return(stat_df)
}


# AGGREGATE stats_dm OBJECTS ----------------------------------------------

#' Aggregate Statistics Across ID
#'
#' @description
#' `aggregate_stats` is a (not exported) function to aggregate
#' `stats_dm` objects across `ID`s. Since the column names may vary by the
#' statistic type, the behavior of aggregate depends on the subclass of
#' `stats_dm`.
#'
#' @param stat_df A `data.frame` of class `stats_dm` (see
#' [dRiftDM::new_stats_dm()])
#'
#' @details
#' `aggregate_stats` calls the [dRiftDM::internal_aggregate()] with the
#' relevant arguments
#'
#' @return Returns the statistics aggregated across the relevant cols.
#'
#' @seealso [dRiftDM::new_stats_dm], [dRiftDM::calc_stats],
#' [dRiftDM::internal_aggregate()]
#'
#' @keywords internal
aggregate_stats <- function(stat_df) {
  stopifnot(inherits(stat_df, "stats_dm"))
  internal_aggregate(
    data = stat_df,
    group_cols = c("Source", "Cond", "Estimate", "Bin", "Prob", "Stat", "Time")
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
#' specified columns. Columns specified in `group_cols` that are not part of
#' `data` are ignored silently.
#'
#' @return A `data.frame` containing the aggregated data.
#'
#' @seealso [dRiftDM::aggregate_stats()], [dRiftDM::calc_stats()],
#' [dRiftDM::new_stats_dm()]
#'
#' @keywords internal
internal_aggregate <- function(data, group_cols) {
  all_cols <- colnames(data)
  for_agg <- unpack_obj(data)

  # Select columns that aren't grouping or ID columns
  dv_cols <- setdiff(all_cols, c("ID", group_cols))

  # Keep only present group columns
  group_cols <- intersect(all_cols, group_cols)

  # Capture original group combinations in their appearance order
  original_keys <- unique(for_agg[group_cols])

  # Perform aggregation (aggregate() sorts group columns internally)
  agg_df <- stats::aggregate(
    x = for_agg[dv_cols],
    by = for_agg[rev(group_cols)], # reverse to keep grouping priority
    FUN = mean,
    na.rm = TRUE
  )

  # Reorder columns to match expected layout
  agg_df <- agg_df[c(group_cols, dv_cols)]

  # Reorder rows to match original order
  if (length(group_cols) >= 1) {
    key_agg <- do.call(paste, agg_df[group_cols])
    key_orig <- do.call(paste, original_keys[group_cols])
    agg_df <- agg_df[match(key_orig, key_agg), ]
  }
  rownames(agg_df) <- NULL

  # turn NAN to NA
  agg_df[] <- lapply(agg_df, function(x) {
    x[is.nan(x)] <- NA_real_
    return(x)
  })

  # Keep class and attributes
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
  for (one_attr in lost_attribtues) {
    # doesn't ensure sorting
    attr(new, one_attr) <- attr(old, one_attr)
  }

  return(new)
}


# HELPER TO TEMPORARILY SET OPTIONS ---------------------------------------

#' Helper to get, set, or reset package-global options for statistics
#'
#' Internal utility to manage global options for the package via the
#' "stats.dRiftDM" option slot.
#'
#' @param ... input, see Details below
#'
#' @details
#'
#' Usage patterns:
#' - `stats.options()`:
#'     Returns the full list of currently stored options.
#' - `stats.options(name)`:
#'     Returns the value of a specific option (must be a single unnamed string).
#' - `stats.options(name = value, ...)`:
#'     Sets (or updates) named option(s).
#' - `stats.options(NULL)`:
#'     Resets (clears) the entire option list.
#'
#' This function is intended for internal use only.
#' It behaves similarly to [options()] and keeps all package-specific options
#' in a single named list under `getOption("stats.dRiftDM")`.
#'
#' Setting an argument can only be done once with this function, any additional
#' attempts to modify an option will not work (unless this argument is
#' explicitly set to `NULL`).
#'
#' @return Depending on usage:
#' - Full list of options (if no input),
#' - A specific option value (if string input),
#' - Invisibly `NULL` (if setting or resetting options).
#'
#' @keywords internal
stats.options <- function(...) {
  dots <- list(...)
  args <- getOption("stats.dRiftDM")

  # No input: return full current option list
  if (length(dots) == 0) {
    return(args)
  }

  # Named arguments: update options
  if (!is.null(names(dots)) && all(nzchar(names(dots)))) {
    updated_args <- args
    for (nm in names(dots)) {
      if (is.null(dots[[nm]])) {
        updated_args[[nm]] <- NULL # explicitly remove it
      } else if (!(nm %in% names(updated_args))) {
        updated_args[[nm]] <- dots[[nm]] # only add new elements
      }
    }
    options("stats.dRiftDM" = updated_args)
    return(invisible(NULL))
  }

  # Unnamed single argument: retrieve specific entry or reset everything
  stopifnot(length(dots) == 1)
  input <- dots[[1]]

  # if input was NULL, then reset everything
  if (is.null(input)) {
    options("stats.dRiftDM" = NULL)
    return(invisible(NULL))
  }

  # if input was not NULL, check if it is a single character and retrieve entry
  stopifnot(is.character(input), length(input) == 1)
  return(args[[input]])
}

# UNPACK METHODS ----------------------------------------------------------

#' @rdname unpack_obj
#' @export
unpack_obj.stats_dm <- function(object, ..., unpack_elements = TRUE) {
  if (unpack_elements) {
    object <- as.data.frame(object)
    attr(object, "b_coding") <- NULL
  }

  return(object)
}


#' @rdname unpack_obj
#' @export
unpack_obj.stats_dm_list <- function(
  object,
  ...,
  unpack_elements = TRUE,
  type = NULL
) {
  # default is all stored stat types
  if (is.null(type)) {
    type <- names(object)
  }
  type <- match.arg(type, names(object), several.ok = TRUE)

  # iterate across all
  stats <- sapply(
    type,
    function(x) {
      unpack_obj(object[[x]], unpack_elements = unpack_elements)
    },
    simplify = FALSE,
    USE.NAMES = TRUE
  )

  if (length(type) == 1) {
    return(stats[[1]])
  } else {
    return(stats)
  }
}
