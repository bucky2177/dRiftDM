# ==== FUNCTIONS FOR INVERSE CDF SAMPLING
draw_from_pdf <- function(a_pdf, x_def, k, seed = NULL) {
  if (!is.numeric(a_pdf) | length(a_pdf) < 1) {
    stop("a_pdf must be of type numeric vector of length > 0")
  }
  if (!is.numeric(x_def) | length(x_def) < 1) {
    stop("x_def must be of type numeric vector of length > 0")
  }

  if (length(a_pdf) != length(x_def)) {
    stop("the length of x_def and a_pdf don't match")
  }
  if (!is.numeric(k) | length(k) != 1) {
    stop("k must be a single numeric")
  }
  if (k < 0) stop("k must be >= 0")

  if (k == 0) {
    return(numeric())
  }

  if (!is.null(seed)) {
    if (!is.numeric(seed) | length(seed) != 1) {
      stop("seed must be a single numeric")
    }
    withr::local_preserve_seed()
    set.seed(seed)
  }


  if (min(a_pdf) < 0) {
    warning(
      "negative pdf values encountered when drawing values from a pdf. ",
      "Approximate inverse sampling may not work in this case."
    )
  }

  cdf <- cumsum(a_pdf) # 'integrate' the cdf from the pdf
  cdf <- cdf / max(cdf) # normalize

  u <- stats::runif(k)
  indices <- sapply(u, function(one_u) which.max((cdf - one_u) > 0))
  samples <- x_def[indices]

  return(samples)
}

# =============================
# FUNCTION FOR SIMULATING PRMS



#' Simulate Values
#'
#' Draw values, most likely model parameters.
#'
#' @param lower,upper Numeric vectors, indicating the lower/upper boundary of
#' the drawn values.
#' @param k Numeric, the number of values to be drawn for each value pair of
#' lower/upper. If named numeric, the labels are used for the column names
#' of the returned object
#' @param distr Character, indicating which distribution to draw from. Currently
#'  available are: `"unif"` for a uniform distribution or `"tnorm"` for a
#'  truncated normal distribution
#' @param cast_to_data_frame Logical, controls whether the returned object
#' is of type data.frame (TRUE) or matrix (FALSE). Default is TRUE
#' @param add_id_column Character, controls whether an ID column should be
#' added. Options are "numeric", "character", or "none". If "numeric" or
#' "character" the column ID provides values from 1 to k of the respective type.
#' If none, no column is added. Note that "character" casts all simulated values
#' to character if the argument `cast_to_data_frame` is set to FALSE.
#' @param ... Further arguments relevant for the distribution to draw from
#' @param seed Numeric, optional seed for making the simulation reproducable
#'  (see details)
#'
#' @details
#' When drawing from a truncated normal distribution, users must provide values
#' for the arguments `means` and `sds`. These are numeric vectors of the same
#' size as `lower` and `upper`, and indicate the mean and the standard deviation
#' of the normal distributions.
#'
#'
#' @return
#' If `cast_to_data_frame` is TRUE, a data.frame with `k` rows and at least
#' \code{length(lower)/length(upper)} columns. Otherwise a matrix with
#'  the same number of rows and columns. Columns are labeled either from
#'  V1 to Vk or in case `lower` and `upper` are named numeric vectors using
#'  the labels of both vectors.
#'
#' If `add_id_column` is not "none", an ID column is provided.
#'
#'
#' @export
simulate_values <- function(lower, upper, k, distr = "unif",
                            cast_to_data_frame = T, add_id_column = "numeric",
                            seed = NULL, ...) {
  dotdot <- list(...)

  # input checks
  if (!is.numeric(lower) | length(lower) <= 0) {
    stop("lower must be numeric with length >= 1")
  }
  if (!is.numeric(upper) | length(upper) <= 0) {
    stop("upper must be numeric with length >= 1")
  }
  if (length(upper) != length(lower)) {
    stop("lower and upper are not of the same length")
  }
  names_upper <- names(upper)
  names_lower <- names(lower)
  if (!isTRUE(all.equal(names_upper, names_lower))) {
    stop("labels provided in upper and lower don't match!")
  }


  if (!is.numeric(k) | length(k) != 1) {
    stop("k must be a single numeric")
  }

  distr <- match.arg(distr, c("unif", "tnorm"))

  if (!is.logical(cast_to_data_frame) | length(cast_to_data_frame) != 1) {
    stop("cast_to_data_frame must be a single logical value")
  }

  add_id_column <- match.arg(add_id_column, c("numeric", "character", "none"))

  if (!is.null(seed)) {
    if (!is.numeric(seed) | length(seed) != 1) {
      stop("seed must be a single numeric")
    }
    withr::local_preserve_seed()
    set.seed(seed)
  }

  # draw the parameters
  n_prms <- length(lower)
  if (distr == "unif") {
    prms <- lapply(1:n_prms, function(i) {
      stats::runif(k, min = lower[i], max = upper[i])
    })
  } else if (distr == "tnorm") {
    means <- dotdot$means
    sds <- dotdot$sds
    if (is.null(means)) {
      stop("tnorm was requested but no means argument provided")
    }
    if (is.null(sds)) {
      stop("tnorm was requested but no sds argument provided")
    }
    if (!is.numeric(means) | length(means) != n_prms) {
      stop("means is not numeric with length equal to lower/upper")
    }
    if (!is.numeric(sds) | length(sds) != n_prms) {
      stop("sds is not numeric with length equal to lower/upper")
    }

    prms <- lapply(1:n_prms, function(i) {
      cdf_val_l <- stats::pnorm(q = lower[i], mean = means[i], sd = sds[i])
      cdf_val_u <- stats::pnorm(q = upper[i], mean = means[i], sd = sds[i])
      cdf_vals <- stats::runif(n = k, min = cdf_val_l, max = cdf_val_u)
      stats::qnorm(p = cdf_vals, mean = means[i], sd = sds[i])
    })
  }
  prms <- do.call("cbind", prms)

  # wrangle and pass back
  col_names <- paste0("V", 1:length(upper))
  if (!is.null(names_upper)) col_names <- names_upper
  colnames(prms) <- col_names

  ids <- 1:k
  if (add_id_column == "numeric") {
    prms <- cbind(prms, ID = ids)
  } else if (add_id_column == "character") {
    prms <- cbind(prms, ID = as.character(ids))
  }


  if (cast_to_data_frame) prms <- as.data.frame(prms)

  return(prms)
}
