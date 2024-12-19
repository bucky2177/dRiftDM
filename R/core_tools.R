# FUNCTIONS FOR INVERSE CDF SAMPLING --------------------------------------

#' Draw Samples Using Inverse Transform Sampling
#'
#' @description
#' `draw_from_pdf` generates samples from a given probability density function
#' (PDF) using inverse transform sampling. This function takes in a numeric PDF
#' vector and a corresponding domain vector, then returns a specified number
#' of samples.
#'
#' @param a_pdf a numeric vector representing the PDF values.
#' @param x_def a numeric vector defining the domain (or x-values) corresponding
#' to the values in `a_pdf`. The vector `x_def` must be sorted in increasing
#' order.
#' @param k a single integer specifying the number of samples to generate.
#' @param seed an optional single integer value used to set the seed for random
#' number generation, allowing for reproducibility of results.
#'
#' @details
#' This function implements inverse transform sampling by first constructing a
#' cumulative distribution function (CDF) from the given PDF. A uniform random
#' variable is then sampled for each of the `k` samples, and the corresponding
#' value in `x_def` is selected by locating the appropriate interval in the CDF.
#'
#' @returns A numeric vector of length `k` containing the sampled values from
#' the specified PDF. If `k` is 0, an empty numeric vector is returned.
#'
#' @keywords internal
draw_from_pdf <- function(a_pdf, x_def, k, seed = NULL) {
  if (!is_numeric(a_pdf) | length(a_pdf) < 1) {
    stop("a_pdf must provide a valid numeric vector of length > 0")
  }
  if (!is_numeric(x_def) | length(x_def) < 1) {
    stop("x_def must provide a valid numeric vector of length > 0")
  }

  if (length(a_pdf) != length(x_def)) {
    stop("the length of x_def and a_pdf don't match")
  }
  if (!is_numeric(k) | length(k) != 1) {
    stop("k must be a single valid numeric")
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



# FUNCTIONS FOR SIMULATING PRMS --------------------------------------------

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
#'  truncated normal distribution. `NUll` will lead to `"unif"` (default).
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
#' \code{length(lower);length(upper)} columns. Otherwise a matrix with
#'  the same number of rows and columns. Columns are labeled either from V1 to
#'  Vk or in case `lower` and `upper` are named numeric vectors using the
#'  labels of both vectors.
#'
#' If `add_id_column` is not "none", an ID column is provided of the respective
#' data type.
#'
#' The data type of the parameters will be numeric, unless `add_id_column`
#' is "character" and `cast_to_data_frame` is FALSE. In this case the returned
#' matrix will be of type character.
#'
#' @examples
#'
#' # Example 1: Draw from uniform distributions ------------------------------
#' lower = c(a = 1, b = 1, c = 1)
#' upper = c(a = 3, b = 4, c = 5)
#' values = simulate_values(
#'   lower = lower,
#'   upper = upper,
#'   k = 50,
#'   add_id_column = "none"
#' )
#' summary(values)
#'
#' # Example 2: Draw from truncated normal distributions ---------------------
#' lower = c(a = 1, b = 1, c = 1)
#' upper = c(a = 3, b = 4, c = 5)
#' means = c(a = 2, b = 2.5, c = 3)
#' sds = c(a = 0.5, b = 0.5, c = 0.5)
#' values = simulate_values(
#'   lower = lower,
#'   upper = upper,
#'   distr = "tnorm",
#'   k = 5000,
#'   add_id_column = "none",
#'   means = means,
#'   sds = sds
#' )
#' quantile(values$a, probs = c(0.025, 0.5, 0.975))
#' quantile(values$b, probs = c(0.025, 0.5, 0.975))
#' quantile(values$c, probs = c(0.025, 0.5, 0.975))
#'
#'
#' @export
simulate_values <- function(lower, upper, k, distr = NULL,
                            cast_to_data_frame = TRUE,
                            add_id_column = "numeric",
                            seed = NULL, ...) {
  dotdot <- list(...)

  # input checks
  if (!is_numeric(lower) | length(lower) <= 0) {
    stop("lower must be a valid numeric vector with length >= 1")
  }
  if (!is_numeric(upper) | length(upper) <= 0) {
    stop("upper must be a valid numeric vector with length >= 1")
  }
  if (length(upper) != length(lower)) {
    stop("lower and upper are not of the same length")
  }
  names_upper <- names(upper)
  names_lower <- names(lower)
  if (!isTRUE(all.equal(names_upper, names_lower))) {
    stop("labels provided in upper and lower don't match!")
  }

  if (any(lower >= upper)) {
    stop("values in lower are not always smaller than the values in upper")
  }

  if (!is_numeric(k) | length(k) != 1) {
    stop("k must be a single numeric")
  }

  if (is.null(distr)) {
    distr <- "unif"
  }
  distr <- match.arg(distr, c("unif", "tnorm"))

  if (!is.logical(cast_to_data_frame) | length(cast_to_data_frame) != 1) {
    stop("cast_to_data_frame must be a single logical value")
  }

  if (is.logical(add_id_column) && !add_id_column) add_id_column <- "none"
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
    if (!is_numeric(means) | length(means) != n_prms) {
      stop("means is not a valid numeric vector with length equal to lower/upper")
    }
    if (!is_numeric(sds) | length(sds) != n_prms) {
      stop("sds is not numeric with length equal to lower/upper")
    }
    names_means <- names(means)
    names_sds <- names(sds)
    if (!isTRUE(all.equal(names_means, names_sds))) {
      stop("labels provided in means and sds don't match!")
    }
    if (!isTRUE(all.equal(names_means, names_upper))) {
      stop("labels provided in means/sds don't match with upper/lower!")
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

  if (cast_to_data_frame) prms <- as.data.frame(prms)

  ids <- 1:k
  if (add_id_column == "numeric") {
    prms <- cbind(prms, ID = ids)
  } else if (add_id_column == "character") {
    prms <- cbind(prms, ID = as.character(ids))
  }



  return(prms)
}
