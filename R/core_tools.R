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
    withr::local_seed(seed)
  }

  a_pdf <- a_pdf + drift_dm_robust_prm()
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

#=============================
# FUNCTION FOR SIMULATING PRMS



#' Simulate Values
#'
#' Draw values, most likely model parameters.
#'
#' @param lower,upper numeric vectors, indicating the lower/upper boundary of
#' the drawn values.
#' @param n numeric, the number of values to be drawn for each value pair of
#' lower/upper
#' @param distr character, indicating which distribution to draw from. Currently
#'  available are: `"unif"` for a uniform distribution or `"tnorm"` for a
#'  truncated normal distribution
#' @param seed numeric, optional seed for making the simulation reproducable
#' @param ... further arguments relevant for the distribution to draw from
#'  (see details)
#'
#' @details
#' When drawing from a truncated normal distribution, users must provide values
#' for the arguments `means` and `sds`. These are numeric vectors of the same
#' size as `lower` and `upper`, and indicate the mean and the standard deviation
#' of the normal distribution.
#'
#'
#' @return
#' A matrix with `n` rows and \code{length(lower)/length(upper)} columns.
#'
#' @export
simulate_values = function(lower, upper, n, distr = "unif", seed = NULL, ...) {
  dotdot = list(...)

  #input checks
  if (!is.numeric(lower) | length(lower) <= 0)
    stop("lower must be numeric with length >= 1")
  if (!is.numeric(upper) | length(upper) <= 0)
    stop("upper must be numeric with length >= 1")
  if (length(upper) != length(lower))
    stop("lower and upper are not of the same length")
  if (!is.numeric(n) | length(n) != 1)
    stop("n must be a single numeric")

  distr = match.arg(distr, c("unif", "tnorm"))

  if (!is.null(seed)) {
    if (!is.numeric(seed) | length(seed) != 1) {
      stop("seed must be a single numeric")
    }
    withr::local_seed(seed)
  }

  # draw the parameters
  n_prms = length(lower)
  if (distr == "unif") {
    prms = lapply(1:n_prms, function(i){
      runif(n, min = lower[i], max = upper[i])
    })
  } else if (distr == "tnorm") {
    means = dotdot$means
    sds = dotdot$sds
    if (is.null(means))
      stop("tnorm was requested but no means argument provided")
    if (is.null(sds))
      stop("tnorm was requested but no sds argument provided")
    if (!is.numeric(means) | length(means) != n_prms)
      stop("means is not numeric with length equal to lower/upper")
    if (!is.numeric(sds) | length(sds) != n_prms)
      stop("sds is not numeric with length equal to lower/upper")

    prms = lapply(1:n_prms, function(i){
      cdf_val_l = pnorm(q = lower[i], mean = means[i], sd = sds[i])
      cdf_val_u = pnorm(q = upper[i], mean = means[i], sd = sds[i])
      cdf_vals = runif(n = n, min = cdf_val_l, max = cdf_val_u)
      qnorm(p = cdf_vals, mean = means[i], sd = sds[i])
    })
  }
  prms = do.call("cbind", prms)
  return(prms)
}
