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

  a_pdf = a_pdf + drift_dm_robust_prm()
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
