# === FUNCTIONS USED WITHIN KFE_ALE

tridiag <- function(f, a, b, c) {
  stopifnot(is.numeric(f) & length(f) > 1)
  stopifnot(is.numeric(a) & length(a) > 1)
  stopifnot(is.numeric(b) & length(b) == 1)
  stopifnot(is.numeric(c) & length(c) > 1)
  stopifnot(length(f) == length(a) & length(f) == length(c))

  n <- length(f)
  ab <- matrix(0, nrow = 3, ncol = n)
  ab[2, ] <- b
  ab[2, c(1, n)] <- 1
  ab[1, 3:(n - 1)] <- c[3:(n - 1)]
  ab[3, 2:(n - 2)] <- a[2:(n - 2)]

  # Solve tridiagonal system
  x <- limSolve::Solve.banded(abd = ab, nup = 1, nlow = 1, B = f, full = F)
  x <- as.vector(x)
  return(x)
}


# ==== FUNCTIONS FOR INVERSE CDF SAMPLING
draw_from_pdf <- function(a_pdf, x_def, k, seed = NULL) {
  if (length(a_pdf) != length(x_def)) {
    stop("the length of x_def and a_pdf don't match")
  }
  if (!is.numeric(k) || k < 0) {
    stop("k must be a numeric >= 0")
  }

  if (k == 0) {
    return(numeric())
  }

  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1) {
      stop("seed must be a single numeric")
    }
    withr::local_seed(seed)
  }

  cdf <- cumsum(a_pdf) # 'integrate' the cdf from the pdf
  cdf <- cdf / max(cdf) # normalize

  u <- stats::runif(k)
  indices <- sapply(u, function(one_u) which.max((cdf - one_u) > 0))
  samples <- x_def[indices]

  return(samples)
}
