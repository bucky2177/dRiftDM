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
#' @param round_to an optional integer, indicating the number of digits to
#' which the result should be rounded.
#' @param method a single character string. If "discr", then simulated values
#' match `x_def`. If "linear", `x_def` and `a_pdf` are linearly interpolated,
#' so that the simulated values can lay in between the discrete values of
#' `x_def`.
#'
#' @details
#' This function implements inverse transform sampling by first constructing a
#' cumulative distribution function (CDF) from the given PDF. Then `k` values
#' between zero and one are sampled from a uniform distribution, and
#' the corresponding values are mapped to `x_def` using linear interpolation.
#'
#' @returns A numeric vector of length `k` containing the sampled values from
#' the specified PDF. If `k` is 0, an empty numeric vector is returned.
#'
#' @keywords internal
draw_from_pdf <- function(a_pdf, x_def, k, seed = NULL,
                          round_to = NULL, method = "discr") {
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

  if (!is.null(round_to)) {
    if (!is_numeric(round_to) | length(round_to) != 1) {
      stop("round_to must be a single valid numeric")
    }
  }


  method = match.arg(method, choices = c("discr", "linear"))

  if (min(a_pdf) < 0) {
    warning(
      "negative pdf values encountered when drawing values from a pdf. ",
      "Approximate inverse sampling may not work in this case."
    )
  }

  # create the cdf
  if (method == "discr") {
    cdf <- cumsum(a_pdf)
  } else {
    cdf <- cumtrapz(x = x_def, y = a_pdf)
  }
  cdf <- cdf / max(cdf) # normalize


  # draw values between zero and one and then map it to the cdf
  u <- stats::runif(k)
  if (method == "discr") {
    indices <- sapply(u, function(one_u) which.max((cdf - one_u) > 0))
    samples <- x_def[indices]
  } else {
    samples <- stats::approx(x = cdf, y = x_def, xout = u)$y
  }

  # round if requested
  if (!is.null(round_to))
    samples <- round(samples, digits = round_to)


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
#' lower <- c(a = 1, b = 1, c = 1)
#' upper <- c(a = 3, b = 4, c = 5)
#' values <- simulate_values(
#'   lower = lower,
#'   upper = upper,
#'   k = 50,
#'   add_id_column = "none"
#' )
#' summary(values)
#'
#' # Example 2: Draw from truncated normal distributions ---------------------
#' lower <- c(a = 1, b = 1, c = 1)
#' upper <- c(a = 3, b = 4, c = 5)
#' means <- c(a = 2, b = 2.5, c = 3)
#' sds <- c(a = 0.5, b = 0.5, c = 0.5)
#' values <- simulate_values(
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
      stop("sds is not a valid numeric vector with length equal to lower/upper")
    }
    names_means <- names(means)
    names_sds <- names(sds)
    if (!isTRUE(all.equal(names_means, names_sds))) {
      stop("labels provided in means and sds don't match!")
    }
    if (!isTRUE(all.equal(names_means, names_upper))) {
      stop("labels provided in means/sds don't match with lower/upper!")
    }

    prms <- lapply(1:n_prms, function(i) {
      rtnorm(n = k, mean = means[i], sd = sds[i], lower = lower[i],
             upper = upper[i])
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




# FUNCTIONS FOR DX AND DT SETTINGS ----------------------------------------


#' Check time/space discretization via reference comparison
#'
#' @description
#'
#' `check_discretization()` helps you choose or check time (`dt`) and space
#' (`dx`) discretization settings. It computes a high-precision *reference*
#' solution of the model's PDFs with `dt_ref`/`dx_ref`, and then compares the
#' reference PDFs to the discretization settings of the supplied object, using
#' the Hellinger distance per condition. Smaller distances indicate closer
#' agreement with the reference --- i.e., a sufficiently fine grid.
#'
#' There are not yet overall and officially published recommendations on how
#' large the Hellinger distance can be without affecting model precision, and
#' this might even depend on the model itself. Based on some preliminary
#' simulations we would recommend trying to keep the Hellinger Distance between
#' below 5\% on average.
#'
#' @param object a [dRiftDM::drift_dm], `fits_agg_dm`, or `fits_ids_dm` object.
#'   (the latter two are returned by [dRiftDM::estimate_dm()])
#' @param dt_ref,dx_ref numeric scalars, providing a fine time or space step
#'   size for the reference solution. Defaults to `0.001`.
#' @param ... further arguments passed forward to the respective method.
#'
#' @return a named numeric vector of Hellinger distances (one per condition)
#'   if `object` is of type [dRiftDM::drift_dm] or `fits_agg_dm`. A [data.frame]
#'   of Hellinger distances across IDs and conditions if `object` is of type
#'   `fits_ids_dm`. Hellinger distances are in `[0, 1]`, where `0` means
#'   identical to the reference.
#'
#' @details
#' Under the hood, for each condition, we concatenate the lower- and upper-
#' boundary PDFs (`pdf_l`, `pdf_u`), interpolate the model PDFs to a time space
#' matching with the reference PDFs, and then compute the Hellinger distance:
#' \eqn{H(p,q) = \sqrt{1 - \int \sqrt{p(t)\,q(t)}\,dt}}
#'
#' There are not yet overall, officially published recommendations on how large
#' the Hellinger distance can be without affecting model precision, and this may
#' even depend on the specific model. Based on preliminary simulations, we
#' recommend trying to keep the average Hellinger distance below 5\%.
#'
#' The reference discretizations (`dt_ref/dx_ref`) must be at least as fine as
#' the object's current discretization settings (`dt_model/dx_model`). If
#' `dt_model < dt_ref` or `dx_model < dx_ref`, an error is raised because the
#' “reference” would not be the finest solution.
#'
#' @examples
#' # Example:
#' my_model <- ratcliff_dm(t_max = 1.5, dx = 0.1, dt = 0.005)
#'
#' # Assess current (dt=0.01, dx=0.01) against a fine reference:
#' check_discretization(my_model)
#'
#' # If distances are near zero across conditions, the current grid is adequate.
#'
#' @seealso [estimate_dm()], [trapz()]
#' @export
check_discretization <- function(object, ...) {
  UseMethod("check_discretization")
}

#' @rdname check_discretization
#' @export
check_discretization.drift_dm <- function(object, dt_ref = 0.001,
                                          dx_ref = 0.001, round_digits = 5) {

  drift_dm_obj <- object

  # basic input checks
  stopifnot(is.numeric(dt_ref), length(dt_ref) == 1)
  stopifnot(is.numeric(dx_ref), length(dx_ref) == 1)
  dt_model <- prms_solve(drift_dm_obj)["dt"]
  dx_model <- prms_solve(drift_dm_obj)["dx"]
  t_max = prms_solve(drift_dm_obj)["t_max"]
  if (dt_model < dt_ref) {
    stop(
      "the model's 'dt' is smaller than 'dt_ref'. The reference should be ",
      "finer (smaller dt) than the model."
    )
  }
  if (dx_model < dx_ref) {
    stop(
      "the model's 'dx' is smaller than 'dx_ref'. The reference should be ",
      "finer (smaller dx) than the model."
    )
  }

  # create the common time space
  time_pm = c(seq(-t_max - dt_ref, 0 - dt_ref, dt_ref), seq(0, t_max, dt_ref))

  ###
  # interim: define helper functions to calculate the distance between two
  # pdfs
  hellinger_dist <- function(pdf_a, pdf_b, x) {
    pdf_a <- pdf_a / trapz(x = x, y = pdf_a)
    pdf_b <- pdf_b / trapz(x = x, y = pdf_b)
    dist <- sqrt(max(0.0, 1 - trapz(x = x, sqrt(pdf_a * pdf_b))))
    round(dist, round_digits)
  }

  # paste pdf_u and pdf_l together
  interp_pdf <- function(pdfs_one_cond, dt) {

    # unpack the pdfs
    pdf_u = pdfs_one_cond$pdf_u
    pdf_l = pdfs_one_cond$pdf_l
    stopifnot(length(pdf_u) == length(pdf_l))
    stopifnot(length(pdf_u) == (t_max / dt) + 1 )

    # create new time space (negative and positive)
    x = c(seq(-t_max - dt, 0 - dt, dt), seq(0, t_max, dt))

    # paste the pdfs together and interpolate to common time_space
    pdf <- c(rev(pdf_l), pdf_u)
    pdf <- approx(x = x, y = pdf, xout = time_pm)$y
    return(pdf)
  }

  # wraps around the model, returns the interpolated pdfs per condition as a
  # list
  pdfs_by_dx_dt <- function(model, one_dt = NULL, one_dx = NULL) {
    stopifnot(!xor(is.null(one_dt), is.null(one_dx)))
    if (!is.null(one_dt) & !is.null(one_dx)) {
      prms_solve(model)[c("dt", "dx")] = c(one_dt, one_dx)
    }
    pdfs_per_cond = pdfs(model)$pdfs
    sapply(
      pdfs_per_cond, \(x) interp_pdf(x, prms_solve(model)["dt"]),
      simplify = FALSE, USE.NAMES = TRUE
    )
  }
  ###


  # calculate the reference and model
  pdfs_ref = pdfs_by_dx_dt(model = drift_dm_obj, one_dt = dt_ref, one_dx = dx_ref)
  pdfs_model = pdfs_by_dx_dt(model = drift_dm_obj)

  # iterate over all conditions and calculate the hellinger distance
  conds = names(pdfs_ref)
  hs <- vapply(conds, \(one_cond) {
    hellinger_dist(
      pdf_a = pdfs_ref[[one_cond]],
      pdf_b = pdfs_model[[one_cond]],
      x = time_pm
    )
  }, FUN.VALUE = numeric(1))

  return(hs)
}

#' @rdname check_discretization
#' @export
check_discretization.fits_ids_dm <- function(object, ...) {

  hs <- sapply(object$all_fits, \(x) check_discretization(x, ...))
  hs <- t(hs)
  ids <- rownames(hs)
  hs = cbind(ID = ids, as.data.frame(hs))
  row.names(hs) <- NULL
  hs$ID = try_cast_integer(hs$ID)
  hs = hs[order(hs$ID),]
  return(hs)
}

#' @rdname check_discretization
#' @export
check_discretization.fits_agg_dm <- function(object, ...) {
  check_discretization(object$drift_dm_obj, ...)
}
