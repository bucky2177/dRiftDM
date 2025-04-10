# FUNCTIONS FOR GETTING THE PDF OF A MODEL --------------------------------

#' Calculate the PDFs
#'
#' This method takes a model, the time and space vectors, and the
#' unpacked parameters for solving the PDF to derive the first passage
#' time across all conditions. It is a wrapper around the cpp implementations
#' and [dRiftDM::add_residual]. Important: This function is used in the
#' depths of the package and the generic method is not exported.
#'
#' @param drift_dm_obj a model of type [dRiftDM::drift_dm]
#' @param x_vec numeric vector, the evidence space
#' @param t_vec numeric vector, the time space
#' @param prms_solve the discretization (see [dRiftDM::prms_solve])
#'
#' @returns a list of PDFs, with named entries for each condition. Each of this
#' entry contains a list of vectors, named "pdf_u" and "pdf_l"
#'
#' @details
#' calc_pdfs is a generic method which dispatches the function call (not
#' exported).
#'
#' calc_pdfs.ratcliff_dm, is a specific method that checks for the presence of
#' the parameter `sd_muc`, and, if present, calls the calc_pdfs.drift_dm
#' function multiple times with different value for `muc` to approximate the
#' variable drift rate.
#'
#' calc_pdfs.drift_dm is the function that will be called for all models.
#' It evaluates the different components of a model, and subsequently calls
#' the cpp implementations for the KFE or integral method. It also calls the
#' [dRiftDM::add_residual] function to convolute the non-decision time to the
#' first passage time.
#'
#' The numerical methods for deriving the PDFs are based on the code provided
#' by \insertCite{Richteretal.2023}{dRiftDM}.
#'
#'
#' @seealso [dRiftDM::add_residual]
#'
#' @references
#' \insertRef{Richteretal.2023}{dRiftDM}
#'
#' @keywords internal
calc_pdfs <- function(drift_dm_obj, x_vec, t_vec, prms_solve) {
  UseMethod("calc_pdfs")
}

#' @rdname calc_pdfs
#' @export
calc_pdfs.ratcliff_dm <- function(drift_dm_obj, x_vec, t_vec, prms_solve) {
  # check if variability in drift rate is requested
  prms_matrix <- drift_dm_obj$flex_prms_obj$prms_matrix
  if (all("sd_muc" != colnames(prms_matrix))) {
    return(NextMethod("calc_pdfs", drift_dm_obj))
  }

  # if requested, check for the presence of muc
  if (!any("muc" == colnames(prms_matrix))) {
    stop("parameter sd_muc found, but no parameter muc")
  }

  # and check if constant drift rate was not modified
  if (!isTRUE(all.equal(drift_dm_obj$comp_funs$mu_fun, mu_constant))) {
    stop(
      "Ratcliff DDM with variable drift rate requires dRiftDM's",
      " mu_constant function"
    )
  }

  if (drift_dm_obj$solver == "im_zero" &&
      !isTRUE(all.equal(drift_dm_obj$comp_funs$mu_int_fun, mu_int_constant))) {
    stop(
      "Ratcliff DDM with variable drift rate requires dRiftDM's",
      " mu_int_constant function"
    )
  }




  # do the quadrature
  X <- c(-2.02018287, -0.95857246, 0., 0.95857246, 2.02018287)
  W <- c(0.01995324, 0.39361932, 0.94530872, 0.39361932, 0.01995324)
  wgts <- W / sqrt(3.141592653589793)

  # iterate multiple times over calc_pdfs
  all_pdfs <- lapply(1:length(W), \(q) {
    # update muc
    drift_dm_obj$flex_prms_obj$prms_matrix[, "muc"] <-
      sqrt(2.0) * prms_matrix[, "sd_muc"] * X[q] + prms_matrix[, "muc"]

    # get PDFs
    pdfs <- calc_pdfs.drift_dm(
      drift_dm_obj = drift_dm_obj, x_vec = x_vec,
      t_vec = t_vec, prms_solve = prms_solve
    )
    return(pdfs)
  })

  # add up
  conds <- rownames(prms_matrix)
  pdfs <- sapply(conds, \(one_cond){
    pdf_u <- rowSums(sapply(1:length(all_pdfs), \(idx){
      all_pdfs[[idx]][[one_cond]]$pdf_u * wgts[idx]
    }))

    pdf_l <- rowSums(sapply(1:length(all_pdfs), \(idx){
      all_pdfs[[idx]][[one_cond]]$pdf_l * wgts[idx]
    }))
    return(list(pdf_u = pdf_u, pdf_l = pdf_l))
  }, simplify = FALSE, USE.NAMES = TRUE)

  return(pdfs)
}

#' @rdname calc_pdfs
#' @export
calc_pdfs.drift_dm <- function(drift_dm_obj, x_vec, t_vec, prms_solve) {
  # unpack parameters and conditions
  nt <- prms_solve[["nt"]]
  dt <- prms_solve[["dt"]]
  nx <- prms_solve[["nx"]]
  dx <- prms_solve[["dx"]]
  sigma <- prms_solve[["sigma"]]
  solver <- drift_dm_obj$solver
  prms_matrix <- drift_dm_obj$flex_prms_obj$prms_matrix
  conds <- rownames(prms_matrix)

  # get the component functions
  comp_vals <- comp_vals(
    drift_dm_obj = drift_dm_obj, x_vec = x_vec,
    t_vec = t_vec, nt = nt, dt = dt, nx = nx, dx = dx,
    prms_solve = prms_solve, solver = solver,
    prms_matrix = prms_matrix
  )


  # Second, calculate the pdfs
  pdfs <- sapply(conds, function(one_cond) {
    # unpack component values
    comp_vals_one_cond <- comp_vals[[one_cond]]
    x_vals <- comp_vals_one_cond$x_vals
    b_vals <- comp_vals_one_cond$b_vals
    mu_vals <- comp_vals_one_cond$mu_vals
    mu_int_vals <- comp_vals_one_cond$mu_int_vals
    dt_b_vals <- comp_vals_one_cond$dt_b_vals
    nt_vals <- comp_vals_one_cond$nt_vals

    # Initializing containers
    pdf_u <- numeric(nt + 1)
    pdf_l <- numeric(nt + 1)

    if (solver == "kfe") {
      # solve the pdfs with kfe
      cpp_kfe(
        pdf_u = pdf_u, pdf_l = pdf_l, xx = x_vals,
        nt = nt, nx = nx, dt = dt, dx = dx, sigma = sigma,
        b_vals = b_vals, mu_vals = mu_vals,
        dt_b_vals = dt_b_vals, x_vec = x_vec
      )
    } else if (solver == "im_zero") {
      # solve the pdfs with integral approach
      cpp_imzero(
        pdf_u = pdf_u, pdf_l = pdf_l, nt = nt, dt = dt,
        sigma = sigma, b_vals = b_vals, mu_vals = mu_vals,
        mu_int_vals = mu_int_vals, dt_b_vals = dt_b_vals,
        t_vec = t_vec
      )
    } else {
      stop("solver '", solver, "' not implemented yet!")
    }


    # Omit all states that did not reach a threshold
    scale <- sum(pdf_u) * dt + sum(pdf_l) * dt
    pdf_u <- pdf_u / scale
    pdf_l <- pdf_l / scale

    #  add residual time ...
    pdfs_one_cond <- add_residual(
      pdf_nt = nt_vals, pdf_u = pdf_u, pdf_l = pdf_l, dt = dt, nt = nt
    )

    return(pdfs_one_cond)
  }, simplify = FALSE, USE.NAMES = TRUE)


  return(pdfs)
}


#' Convolute the First Passage Times with the Non-Decision Time Distribution
#'
#' Calls [stats::convolve] for the first passage times and the non-decision
#' time distribution to derive the full distribution of response times. Before
#' convolution, I add the robustness parameter.
#'
#' @param pdf_nt the non-decision time density values
#' @param pdf_u,pdf_l the first passage times
#' @param dt,nt step size and number of steps for the time space (for input
#' checks and scaling)
#'
#' @returns a list of PDFs for one condition "pdf_u" and "pdf_l"
#'
#' @keywords internal
add_residual <- function(pdf_nt, pdf_u, pdf_l, dt, nt) {
  if (length(pdf_nt) != length(pdf_u)) {
    stop("pdf_u and pdf_nt don't have the same length!")
  }
  if (length(pdf_nt) != length(pdf_l)) {
    stop("pdf_l and pdf_nt don't have the same length!")
  }

  if (abs(sum(pdf_nt) * dt - (sum(pdf_l) * dt + sum(pdf_u) * dt)) >
      drift_dm_medium_approx_error()) {
    warning(
      "pdf of the non-dec-time and pdf_l/pdf_u don't integrate to the",
      " same value"
    )
  }


  # convolute
  pdf_u <- stats::convolve(pdf_nt, rev(pdf_u), type = "open") * dt
  pdf_l <- stats::convolve(pdf_nt, rev(pdf_l), type = "open") * dt

  pdf_u <- pdf_u[1:(nt + 1)]
  pdf_l <- pdf_l[1:(nt + 1)]

  stopifnot(length(pdf_u) == length(pdf_nt) & length(pdf_l) == length(pdf_nt))


  # add robustness prm
  pdf_u <- (pdf_u + drift_dm_robust_prm())
  pdf_l <- (pdf_l + drift_dm_robust_prm())


  if (min(pdf_u) < 0 | min(pdf_l) < 0) {
    warning(
      "subst. negative density values encountered when calculating",
      " the pdf"
    )
  }

  return(list(pdf_u = pdf_u, pdf_l = pdf_l))
}




# Functions for calculating the log_likelihood ----------------------------


#' Calculate the Log-Likelihood
#'
#' Wrapper function around `log_like_heart`
#'
#' @param pdfs a list of pdfs (see details)
#' @param t_vec time space
#' @param obs_data a list of obs_data
#' @param conds all conditions of a model
#' @param pdf_u,pdf_l numeric vectors of the pdfs (unpacked)
#' @param rts_u,rts_l numeric vectors of the observed RTs (unpacked)
#'
#' @details
#'
#' ## calc_log_like
#'
#' Iterates over all conditions, and passes forward the (unpacked) arguments
#' to `log_like_heart`, adding each log-likelihood of a condition.
#'
#' `pdfs` must be a list with entries named as the conditions, and then
#' each condition being a list of the two PDFs (named pdf_u and pdf_l)
#'
#' `obs_data` must be a list with entries "rts_u" and "rts_l", and then
#' each rts_* entry being a named list with the RT values for each condition
#'
#' ## log_like_heart
#'
#' Gets the density values for RTs in rts_u/rts_l via [stats::approx()],
#' takes the log of that, and then sums across both.
#' Wraps up the calculation in a tryCatch statement, throwing warnings when
#' log_like_values can not be calculated
#'
#'
#' @returns a single value of the log-likelihood. If no data is provided,
#'   `NULL` is returned. If the calculation fails, `-Inf` is returned.
#'
#' @keywords internal
calc_log_like <- function(pdfs, t_vec, obs_data, conds) {
  if (is.null(obs_data)) {
    return(NULL)
  }

  log_like <- 0

  for (one_cond in conds) {
    log_like <- log_like + log_like_heart(
      pdf_u = pdfs[[one_cond]]$pdf_u,
      pdf_l = pdfs[[one_cond]]$pdf_l,
      t_vec = t_vec,
      rts_u = obs_data$rts_u[[one_cond]],
      rts_l = obs_data$rts_l[[one_cond]]
    )
  }

  return(log_like)
}

#' @rdname calc_log_like
log_like_heart <- function(pdf_u, pdf_l, t_vec, rts_u, rts_l) {
  tryCatch(
    expr = {
      app_like_u <- stats::approx(
        x = t_vec, y = pdf_u,
        xout = rts_u
      )$y
      app_like_l <- stats::approx(
        x = t_vec, y = pdf_l,
        xout = rts_l
      )$y

      log_like <- sum(log(app_like_u)) + sum(log(app_like_l))
      if (is.nan(log_like)) { # log(0) gives -Inf
        if (min(app_like_u) < 0 | min(app_like_l) < 0) {
          warning(
            "negative density values encountered",
            " when calculating the log-likelihood"
          )
        }
        return(-Inf)
      }
      return(log_like)
    },
    error = function(e) {
      warning("we encountered an untreated error!", e)
      return(-Inf)
    }
  )
}
