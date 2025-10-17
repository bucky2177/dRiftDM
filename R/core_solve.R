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
#' exported). Currently, the method only considers objects of type drift_dm.
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
  prm_labels <- colnames(prms_matrix)

  # check if variability in drift rate is requested
  var_drift = FALSE
  if (any("sd_muc" == prm_labels)) {
    var_drift = TRUE

    # and check if constant drift rate is used
    if (!identical(drift_dm_obj$comp_funs$mu_fun, mu_constant)) {
      stop(
        "Ratcliff DDM with variable drift rate requires dRiftDM's",
        " mu_constant function"
      )
    }

    # or its integral
    if (drift_dm_obj$solver == "im_zero" &&
        !identical(drift_dm_obj$comp_funs$mu_int_fun, mu_int_constant)) {
      stop(
        "Ratcliff DDM with variable drift rate requires dRiftDM's",
        " mu_int_constant function"
      )
    }

    # check for plausible values
    if (any(prms_matrix[, "sd_muc"] <= 0)) {
      stop("sd_muc values <= 0 are not allowed")
    }
  }


  # get the component values
  comp_vals <- comp_vals(
    drift_dm_obj = drift_dm_obj, x_vec = x_vec,
    t_vec = t_vec, nt = nt, dt = dt, nx = nx, dx = dx,
    prms_solve = prms_solve, solver = solver,
    prms_matrix = prms_matrix
  )

  # calculate the pdfs
  pdfs <- sapply(conds, function(one_cond) {

    if (var_drift) {
      pdfs_one_cond <- calc_pdfs_var_drift(
        sd_muc_one_cond = prms_matrix[one_cond, "sd_muc"],
        comp_vals_one_cond = comp_vals[[one_cond]],
        nt = nt, nx = nx, dt = dt, dx = dx, sigma = sigma, x_vec = x_vec,
        t_vec = t_vec, solver = solver
      )
    } else {
      pdfs_one_cond <- calc_pdfs_heart(
        comp_vals_one_cond = comp_vals[[one_cond]],
        nt = nt, nx = nx, dt = dt, dx = dx, sigma = sigma, x_vec = x_vec,
        t_vec = t_vec, solver = solver
      )
    }

    return(pdfs_one_cond)
  }, simplify = FALSE, USE.NAMES = TRUE)


  return(pdfs)
}

# outsourced, for easier looping across calc_pdfs_heart
calc_pdfs_var_drift <- function(sd_muc_one_cond, comp_vals_one_cond, ...) {

  # do the quadrature
  X <- c(-2.02018287, -0.95857246, 0., 0.95857246, 2.02018287) * sqrt(2.0)
  W <- c(0.01995324, 0.39361932, 0.94530872, 0.39361932, 0.01995324)
  wgts <- W / sqrt(3.141592653589793)

  # iterate multiple times over calc_pdfs_heart
  all_pdfs <- lapply(X, \(one_x) {

    # create a copy of x_vals (because Thomas' cpp modifies in place)
    copy_comp_vals <- comp_vals_one_cond
    copy_comp_vals$x_vals <- copy_comp_vals$x_vals + 0

    # update muc
    copy_comp_vals$mu_vals <- copy_comp_vals$mu_vals + sd_muc_one_cond * one_x

    # get PDFs
    pdfs <- calc_pdfs_heart(comp_vals_one_cond = copy_comp_vals, ...)
    return(pdfs)
  })

  # add up
  pdf_u <- rowSums(sapply(1:length(all_pdfs), \(idx){
    all_pdfs[[idx]]$pdf_u * wgts[idx]
  }))

  pdf_l <- rowSums(sapply(1:length(all_pdfs), \(idx){
    all_pdfs[[idx]]$pdf_l * wgts[idx]
  }))

  # wrap it up and return
  return(list(pdf_u = pdf_u, pdf_l = pdf_l))
}


# outsourced, for easier looping; prepares a call to the cpp functions
# and the convolution
calc_pdfs_heart <- function(comp_vals_one_cond, nt, nx, dt, dx, sigma, t_vec,
                            x_vec, solver) {

  # unpack component values
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
    cpp_kfe_ada(
      pdf_u = pdf_u, pdf_l = pdf_l, xx = x_vals,
      nt = nt, nx = nx, dtbase = dt, dx = dx, sigma = sigma,
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
  eps <- drift_dm_robust_prm()
  pdf_u <- (pdf_u + eps)
  pdf_l <- (pdf_l + eps)

  # and scale the result
  scale <- sum(pdf_u) * dt + sum(pdf_l) * dt
  pdf_u <- pdf_u / scale
  pdf_l <- pdf_l / scale


  if (min(pdf_u) < 0 | min(pdf_l) < 0) {
    warning(
      "subst. negative density values encountered when calculating",
      " the pdf"
    )
  }

  return(list(pdf_u = pdf_u, pdf_l = pdf_l))
}




