# ===== THE USER FUNCTION FOR CREATING A BASIC MODEL
#' Create a drift_dm object
#'
#' @description
#' This function creates an object of type drift_dm, which serves as the parent
#' class for all further created drift diffusion models. Its structure is the
#' backbone  of the dRiftDM package and every child of the drift_dm class must
#' have the attributes  of the parent class. Typically, users will not want to
#' create an object of drift_dm alone, as its use is very limited. Rather, they
#' will want an object of one of its  child classes. See
#' \code{vignette("use_ddm_models", "dRiftDM")} for a list of pre-built diffusion
#' models and more information on how to create/use/modify child classes.
#'
#' @param prms_model A named numeric vector of the model parameters. The names
#'  indicate the model's parameters, and the numeric entries provide the current
#'  parameter values.
#' @param conds A character vector, giving the names of the model's conditions.
#'  values within `conds` will be used when addressing the data and when
#'  deriving the model's predictions
#' @param free_prms A character vector indicating which of the parameters
#'  defined in `prms_model` are allowed to vary (i.e., can be
#'  estimated/changed). The entries of `free_prms` have to match
#'  `names(prms_model)`. The default (`NULL`) will set
#'  `free_prms <- names(prms_model)`
#' @param obs_data A data.frame, providing a data set
#'  (see [dRiftDM::set_obs_data])
#' @param sigma The diffusion constant. Default is set to 1.
#' @param t_max The maximum of the time space. Default is set to 3 (seconds).
#' @param dt The step size of the time discretization. Default is set to .001
#'  (seconds).
#' @param dx The step size of the evidence space discretization. Default is set
#'  to .001.
#' @param mu_fun,mu_int_fun,x_fun,b_fun,dt_b_fun,nt_fun custom functions
#'  defining the components of a diffusion model. See the respective
#'  `set_comp_funs` function.
#' @param b_encoding, a list, specifying how boundaries are coded.
#'  The default is 'accuracy' encoding, see the default of
#'  [dRiftDM::set_b_encoding].
#'
#' @returns A list with the class label "drift_dm". In general, it is not
#' recommended to directly modify the entries of this list. Users should use the
#' built-in setter functions (e.g., [dRiftDM::set_model_prms]);
#' see \code{vignette("use_ddm_models", "dRiftDM")} for more
#' information. The list contains the following entries:
#'
#' * The named numeric vector `prms_model`
#' * The character vector `conds`
#' * Parameters used for deriving the model predictions, `prms_solve`,
#'  containing the diffusion constant (`sigma`), the maximum of the time space
#'  (`t_max`), the steps of time and evidence space (`dt` and `dx`,
#'  respectively), and the number of steps in the time and evidence space
#'  (`nt` and `nx`, respectively).
#' * The character vector `free_prms`
#' * A label indicating the method for deriving the model predictions
#'  (currently only `kfe`).
#' * A list of functions called `comp_funs`, providing the components of the
#'  diffusion model (i.e., `mu_fun`, `mu_int_fun`, `x_fun`, `b_fun`,
#'  `dt_b_fun`, `nt_fun`). These functions are called in the depths of the
#'  package and will determine the behavior of the model
#'
#'
#'  If observed data were passed viat [dRiftDM::set_obs_data],
#'  the list will contain an entry
#'  called `obs_data`.
#'
#'  If the model has been evaluated (see [dRiftDM::re_evaluate_model]), the
#'  list will additionally contain...
#'
#' * ... the log likelihood;can be addressed via `drift_dm_obj$log_like_val`
#' * ... the aic/bic values; can be addressed via `drift_dm_obj$ic_vals`
#'
#' @details
#'
#' Please visit the \code{vignette("use_ddm_models", "dRiftDM")} for more in-depth
#' information on how to modify an object of type drift_dm.
#'
#'
#' @export
drift_dm <- function(prms_model, conds, free_prms = NULL, obs_data = NULL,
                     sigma = 1, t_max = 3, dt = .001, dx = .001,
                     mu_fun = NULL, mu_int_fun = NULL, x_fun = NULL,
                     b_fun = NULL, dt_b_fun = NULL, nt_fun = NULL,
                     b_encoding = NULL) {
  # conduct input checks and set defaults
  if (length(prms_model) == 0) {
    stop("prms_model has length 0")
  }
  check_if_named_numeric_vector(x = prms_model, var_name = "prms_model")
  if (!is.character(conds) | length(conds) == 0) {
    stop("conds is not a character vector of length >= 1")
  }
  if (!is.numeric(sigma) | length(sigma) != 1) {
    stop("sigma is not a single numeric number")
  }
  if (!is.numeric(t_max) | length(t_max) != 1) {
    stop("t_max is not a single numeric number")
  }
  if (!is.numeric(dt) | length(dt) != 1) {
    stop("dt is not a single numeric number")
  }
  if (!is.numeric(dx) | length(dx) != 1) {
    stop("dx is not a single numeric number")
  }

  prms_solve <- c("sigma" = sigma, "t_max" = t_max, "dt" = dt, "dx" = dx)

  if (is.null(free_prms)) {
    free_prms <- names(prms_model)
  } else {
    if (!is.character(free_prms) | length(free_prms) > length(prms_model)) {
      stop(
        "free_prms is not a character vector with fewer elements",
        " than prms_model"
      )
    }

    if (!all(free_prms %in% names(prms_model))) {
      stop("free_prms do not match the names in prms_model")
    }
    free_prms <- names(prms_model)[names(prms_model) %in% free_prms]
  }

  # get default functions, if necessary
  comp_funs <- get_default_functions(
    mu_fun = mu_fun, mu_int_fun = mu_int_fun, x_fun = x_fun, b_fun = b_fun,
    dt_b_fun = dt_b_fun, nt_fun = nt_fun
  )


  # pass the arguments further down
  drift_dm_obj <- new_drift_dm(
    prms_model = prms_model, conds = conds,
    free_prms = free_prms,
    prms_solve = prms_solve,
    obs_data = obs_data,
    comp_funs = comp_funs,
    b_encoding = b_encoding
  )

  # validate the model to ensure everything is as expected and pass back
  drift_dm_obj <- validate_drift_dm(drift_dm_obj)
  return(drift_dm_obj)
}


# ====== BACKEND FUNCTION FOR CREATING A DRIFT_DM OBJECT
new_drift_dm <- function(prms_model, conds, free_prms,
                         prms_solve, obs_data, comp_funs, b_encoding) {
  # calculate the number of discretization steps
  prms_solve["nt"] <- as.integer(
    prms_solve[["t_max"]] / prms_solve[["dt"]] + 1.e-8
  )
  prms_solve["nx"] <- as.integer(2 / prms_solve["dx"] + 1.e-8)


  # add everything
  drift_dm_obj <- list(
    prms_model = prms_model, conds = conds, free_prms = free_prms,
    prms_solve = prms_solve,
    solver = "kfe", comp_funs = comp_funs
  )
  class(drift_dm_obj) <- "drift_dm"

  # set encoding
  drift_dm_obj <- set_b_encoding(
    drift_dm_obj = drift_dm_obj,
    b_encoding
  )

  # add data if necessary
  if (!is.null(obs_data)) {
    drift_dm_obj <- set_obs_data(
      drift_dm_obj = drift_dm_obj,
      obs_data = obs_data
    )
  }

  # return
  return(drift_dm_obj)
}

# ======== BACKEND FUNCTION CHECKS ON EACH DRIFT_DM OBJECT
validate_drift_dm <- function(drift_dm_obj) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }

  # check the prms_model entry
  if (length(drift_dm_obj$prms_model) == 0) {
    stop("prms_model has length 0")
  }
  check_if_named_numeric_vector(
    x = drift_dm_obj$prms_model,
    var_name = "drift_dm_obj$prms_model"
  )

  if (anyDuplicated(names(drift_dm_obj$prms_model)) > 0) {
    stop("there are duplicate entries in prms_model")
  }

  # check the conditions entry
  if (!is.character(drift_dm_obj$conds) | length(drift_dm_obj$conds) == 0) {
    stop("conds in drift_dm_obj is not a character vector of length >= 1")
  }

  # check the prms_solve entry
  check_if_named_numeric_vector(
    x = drift_dm_obj$prms_solve,
    var_name = "prms_solve",
    labels = c("sigma", "t_max", "dt", "dx", "nx", "nt"),
    length = 6
  )

  # check the free prms entry
  if (!is.character(drift_dm_obj$free_prms) |
    length(drift_dm_obj$free_prms) == 0) {
    stop("free_prms in drift_dm_obj is not a character vector of length >= 1")
  }


  if (!all(drift_dm_obj$free_prms %in% names(drift_dm_obj$prms_model))) {
    stop("free_prms do not match prms_model in drift_dm_obj")
  }

  if (anyDuplicated((drift_dm_obj$free_prms)) > 0) {
    stop("there are duplicate entries in free_prms")
  }

  subs <- names(drift_dm_obj$prms_model)[names(drift_dm_obj$prms_model)
  %in% drift_dm_obj$free_prms]
  if (any(subs != drift_dm_obj$free_prms)) {
    warning(
      "free_prms are not ordered according to prms_model.",
      " Automatically fixing this"
    )
    drift_dm_obj$free_prms <- subs
  }

  # check the entries of prms_solve
  prms_solve <- drift_dm_obj$prms_solve # for less intricate code
  if (prms_solve[["sigma"]] <= 0) stop("sigma in prms_model must be positive")
  if (prms_solve[["t_max"]] <= 0) stop("t_max in prms_model must be positive")
  if (prms_solve[["dt"]] <= 0) stop("dt in prms_model must be positive")
  if (prms_solve[["dx"]] <= 0) stop("dx in prms_model must be positive")
  if (prms_solve[["nt"]] <= 0) stop("nt in prms_model must be positive")
  if (prms_solve[["nx"]] <= 0) stop("nx in prms_model must be positive")
  if (abs(prms_solve[["nx"]] - as.integer(prms_solve[["nx"]])) != 0) {
    stop("nx must not have decimal places")
  }
  if (abs(prms_solve[["nt"]] - as.integer(prms_solve[["nt"]])) != 0) {
    stop("nt must not have decimal places")
  }
  if (abs(prms_solve[["dt"]] * prms_solve[["nt"]] - prms_solve[["t_max"]])
  >= drift_dm_approx_error()) {
    stop("Final timeline not nt times dt. Check the dt and t_max values!")
  }
  if (abs(prms_solve[["dx"]] * prms_solve[["nx"]] - 2) >=
    drift_dm_approx_error()) {
    stop("dx times nx is not 2. Check if 2 / dx provides an integer!")
  }

  if (prms_solve[["nx"]] <= 10) {
    warning("nx seems very small. Double check your model")
  }
  if (prms_solve[["nt"]] <= 10) {
    warning("nt seems very small. Double check your model")
  }

  # in case the max RT is larger than expected, adjust the prms_solve
  if (!is.null(drift_dm_obj$obs_data)) {
    max_rt <- max(unlist(drift_dm_obj$obs_data))
    if (max_rt > drift_dm_obj$prms_solve[["t_max"]]) {
      warning(
        "RTs in obs_data are larger than the maximum time in prms_solve ",
        "Trying to fix this by adjusting t_max and nt. Please double-check ",
        "your data and your model!"
      )

      prms_solve <- drift_dm_obj$prms_solve
      prms_solve[["nt"]] <- ceiling(max_rt / prms_solve[["dt"]])
      prms_solve[["t_max"]] <- prms_solve[["nt"]] * prms_solve[["dt"]]
      drift_dm_obj$prms_solve <- prms_solve
    }
  }

  # check if the solver entry is just a single string
  if (!is.character(drift_dm_obj$solver) | length(drift_dm_obj$solver) != 1) {
    stop("solver in drift_dm_obj is not a single character/string")
  }

  # ensure that each element in comp_funs is a function with the correct
  # arguments
  names <- names(drift_dm_obj$comp_funs)
  nec_names <- c("mu_fun", "mu_int_fun", "x_fun", "b_fun", "dt_b_fun", "nt_fun")
  if (any(names != nec_names)) stop("unexpected name in comp_funs")
  for (one_name in names) {
    if (!is.function(drift_dm_obj$comp_funs[[one_name]])) {
      stop(one_name, " listed in comp_funs is not a function")
    }

    arg_names <- names(as.list(args(drift_dm_obj$comp_funs[[one_name]])))


    if (arg_names[[1]] != "prms_model") {
      stop("the first argument of ", one_name, " must be 'prms_model'")
    }

    if (arg_names[[2]] != "prms_solve") {
      stop("the second argument of ", one_name, " must be 'prms_solve'")
    }

    if (one_name != "x_fun" & arg_names[[3]] != "t_vec") {
      stop("the third argument of ", one_name, " must be 't_vec'")
    }

    if (one_name == "x_fun" & arg_names[[3]] != "x_vec") {
      stop("the third argument of ", one_name, " must be 'x_vec'")
    }

    if (arg_names[[4]] != "one_cond") {
      stop("the fourth argument of ", one_name, " must be 'one_cond'")
    }

    if (arg_names[[5]] != "ddm_opts") {
      stop("the fifth argument of ", one_name, " must be 'ddm_opts'")
    }
  }

  # check pdfs
  if (!is.null(drift_dm_obj$pdfs)) {
    if (any(names(drift_dm_obj$pdfs) != drift_dm_obj$conds)) {
      stop("the pdf entry of drift_dm_obj is not labeled like the conditions")
    }

    check <- sapply(drift_dm_obj$pdfs, function(x) {
      names_check <- all(names(x) == c("pdf_u", "pdf_l"))
      length_check <-
        as.vector(sapply(x, length)) == drift_dm_obj$prms_solve[["nt"]] + 1
      length_check <- all(length_check)
      numeric_check <- all(as.vector(sapply(x, is.numeric)))
      return(list(names_check, length_check, numeric_check))
    })
    if (!all(unlist(check[1, ]))) {
      stop("a pdf by condition entry is not named pdf_u or pdf_l")
    }
    if (!all(unlist(check[2, ]))) {
      stop("one of the pdf vectors has not the expected size")
    }

    if (!all(unlist(check[3, ]))) {
      stop("one of the pdf vectors is not of type numeric")
    }
  }

  # check log_like
  if (!is.null(drift_dm_obj$log_like_val)) {
    if (!is.numeric(drift_dm_obj$log_like_val) |
      length(drift_dm_obj$log_like_val) != 1) {
      stop("log_like val in drift_dm_obj is not a single numeric")
    }
  }

  # check aic/bic
  if (!is.null(drift_dm_obj$ic_vals)) {
    check_if_named_numeric_vector(
      x = drift_dm_obj$ic_vals, var_name = "ic_vals",
      labels = c("aic", "bic"), length = 2
    )
  }


  # check boundary encoding
  # check encoding
  b_encoding <- attr(drift_dm_obj, "b_encoding")
  if (!is.character(b_encoding$column) | length(b_encoding$column) != 1) {
    stop("b_encoding_column is not a single character")
  }

  if (class(b_encoding$u_name_value) != class(b_encoding$l_name_value)) {
    stop("u_name_value and l_name_value in b_encoding are not of the same type")
  }

  if (length(b_encoding$u_name_value) != 1 | length(b_encoding$l_name_value) != 1) {
    stop("u_name_value or l_name_value in b_encoding are not of length 1")
  }
  names_u <- names(b_encoding$u_name_value)
  if (is.null(names_u)) {
    stop("u_name_value in b_encoding is not a named vector")
  }

  names_l <- names(b_encoding$l_name_value)
  if (is.null(names_l)) {
    stop("l_name_value in b_encoding is not a named vector")
  }



  return(drift_dm_obj)
}



# =======DEFAULT FUNCTIONS FOR THE DIFFERENT COMPONENTS OF A DDM

standard_drift <- function() {
  return(3)
}
standard_boundary <- function() {
  return(0.5)
}
standard_nt <- function() {
  return(0.3)
}


get_default_functions <- function(mu_fun = NULL, mu_int_fun = NULL,
                                  x_fun = NULL, b_fun = NULL,
                                  dt_b_fun = NULL, nt_fun = NULL) {
  if (is.null(mu_fun)) {
    mu_fun <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
      # a constant drift rate
      mu <- standard_drift()
      if (!is.numeric(t_vec) | length(t_vec) <= 1) {
        stop("t_vec is not a vector")
      }
      mu <- rep(mu, length(t_vec))
      return(mu)
    }
  }

  if (is.null(mu_int_fun)) {
    mu_int_fun <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
      # integral of a constant drift rate
      mu <- standard_drift()
      if (!is.numeric(t_vec) | length(t_vec) <= 1) {
        stop("t_vec is not a vector")
      }
      return(mu * t_vec)
    }
  }

  if (is.null(x_fun)) {
    x_fun <- x_dirac_0
  }

  if (is.null(b_fun)) {
    b_fun <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
      # constant boundary
      b <- standard_boundary()
      if (!is.numeric(t_vec) | length(t_vec) <= 1) {
        stop("t_vec is not a vector")
      }
      b <- rep(b, length(t_vec))
      return(b)
    }
  }

  if (is.null(dt_b_fun)) {
    dt_b_fun <- dt_b_constant
  }


  if (is.null(nt_fun)) {
    nt_fun <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
      non_dec_time <- standard_nt()
      if (non_dec_time < 0 | non_dec_time > prms_solve[["t_max"]]) {
        stop("non_dec_time larger than t_max or smaller than 0!")
      }
      if (!is.numeric(t_vec) | length(t_vec) <= 1) {
        stop("t_vec is not a vector")
      }
      dt <- prms_solve[["dt"]]
      d_nt <- numeric(length(t_vec))
      which_index <- as.integer(non_dec_time / dt)
      d_nt[which_index + 1] <- 1 / dt
      return(d_nt)
    }
  }

  return(
    list(
      mu_fun = mu_fun, mu_int_fun = mu_int_fun, x_fun = x_fun,
      b_fun = b_fun, dt_b_fun = dt_b_fun, nt_fun = nt_fun
    )
  )
}





# ===== FUNCTIONS FOR GETTING THE INFORMATION and FITTING CRITERIA

# prms: ll -> log_like_val, k = number of prms, n = number of observed data
# points
calc_ic <- function(ll, k, n) {
  aic <- 2 * k - 2 * ll
  bic <- k * log(n) - 2 * ll

  return(c(aic = aic, bic = bic))
}



# ===== FUNCTION FOR ENSURING EVERYTHING IS UP-TO-DATE

#' Re-evaluate the model
#'
#' Updates the pdfs of model. If data is set to the model, the log likelihood,
#' the Akaike information criterion (aic) and the
#' Bayesian information criterion (bic) also updated
#'
#' @param drift_dm_obj an object inheriting from [dRiftDM::drift_dm]
#' @param eval_model logical, indicating if the model should be evaluated or not.
#'  If `False` pdfs, log_like_val and ic_vals are deleted from the model
#'  (used in the internals of dRiftDM)
#'
#' @returns Returns the passed `drift_dm_obj` object, after (re-)calculating
#' the pdfs, log likelihood (and aic/bic) of the model.
#'
#' * the pdfs an be addressed via `drift_dm_obj$pdfs`
#' * the log likelihood can be addressed via `drift_dm_obj$log_like_val`
#' * the aic/bic values can be addressed via `drift_dm_obj$ic_vals`
#'
#' Note that if re_evaluate model is called before observed data was set,
#' the function silently updates the `pdfs`, but not `log_like_val` or
#' `ic_vals`. More in-depth information about the mathematical details for
#' deriving the PDFs can be found in
#' \insertCite{Richteretal.2023;textual}{dRiftDM}
#'
#' @export
re_evaluate_model <- function(drift_dm_obj, eval_model = T) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }

  # set all fit indices and the pdfs to NULL
  drift_dm_obj$log_like_val <- NULL
  drift_dm_obj$ic_vals <- NULL
  drift_dm_obj$pdfs <- NULL

  # pass back if no evaluation is requested
  if (!eval_model) {
    return(drift_dm_obj)
  }

  # First evaluate all component functions
  # unpack values and create time and evidence vector
  t_max <- drift_dm_obj$prms_solve[["t_max"]]
  nt <- drift_dm_obj$prms_solve[["nt"]]
  dt <- drift_dm_obj$prms_solve[["dt"]]

  dx <- drift_dm_obj$prms_solve[["dx"]]
  nx <- drift_dm_obj$prms_solve[["nx"]]

  x_vec <- seq(-1, 1, length.out = nx + 1)
  t_vec <- seq(0, t_max, length.out = nt + 1)

  if (drift_dm_obj$solver == "kfe") {
    comp_fun_names <- c("mu_fun", "x_fun", "b_fun", "dt_b_fun", "nt_fun")
  } else {
    stop("not implemented yet another solver")
  }

  all_comp_vecs <- sapply(drift_dm_obj$conds, function(one_cond) {
    one_set_comp_vecs <- sapply(comp_fun_names, function(name_comp_fun) {
      if (name_comp_fun == "x_fun") {
        vals <- drift_dm_obj$comp_funs[[name_comp_fun]](drift_dm_obj$prms_model,
          drift_dm_obj$prms_solve,
          x_vec,
          one_cond,
          drift_dm_obj$ddm_opts)

        if (any(is.infinite(vals)) | any(is.na(vals))) {
          stop("x_fun provided infinite values or NAs, condition ", one_cond)
        }
        if (min(vals) < 0) {
          stop("x_fun provided negative values")
        }
        if (abs(sum(vals) * dx - 1) > drift_dm_small_approx_error()) {
          stop("starting condition doesn't integrate to 1, condition ", one_cond)
        }
        if (length(vals) != nx + 1) {
          stop("unexpected length of x_vals, condition ", one_cond)
        }
      } else {
        if (name_comp_fun == "mu_int_fun" & drift_dm_obj$solver == "kfe") {
          return(NULL)
        }
        vals <- drift_dm_obj$comp_funs[[name_comp_fun]](drift_dm_obj$prms_model,
          drift_dm_obj$prms_solve,
          t_vec,
          one_cond,
          drift_dm_obj$ddm_opts)
        if (any(is.infinite(vals)) | any(is.na(vals))) {
          stop(
            "function for ", name_comp_fun,
            " provided infinite values or NAs, condition ", one_cond
          )
        }
        if (length(vals) != nt + 1) {
          stop(
            "function for ", name_comp_fun,
            " provided a vector of unexpected length, condition ", one_cond
          )
        }
      }

      if (name_comp_fun == "nt_fun") {
        if (min(vals) < 0) {
          stop("pdf_nt provided negative values, condition ", one_cond)
        }
        if (abs(sum(vals) * dt - 1) > drift_dm_small_approx_error()) {
          stop("pdf_nt doesn't integrate to 1, condition ", one_cond, drift_dm_obj$prms_model)
        }
      }
      return(vals)
    }, USE.NAMES = T, simplify = F)
    names(one_set_comp_vecs) <- sub(
      pattern = "fun", replacement = c("vals"),
      x = names(one_set_comp_vecs)
    )
    return(one_set_comp_vecs)
  }, USE.NAMES = T, simplify = F)


  # Second, calculate the pdfs
  pdfs <-
    sapply(drift_dm_obj$conds, function(one_cond) {
      calc_pdfs(
        solver = drift_dm_obj$solver, x_vec = x_vec, t_vec = t_vec,
        prms_solve = drift_dm_obj$prms_solve,
        one_set_comp_vecs = all_comp_vecs[[one_cond]], one_cond = one_cond
      )
    }, simplify = F, USE.NAMES = T)
  drift_dm_obj$pdfs <- pdfs



  # return if no data is supplied, so log_like and ic_vals are not updated
  if (is.null(drift_dm_obj$obs_data)) {
    return(drift_dm_obj)
  }

  # update log_like_val and ic_vals
  log_like_val <- calc_log_like(
    pdfs = pdfs, t_vec = t_vec,
    obs_data = drift_dm_obj$obs_data,
    conds = drift_dm_obj$conds
  )

  ic_vals <- calc_ic(
    ll = log_like_val,
    k = length(drift_dm_obj$free_prms),
    n = length(unlist(drift_dm_obj$obs_data))
  )


  # attach all and pass back
  drift_dm_obj$log_like_val <- log_like_val
  drift_dm_obj$ic_vals <- ic_vals

  return(drift_dm_obj)
}

# ===== FUNCITONS FOR SETTING THINGS

#' Setting attributes/components of a drift_dm model
#'
#' @description Functions starting with `set_*` provide ways for modifying the
#' list underlying every object inheriting from `drift_dm`. Using the setter
#' methods is the highly recommended way of changing a model
#' (see [dRiftDM::drift_dm] for a list of the built-in attributes).
#'
#' * `set_model_prms` for setting the parameters of the model
#'
#' * `set_free_prms` for declaring which parameters are "free"  or "fixed"
#'
#' * `set_solver_settings` for modifying the settings relevant to the functions
#'  deriving the pdfs of the diffusion model
#'
#' * `set_obs_data` can be used to pass/set observed data
#'
#' * `set_comp_funs` can be used to pass/set component functions of the model
#'
#' * `set_b_encoding` is used to specify how a model's boundary shall be labeled
#'
#' @param drift_dm_obj an object inheriting from [dRiftDM::drift_dm].
#' @param new_prm_vals a named numeric vector, specifying new values
#' for the model's parameters listed in `drift_dm_obj$prms_model`
#' @param replace logical, specific for `set_model_prms()`, entirely replaces
#' old parameters with `new_prm_vals`. In this case, `free_prms` is updated
#' as well (so that all new parameters are considered free).
#' @param new_free_prms,new_fixed_prms a character vector specifying the names
#'  of the parameters that are either allowed to vary or which are fixed. When
#'  calling `set_free_prms`, users can only specify `new_free_prms` or
#'  `new_fixed_prms`, not both. In addition, when using `new_fixed_prms`
#'  all parameters not listed are automatically assumed to be free.
#' @param new_solver_vals a named vector specifying settings relevant for
#'  numerically solving  a model. Labels for each entry must match with
#'  `solver`, `sigma`, `t_max`, `dt` or `dx` (Note that `solver` can only be
#'  `kfe` at the moment).
#' @param obs_data a [data.frame] which provides three columns: (1) `RT` for
#'  the response times, (2) a column for boundary coding according `b_encoding`
#'  below, (3) `Cond` for specifying the conditions (see
#'  \code{vignette("use_ddm_models", "dRiftDM")} for more information).
#'
#' @param comp_funs list with named entries, containing component functions
#'  to set (see the details below).
#' @param eval_model logical, indicating whether [dRiftDM::re_evaluate_model]
#'  should be called after modifying the model. Default is `FALSE`. Note that if
#'  `eval_model` is set to `FALSE`, the attributes `pdfs`, `log_like_val`,
#'  and `ic_vals` are deleted from the model.
#'
#' @param b_encoding, a list, specifying how boundaries are coded.
#'  The default `NULL` will internally result to 'accuracy' encoding:
#' `list(column = "Error", u_name_value = c("correct" = 0), l_name_value = c("error" = 1))`.
#' This means that `obs_data` (if provided) must contain a column "Error"
#' with values 0 and 1 referring to the upper and lower boundary, respectively.
#'
#' @details
#'
#' Please visit the \code{vignette("use_ddm_models", "dRiftDM")} for more in-depth
#' information on how to modify an object of type drift_dm.
#'
#' `mu_fun` and `mu_int_fun` provide the drift rate and its integral,
#' respectively, across the time space.
#'
#' `x_fun` provides a distribution of the starting point across the evidence
#' space.
#'
#' `b_fun` and `dt_b_fun` provide the values of the (symmetric) boundary and its
#' derivative, respectively, across the time space.
#'
#' `nt_fun` provides a distribution of the non-decision component across the
#' time space.
#'
#' All of the listed functions are stored in the list `comp_funs` of the
#' respective model.
#'
#' Each component function must take the model parameters, the parameters
#' relevant for deriving the model's pdfs, the time or evidence
#' space, a condition, and a list of optional values as arguments.
#' These arguments are provided with values
#' when dRiftDM internally calls them.
#'
#' In order to work with `dRiftDM`, `mu_fun`,
#' `mu_int_fun`, `b_fun`, `dt_b_fun`, `nt_fun` must have the following
#' declaration:
#' `my_fun = function(prms_model, prms_solve, t_vec, one_cond, ddm_opts`). Here,
#' `prms_model` are the model parameters, `prms_solve` the parameters relevant
#' for dericing the model's pdfs, `t_vec` is
#' the time space, going from 0 to `t_max` with length `nt + 1` (see
#' [dRiftDM::drift_dm]), `one_cond` is of type character, indicating the
#' current condition. Finally `dmm_opts` may contain additional values.
#' Each function must return a numeric vector
#' of the same length as `t_vec`. For `mu_fun`,
#' `mu_int_fun`, `b_fun`, `dt_b_fun` the returned values provide the
#' respective boundary/drift rate (and their derivative/integral) at every time
#' step \eqn{t}. For `nt_fun` the returned values provide the density of the
#' non-decision time across the time space (which get convoluted with the
#' pdfs when solving the model)
#'
#' In order to work with `dRiftDM`, `x_fun` must have the following
#' declaration:
#' `my_fun = function(prms_model, prms_solve, x_vec, one_cond, ddm_opts`).
#' Here, `x_vec` is
#' the evidence space, going from -1 to 1 with length `nx + 1` (see
#' [dRiftDM::drift_dm]). Each function must return a numeric vector
#' of the same length as `x_vec`, providing the density values of the
#' starting points across the evidence space.
#'
#' ## Drift rate and its integral:
#'
#' The drift rate is the first derivative of the expected time-course
#' of the diffusion process. For instance, if we assume that the diffusion
#' process \eqn{X} is linear with a slope of \eqn{v}...
#' \deqn{E(X) = v \cdot t}
#' ...then the drift rate at every time step \eqn{t} is the constant \eqn{v},
#' obtained by taking the derivative of the expected time-course with respect
#' to \eqn{t}:
#' \deqn{\mu(t) = v}
#' Conversely, the integral of the drift rate is identical to the expected
#' time-course:
#' \deqn{\mu_{int}(t) = v \cdot t}
#'
#' For the drift rate `mu_fun`, the default function when calling `drift_dm()`
#' is a numeric vector containing the number \eqn{3}. Its integral counterpart
#' `mu_int_fun` will return a numeric vector containing the values `t_vec*3`.
#'
#' ## Starting Point Distribution:
#'
#' The starting point of a diffusion model refers to the initial value taken
#' by the evidence accumulation process at time \eqn{t=0}. This is a pdf
#' over the evidence space.
#'
#' The default function when calling `drift_dm()` will be a function
#' returning a dirac delta on zero, meaning that every potential diffusion
#' process starts at 0.
#'
#' ## Boundary:
#'
#' The Boundary refers to the values of the absorbing boundaries at every time
#' step \eqn{t} in a diffusion model. In most cases, this will be a constant.
#' For instance:
#' \deqn{b(t) = b}
#' In this case, its derivative with respect to \eqn{t} is 0.
#'
#' The default function when calling `drift_dm()` will be function for `b_fun`
#' returning a  numeric vector of length `length(t_vec)` containing the number
#' \eqn{0.5}. Its counterpart `dt_b` will return a numeric vector of the same
#' length containing its derivative, namely, `0`.
#'
#' ## Non-Decision Time:
#'
#' The non-decision time refers to an additional time-requirement. Its
#' distribution across the time space will be convoluted with the pdfs derived
#' from the diffusion process.
#'
#' In psychology, the non-decision time captures time-requirements outside the
#' central decision process, such as stimulus perception and motor execution.
#'
#' The default function when calling `drift_dm()` returns a dirac
#' delta, shifted to \eqn{t = 0.3}.
#'
#' @returns Returns the modified `drift_dm_obj` object.
#'
#' @export
set_model_prms <- function(drift_dm_obj, new_prm_vals,
                           replace = F, eval_model = F) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }
  if (!is.logical(eval_model) | length(eval_model) != 1) {
    stop("eval_model must be logical")
  }
  if (!is.logical(replace) | length(replace) != 1) {
    stop("replace must be logical")
  }

  check_if_named_numeric_vector(x = new_prm_vals, var_name = "new_prm_vals")

  if (replace == T) {
    drift_dm_obj$prms_model <- new_prm_vals
    drift_dm_obj$free_prms <- names(new_prm_vals)
  } else {
    if (!all(names(new_prm_vals) %in% names(drift_dm_obj$prms_model))) {
      stop("the names specified in new_prm_vals don't match the model's parameters")
    }

    drift_dm_obj$prms_model[names(new_prm_vals)] <- new_prm_vals
  }

  # ensure that everything is up-to-date (or skip)
  drift_dm_obj <- re_evaluate_model(
    drift_dm_obj = drift_dm_obj,
    eval_model = eval_model
  )
  # ensure that nothing went wrong
  drift_dm_obj <- validate_drift_dm(drift_dm_obj)

  return(drift_dm_obj)
}

#' @rdname set_model_prms
#' @export
set_free_prms <- function(drift_dm_obj, new_free_prms = NULL,
                          new_fixed_prms = NULL) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }

  if (is.null(new_free_prms) & is.null(new_fixed_prms)) {
    stop(
      "Neither new_free_prms nor new_fixed_prms specified. ",
      "Please specify one of the two"
    )
  }

  if (!is.null(new_free_prms) & !is.null(new_fixed_prms)) {
    stop(
      "Both new_free_prms and new_fixed_prms were specified. ",
      "Only one of the two is allowed"
    )
  }

  if (!is.null(new_free_prms)) {
    set_free <- T
    input_prms <- new_free_prms
  } else {
    set_free <- F
    input_prms <- new_fixed_prms
  }

  # match inputs
  name_prms_model <- names(drift_dm_obj$prms_model)
  matched_prms <- sapply(input_prms, function(x) {
    match.arg(x, name_prms_model)
  })
  matched_prms <- unname(matched_prms)
  if (any(matched_prms != input_prms)) {
    warning(
      "Some of the provided parameter names did not match the parameters",
      " of the model. Automatically corrected.",
      " Please Double check the result"
    )
  }

  # ensure ordering and non-duplicates when setting the free parameters
  matched_prms <- name_prms_model[name_prms_model %in% matched_prms]
  if (set_free) {
    drift_dm_obj$free_prms <- matched_prms
  } else {
    drift_dm_obj$free_prms <-
      name_prms_model[!(name_prms_model %in% matched_prms)]
  }

  # check model and return
  drift_dm_obj <- validate_drift_dm(drift_dm_obj)
  return(drift_dm_obj)
}



#' @rdname set_model_prms
#' @export
set_solver_settings <- function(drift_dm_obj, new_solver_vals,
                                eval_model = F) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }

  names_prm_solve <- names(new_solver_vals)
  values_prm_solve <- unname(new_solver_vals)

  if (is.null(names_prm_solve)) {
    stop("values_prm_solve must be a named vector")
  }

  matched_names <- sapply(names_prm_solve, function(x) {
    match.arg(x, c("solver", "sigma", "t_max", "dx", "dt"))
  })
  matched_names <- unname(matched_names)
  if (any(matched_names != names_prm_solve)) {
    warning(
      "Some of the labels in values_prm_solve only partially matched with",
      " 'solver', 'dx', 'dt', 't_max', 'sigma'. Automatically corrected.",
      " Please Double check the result"
    )
  }

  if (!is.numeric(values_prm_solve) & !is.character(values_prm_solve)) {
    stop("values_prm_solve must be either of type numeric or character")
  }

  # set all desired arguments one by one
  for (i in seq_along(names_prm_solve)) {
    drift_dm_obj <- set_one_solver_setting(
      drift_dm_obj = drift_dm_obj,
      name_prm_solve = names_prm_solve[i],
      value_prm_solve = values_prm_solve[i]
    )
  }


  # ensure that everything is up-to-date (or skip)
  drift_dm_obj <- re_evaluate_model(
    drift_dm_obj = drift_dm_obj,
    eval_model = eval_model
  )

  # ensure that nothing went wrong
  drift_dm_obj <- validate_drift_dm(drift_dm_obj)

  return(drift_dm_obj)
}

# separate function for setting one argument (internal)
set_one_solver_setting <- function(drift_dm_obj, name_prm_solve,
                                   value_prm_solve) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }

  # if desired, set solver
  if (name_prm_solve == "solver") {
    value_prm_solve <- as.character(value_prm_solve)
    if (length(value_prm_solve) != 1) {
      stop("solver argument must be a single character")
    }
    drift_dm_obj$solver <- value_prm_solve
  }

  # if desired, set sigma
  if (name_prm_solve == "sigma") {
    value_prm_solve <- as.numeric(value_prm_solve)
    if (length(value_prm_solve) != 1) {
      stop("sigma argument must be a single numeric")
    }
    drift_dm_obj$prms_solve[["sigma"]] <- value_prm_solve
  }


  # if desired, set t_max or dt
  if (name_prm_solve == "t_max" | name_prm_solve == "dt") {
    value_prm_solve <- as.numeric(value_prm_solve)
    if (length(value_prm_solve) != 1) {
      stop(name_prm_solve, "argument must be a single numeric")
    }

    prms_solve <- drift_dm_obj$prms_solve
    prms_solve[[name_prm_solve]] <- value_prm_solve
    prms_solve["nt"] <- as.integer(
      prms_solve[["t_max"]] / prms_solve[["dt"]] + 1.e-8
    )
    drift_dm_obj$prms_solve <- prms_solve
  }

  # if desired, set dx
  if (name_prm_solve == "dx") {
    value_prm_solve <- as.numeric(value_prm_solve)
    if (length(value_prm_solve) != 1) {
      stop("dx argument must be a single numeric")
    }

    prms_solve <- drift_dm_obj$prms_solve
    prms_solve[["dx"]] <- value_prm_solve
    prms_solve["nx"] <- as.integer(2 / prms_solve["dx"] + 1.e-8)
    drift_dm_obj$prms_solve <- prms_solve
  }

  return(drift_dm_obj)
}

#' @rdname set_model_prms
#' @export
set_obs_data <- function(drift_dm_obj, obs_data, eval_model = F) {
  # input check
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }


  b_encoding <- attr(drift_dm_obj, "b_encoding")
  column <- b_encoding$column
  u_name_value <- b_encoding$u_name_value
  l_name_value <- b_encoding$l_name_value
  obs_data <- check_raw_data(obs_data,
    b_encoding_column = column,
    u_name_value = u_name_value,
    l_name_value = l_name_value
  )


  if (!all(unique(obs_data$Cond) %in% drift_dm_obj$conds)) {
    warning(
      "The Cond column in obs_data provides a condition that is not",
      " listed in the model's conditions."
    )
  }

  if (!all(drift_dm_obj$conds %in% unique(obs_data$Cond))) {
    stop(
      "At least one of the model's conditions is not part of the Cond",
      " column of the provided data set."
    )
  }




  # add rts to the model
  rts_u <- list()
  rts_l <- list()
  for (one_cond in drift_dm_obj$conds) {
    sub_dat <- obs_data[obs_data$Cond == one_cond, ]
    rts_u[[one_cond]] <- sub_dat$RT[sub_dat[[column]] == u_name_value]
    rts_l[[one_cond]] <- sub_dat$RT[sub_dat[[column]] == l_name_value]

    if (length(rts_u[[one_cond]]) == 0 & length(rts_l[[one_cond]]) == 0) {
      stop("Condition ", one_cond, " did not provide any RTs")
    }
  }
  drift_dm_obj$obs_data <- list(rts_u = rts_u, rts_l = rts_l)

  # ensure that everything is up-to-date (or skip)
  drift_dm_obj <- re_evaluate_model(
    drift_dm_obj = drift_dm_obj,
    eval_model = eval_model
  )

  drift_dm_obj <- validate_drift_dm(drift_dm_obj)


  return(drift_dm_obj)
}


check_raw_data <- function(obs_data, b_encoding_column, u_name_value,
                           l_name_value) {
  # check if the provided data.frame provides all necessary things
  if (!is.data.frame(obs_data)) stop("obs_data argument is not a data frame")
  if (!("RT" %in% colnames(obs_data))) stop("no RT column in data frame")
  if (!(b_encoding_column %in% colnames(obs_data))) {
    stop("no ", b_encoding_column, " column in data frame")
  }
  if (!("Cond" %in% colnames(obs_data))) stop("no Cond column in data frame")
  if (!is.character(obs_data$Cond)) {
    warning(
      "Cond column in the provided data frame is not of type character.",
      " Trying to fix this by applying as.character() on the column"
    )
    obs_data$Cond <- as.character(obs_data$Cond)
  }
  if (!is.numeric(obs_data$RT)) {
    warning(
      "RT column in the provided data frame is not of type numeric",
      " Trying to fix this by applying as.numeric() on the column"
    )
    obs_data$RT <- as.numeric(obs_data$RT)
  }

  if (min(obs_data$RT) < 0) stop("RTs are not >= 0")


  type_obs_b_encoding <- class(obs_data[[b_encoding_column]])
  type_b_encoding_u <- class(u_name_value)

  if (!isTRUE(all.equal(type_obs_b_encoding, type_b_encoding_u))) {
    warning(
      "column ", b_encoding_column, " in obs_data expected to be of type ",
      type_b_encoding_u, ", but it is of type ", type_obs_b_encoding,
      " trying to fix this by using as.", type_b_encoding_u
    )
    as_function <- get(paste0("as.", type_b_encoding_u))
    obs_data[[b_encoding_column]] <- as_function(obs_data[[b_encoding_column]])
  }

  type_obs_b_encoding <- class(obs_data[[b_encoding_column]])
  type_b_encoding_l <- class(l_name_value)
  if (!isTRUE(all.equal(type_obs_b_encoding, type_b_encoding_l))) {
    warning(
      "column ", b_encoding_column, " in obs_data expected to be of type ",
      type_b_encoding_l, ", but it is of type ", type_obs_b_encoding,
      " trying to fix this by using as.", type_b_encoding_u
    )
    as_function <- get(paste0("as.", type_b_encoding_u))
    obs_data[[b_encoding_column]] <- as_function(obs_data[[b_encoding_column]])
  }

  if (!all(unique(obs_data[[b_encoding_column]]) %in%
    c(u_name_value, l_name_value))) {
    stop(
      b_encoding_column, " column should only contain ",
      u_name_value, " and ", l_name_value
    )
  }

  if ("ID" %in% colnames(obs_data)) {
    id_cond_table <- table(obs_data$ID, obs_data$Cond)
    idx_0 <- which(id_cond_table == 0, arr.ind = T)

    if (nrow(idx_0) > 0) {
      which_ids <-
        paste(rownames(id_cond_table)[idx_0[, 1]], collapse = ", ")
      stop("ID(s) ", which_ids, " do not provide RTs for all conditions")
    }
  }
  return(obs_data)
}


#' @rdname set_model_prms
#' @export
set_comp_funs <- function(drift_dm_obj, comp_funs, eval_model = F) {
  # user input checks
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }

  if (!is.list(comp_funs)) stop("provided comp_funs argument is not a list")
  if (length(comp_funs) == 0) stop("provided comp_funs argument is empty")
  if (is.null(names(comp_funs))) stop("entries in comp_funs are not named")

  names_all_funs <- names(comp_funs)

  # iterate over the list
  for (one_fun_name in names_all_funs) {
    # ensure a reasonable name
    one_fun_name <- match.arg(
      one_fun_name,
      choices = c("mu_fun", "mu_int_fun", "x_fun", "b_fun", "dt_b_fun", "nt_fun")
    )
    if (!is.function(comp_funs[[one_fun_name]])) {
      stop(one_fun_name, " in comp_funs is not a function")
    }
    # set the function
    drift_dm_obj$comp_funs[[one_fun_name]] <- comp_funs[[one_fun_name]]
  }

  # validate
  drift_dm_obj <- validate_drift_dm(drift_dm_obj)

  # ensure that everything is up-to-date (or skip)
  drift_dm_obj <- re_evaluate_model(
    drift_dm_obj = drift_dm_obj,
    eval_model = eval_model
  )
  return(drift_dm_obj)
}



#' @rdname set_model_prms
#' @export
set_b_encoding <- function(drift_dm_obj, b_encoding = NULL, eval_model = F) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }

  # check and set encoding
  if (is.null(b_encoding)) {
    b_encoding <- list(
      column = "Error",
      u_name_value = c("corr" = 0),
      l_name_value = c("err" = 1)
    )
  }

  if (!is.list(b_encoding)) {
    stop("b_encoding is not a list")
  } else {
    exp_names <- c("column", "u_name_value", "l_name_value")
    if (!all(names(b_encoding) %in% exp_names)) {
      stop(
        "unexpected entries in b_encoding. Expected column, u_name_value,",
        " l_name_value, found ", paste(names(b_encoding), collapse = ", ")
      )
    }
  }

  attr(drift_dm_obj, "b_encoding") <- b_encoding


  # ensure that everything is up-to-date (or skip)
  drift_dm_obj <- re_evaluate_model(
    drift_dm_obj = drift_dm_obj,
    eval_model = eval_model
  )

  drift_dm_obj <- validate_drift_dm(drift_dm_obj)
}


# ===== FUNCTIONS FOR SIMULATING DATA/TRIALS
#' Simulate trajectories/traces of a model
#'
#' @description
#' This function simulates single trajectories/traces of a diffusion model
#' (i.e., single evidence accumulation processes) using forward euler.
#'
#' Might come in handy when exploring the model's behavior or when
#' creating figures (see also [dRiftDM::plot_traces])
#'
#' @param drift_dm_obj an object inheriting from [dRiftDM::drift_dm].
#'
#' @param k numeric, the number of traces to simulate per condition
#' @param conds character vector, conditions for which traces shall be
#' simulated
#' @param add_x logical, indicating whether traces should contain a variable
#' starting point. If `TRUE`, samples from `x_fun` (see [dRiftDM::drift_dm]) are
#' drawn and added to each trace. Default is `FALSE`.
#' @param seed numerical, an optional seed for reproducible sampling
#'
#' @returns
#' The structre of the returned object depends on the arguments provided.
#' If conds is a character vector of length 1 (i.e., a single string), either
#' a single vector of length `nt + 1` or a matrix of size (`k`, `nt + 1`)
#' is returned (depending on whether `k` = 1 or `k` > 1, respectively; note
#' that `nt` is the number of steps in the discretization of time;
#' see [dRiftDM::drift_dm]).
#'
#' If conds is a character vector of length > 1, a named list is returned with
#' either vectors or matrices as entries.
#'
#' Note that evidence values with traces beyond the boundary of the
#' model are set to NA before passing them back.
#'
#' @details
#' The algorithm for simulating traces is forward euler. See
#' \insertCite{Richteretal.2023;textual}{dRiftDM} and
#' \insertCite{Ulrichetal.2015;textual}{dRiftDM} (Appendix A) for more
#' information
#'
#'
#'
#' @export
simulate_traces <- function(drift_dm_obj, k, conds = NULL, add_x = FALSE,
                            seed = NULL) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }

  if (!is.numeric(k) | k <= 0) {
    stop("k must be a numeric > 0")
  }

  if (!is.logical(add_x) | length(add_x) != 1) {
    stop("add_x must be of type logical")
  }

  if (!is.null(seed)) {
    if (!is.numeric(seed) | length(seed) != 1) {
      stop("seed must be a single numeric")
    }
    withr::local_preserve_seed()
    set.seed(seed)
  }

  if (is.null(conds)) {
    conds <- drift_dm_obj$conds
  }

  # if more conds are requested, call function for each condition
  # and pass pack as a list
  if (length(conds) > 1) {
    all_samples <- sapply(conds, function(one_cond) {
      simulate_traces(
        drift_dm_obj = drift_dm_obj, k = k,
        conds = one_cond, add_x = add_x
      )
    }, USE.NAMES = T, simplify = F)
    return(all_samples)
  }

  # else run the function with one condition
  one_cond <- conds

  if (!is.character(one_cond) | length(one_cond) != 1) {
    stop("one_cond must be a character vector of length 1")
  }
  if (!(one_cond %in% drift_dm_obj$conds)) {
    stop(one_cond, " not in the model's conds")
  }


  # unpack arguments for easier usage
  t_max <- drift_dm_obj$prms_solve[["t_max"]]
  dt <- drift_dm_obj$prms_solve[["dt"]]
  nt <- drift_dm_obj$prms_solve[["nt"]]
  nx <- drift_dm_obj$prms_solve[["nx"]]
  sigma <- drift_dm_obj$prms_solve[["sigma"]]


  e_samples <- matrix(0, nrow = k, ncol = nt + 1) # create matrix for storage
  t_vec <- seq(0, t_max, length.out = nt + 1) # all time steps
  mu_vec <- drift_dm_obj$comp_funs$mu_fun(
    prms_model = drift_dm_obj$prms_model,
    prms_solve = drift_dm_obj$prms_solve,
    t_vec = t_vec,
    one_cond = one_cond,
    ddm_opts = drift_dm_obj$ddm_opts
  )
  b_vec <- drift_dm_obj$comp_funs$b_fun(
    prms_model = drift_dm_obj$prms_model,
    prms_solve = drift_dm_obj$prms_solve,
    t_vec = t_vec,
    one_cond = one_cond,
    ddm_opts = drift_dm_obj$ddm_opts
  )
  samp_x <- numeric(k) # storage for starting values

  if (add_x) {
    xx <- seq(-1, 1, length.out = nx + 1)
    pdf_x <- drift_dm_obj$comp_funs$x_fun(
      prms_model = drift_dm_obj$prms_model,
      prms_solve = drift_dm_obj$prms_solve,
      x_vec = xx,
      one_cond = one_cond,
      ddm_opts = drift_dm_obj$ddm_opts
    )
    xx <- xx * b_vec[1]
    samp_x <- draw_from_pdf(a_pdf = pdf_x, x_def = xx, k = k)
  }

  e_samples <-
    sapply(1:k, function(one_k) {
      steps <- mu_vec * dt + sigma * sqrt(dt) * stats::rnorm(length(t_vec))
      acc_steps <- c(0, cumsum(steps)) + samp_x[one_k]
      acc_steps <- acc_steps[-length(acc_steps)] # discard last step
      idx_fP <- which(abs(acc_steps) >= b_vec)[1] # get first passage index
      if (is.na(idx_fP)) {
        warning("no boundary hit when simulating trial")
        return(acc_steps)
      }
      if (idx_fP > 0 & idx_fP < length(acc_steps)) {
        acc_steps[(idx_fP + 1):length(acc_steps)] <- NA
      }
      return(acc_steps)
    })

  if (k == 1) {
    return(as.vector(e_samples))
  }

  return(t(e_samples))
}

#' Simulate "observed" response times from a diffusion model
#'
#' This function simulates data based on the provided diffusion model. To this
#' end, random samples from the model's predicted pdfs are drawn via approximate
#' inverse cdf sampling.
#'
#' @param drift_dm_obj an object inheriting from [dRiftDM::drift_dm].
#'
#' @param n numeric, the number of samples per condition to draw
#' @param df_prms data.frame, an optional data.frame providing the parameters
#' that should be used for simulating the data. `df_prms` must provide columns
#' named like the parameters in `drift_dm_obj$free_prms`, plus a column `ID`
#' that will identify each simulated data set.
#' @param seed numeric, an optional seed for reproducable sampling
#' @param verbose integer, indicating if information about the progress
#'  should be displayed. 0 -> no information,
#'  1 -> a progress bar. Default is 1. Only effective when `df_prms` is provided
#'
#' @returns
#' a data.frame containing at least the columns `RT`, `Error`, and `Cond`. If
#' `df_prms` is provided, then the data.frame will additionally contain the
#' column `ID`
#'
#' @details
#' Cdfs are derived from the model's pdfs and response times are drawn by
#' mapping samples from a uniform distribution (in \eqn{[0, 1]}) to the values
#' of the cdf. Note that sampled response times will correspond to the
#' the values of the time space (i.e., they will correspond to
#' `seq(0, t_max, dt)`, see [dRiftDM::drift_dm]).
#'
#' @export
simulate_data <- function(drift_dm_obj, n, df_prms = NULL, seed = NULL,
                          verbose = 1) {
  if (!is.null(seed)) {
    if (!is.numeric(seed) | length(seed) != 1) {
      stop("seed must be a single numeric")
    }
    withr::local_preserve_seed()
    set.seed(seed)
  }

  if (!(verbose %in% c(0, 1))) {
    stop("verbose must be 0 or 1")
  }

  # if no df_prms are provided, call directly simulate_one_data_set
  if (is.null(df_prms)) {
    return(simulate_one_data_set(drift_dm_obj = drift_dm_obj, n = n))
  }

  # otherwise conduct checks ..
  if (!is.data.frame(df_prms)) {
    stop("df_prms must be a data.frame")
  }
  if (nrow(df_prms) <= 0) {
    stop("df_prms must provide at least one row with prms")
  }
  if (!("ID" %in% colnames(df_prms))) {
    stop("no ID column found in df_prms")
  }
  if (!all(names(df_prms)[names(df_prms) != "ID"] %in%
    drift_dm_obj$free_prms)) {
    stop(
      "columns indicating parameters in df_prms don't match",
      " free_prms of the model object drift_dm_obj"
    )
  }
  if (!all(drift_dm_obj$free_prms %in%
    names(df_prms)[names(df_prms) != "ID"])) {
    stop(
      "columns indicating parameters in df_prms don't match",
      " free_prms of the model object drift_dm_obj"
    )
  }

  # .. and run through all df_prms
  # create a progress bar if desired
  if (verbose == 1) {
    n_iter <- nrow(df_prms)
    pb <- progress::progress_bar$new(
      format = "simulating [:bar] :percent; done in: :eta",
      total = n_iter, clear = FALSE, width = 60
    )
    pb$tick(0)
  }

  all_sim_data <- apply(X = df_prms, MARGIN = 1, FUN = function(one_row) {
    one_set <- one_row[names(one_row) != "ID"]
    drift_dm_obj <- set_model_prms(
      drift_dm_obj = drift_dm_obj,
      new_prm_vals = one_set
    )
    one_sim_dat <- simulate_one_data_set(drift_dm_obj = drift_dm_obj, n = n)
    one_sim_dat$ID <- one_row[["ID"]]
    if (verbose == 1) pb$tick()
    return(one_sim_dat)
  })

  all_sim_data <- do.call("rbind", all_sim_data)
  return(all_sim_data)
}

# internal function that simulates data based on a model (using the prms set
# in the model object)
simulate_one_data_set <- function(drift_dm_obj, n) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }

  if (!is.numeric(n) | n <= 0) {
    stop("n must be a numeric > 0")
  }


  t_max <- drift_dm_obj$prms_solve[["t_max"]]
  nt <- drift_dm_obj$prms_solve[["nt"]]
  t_vec <- seq(0, t_max, length.out = nt + 1)


  b_encoding <- attr(drift_dm_obj, "b_encoding")
  sim_data <- data.frame(numeric(), numeric(), character())
  colnames(sim_data) <- c("RT", b_encoding$column, "Cond")

  if (is.null(drift_dm_obj$pdfs)) {
    drift_dm_obj <- re_evaluate_model(
      drift_dm_obj = drift_dm_obj,
      eval_model = T
    )
  }

  for (one_cond in drift_dm_obj$conds) {
    # get pdf and n_u for cond
    pdf_u <- drift_dm_obj$pdfs[[one_cond]]$pdf_u
    pdf_l <- drift_dm_obj$pdfs[[one_cond]]$pdf_l

    stopifnot(length(pdf_u) == length(t_vec))
    stopifnot(length(pdf_l) == length(t_vec))
    p_u <- sum(pdf_u) / (sum(pdf_u) + sum(pdf_l))
    n_u <- stats::rbinom(1, n, p_u)

    # sample upper pdf and lower pdf
    samp_u <- draw_from_pdf(a_pdf = pdf_u, x_def = t_vec, k = n_u)
    samp_l <- draw_from_pdf(a_pdf = pdf_l, x_def = t_vec, k = n - n_u)

    cond_data <- data.frame(RT = c(samp_u, samp_l))
    cond_data[[b_encoding$column]] <-
      rep(c(b_encoding$u_name_value, b_encoding$l_name_value),
        times = c(length(samp_u), length(samp_l))
      )
    cond_data$Cond <- one_cond
    sim_data <- rbind(sim_data, cond_data)
  }
  check_raw_data(sim_data,
    b_encoding_column = b_encoding$column,
    u_name_value = b_encoding$u_name_value,
    l_name_value = b_encoding$l_name_value
  )
  return(sim_data)
}
