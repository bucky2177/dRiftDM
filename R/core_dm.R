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
#' @param obs_data A `data.frame` used to fill the `drift_dm` object with data.
#'  See [dRiftDM::set_obs_data] for more information.
#' @param sigma The diffusion constant. Default is set to 1.
#' @param t_max The maximum of the time space. Default is set to 3 (seconds).
#' @param dt The step size of the time discretization. Default is set to .001
#'  (seconds).
#' @param dx The step size of the evidence space discretization. Default is set
#'  to .001.
#' @param mu_fun,mu_int_fun,x_fun,b_fun,dt_b_fun,nt_fun custom functions
#'  defining the components of a diffusion model. See the respective `set_*`
#'  functions.
#'
#' @returns A list with the class label "drift_dm". In general, it is not
#' recommended to directly modify the entries of this list. Users should use the
#' built-in setter functions (e.g., [dRiftDM::set_model_prms]);
#' see \code{vignette("use_ddm_models", "dRiftDM")} for more
#' information). The list contains the following entries:
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
#'  If observed data were passed to the model, either when calling `drift_dm()`
#'  or when calling [dRiftDM::set_obs_data], the list will contain an entry
#'  called `obs_data`. `obs_data`, in turn, is again a list, containing the
#'  response times of the correct or erroneous responses across conditions.
#'
#'  If the model has been evaluated (see [dRiftDM::re_evaluate_model]), the
#'  list will additionally contain...
#'
#' * ... the log likelihood;can be addressed via `drift_dm_obj$log_like_val`
#' * ... the AIC/BIC values; can be addressed via `drift_dm_obj$ic_vals`
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
                     b_fun = NULL, dt_b_fun = NULL, nt_fun = NULL) {
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
    free_prms = free_prms, obs_data = obs_data,
    prms_solve = prms_solve, comp_funs = comp_funs
  )

  # validate the model to ensure everything is as expected and pass back
  drift_dm_obj <- validate_drift_dm(drift_dm_obj)
  return(drift_dm_obj)
}


# ====== BACKEND FUNCTION FOR CREATING A DRIFT_DM OBJECT
new_drift_dm <- function(prms_model, conds, free_prms, obs_data = NULL,
                         prms_solve, comp_funs) {
  # calculate the number of discretization steps
  prms_solve["nt"] <- as.integer(
    prms_solve[["t_max"]] / prms_solve[["dt"]] + 1.e-8
  )
  prms_solve["nx"] <- as.integer(2 / prms_solve["dx"] + 1.e-8)


  # add everything
  drift_dm_obj <- list(
    prms_model = prms_model, conds = conds,
    prms_solve = prms_solve, free_prms = free_prms,
    solver = "kfe", comp_funs = comp_funs
  )
  class(drift_dm_obj) <- "drift_dm"

  # convert and add data if necessary
  if (!is.null(obs_data)) {
    drift_dm_obj <- set_obs_data(drift_dm_obj, obs_data, eval_model = F)
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

    if (arg_names[[1]] != "drift_dm_obj") {
      stop("the first argument of ", one_name, " must be 'drift_dm_obj'")
    }

    if (one_name != "x_fun" & arg_names[[2]] != "t_vec") {
      stop("the second argument of ", one_name, " must be 't_vec'")
    }

    if (one_name == "x_fun" & arg_names[[2]] != "x_vec") {
      stop("the second argument of ", one_name, " must be 'x_vec'")
    }
    if (arg_names[[3]] != "one_cond") {
      stop("the third argument of ", one_name, " must be 'one_cond'")
    }
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
    mu_fun <- function(drift_dm_obj, t_vec, one_cond) {
      if (!inherits(drift_dm_obj, "drift_dm")) {
        stop("drift_dm_obj is not of type drift_dm")
      }
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
    mu_int_fun <- function(drift_dm_obj, t_vec, one_cond) {
      if (!inherits(drift_dm_obj, "drift_dm")) {
        stop("drift_dm_obj is not of type drift_dm")
      }
      # integral of a constant drift rate
      mu <- standard_drift()
      if (!is.numeric(t_vec) | length(t_vec) <= 1) {
        stop("t_vec is not a vector")
      }
      return(mu * t_vec)
    }
  }

  if (is.null(x_fun)) {
    x_fun <- function(drift_dm_obj, x_vec, one_cond) {
      if (!inherits(drift_dm_obj, "drift_dm")) {
        stop("drift_dm_obj is not of type drift_dm")
      }
      # starting point at 0
      if (!is.numeric(x_vec) | length(x_vec) <= 1) {
        stop("x_vec is not a vector")
      }
      dx <- 2 / (length(x_vec) - 1)
      x <- numeric(length = length(x_vec))
      x[(length(x) + 1) %/% 2] <- 1 / dx
      return(x)
    }
  }

  if (is.null(b_fun)) {
    b_fun <- function(drift_dm_obj, t_vec, one_cond) {
      if (!inherits(drift_dm_obj, "drift_dm")) {
        stop("drift_dm_obj is not of type drift_dm")
      }
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
    dt_b_fun <- function(drift_dm_obj, t_vec, one_cond) {
      if (!inherits(drift_dm_obj, "drift_dm")) {
        stop("drift_dm_obj is not of type drift_dm")
      }
      # constant boundary
      if (!is.numeric(t_vec) | length(t_vec) <= 1) {
        stop("t_vec is not a vector")
      }
      dt_b <- rep(0, length(t_vec))
      return(dt_b)
    }
  }


  if (is.null(nt_fun)) {
    nt_fun <- function(drift_dm_obj, t_vec, one_cond) {
      if (!inherits(drift_dm_obj, "drift_dm")) {
        stop("drift_dm_obj is not of type drift_dm")
      }
      non_dec_time <- standard_nt()

      if (non_dec_time < 0 | non_dec_time > drift_dm_obj$prms_solve[["t_max"]]) {
        stop("non_dec_time larger than t_max or smaller than 0!")
      }

      if (!is.numeric(t_vec) | length(t_vec) <= 1) {
        stop("t_vec is not a vector")
      }

      dt <- drift_dm_obj$prms_solve[["dt"]]
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





# ===== FUNCTIONS FOR GETTING THE INFORMATION CRITERIA

# prms: ll -> log_like_val, k = number of prms, n = number of observed data
# points
calc_ic <- function(ll, k, n) {
  AIC <- 2 * k - 2 * ll
  BIC <- k * log(n) - 2 * ll

  return(c(AIC = AIC, BIC = BIC))
}

# ===== FUNCTION FOR ENSURING EVERYTHING IS UP-TO-DATE

#' Re-evaluate the model
#'
#' Updates the log likelihood value stored in `drift_dm_obj` and subsequently
#' calculates the values for the Akaike information criterion (AIC) and the
#' Bayesian information criterion (BIC)
#'
#' @param drift_dm_obj an object inheriting from [dRiftDM::drift_dm]
#'
#' @returns Returns the passed `drift_dm_obj` object, after (re-)calculating
#' the log likelihood (and AIC/BIC) of the model.
#'
#' * the log likelihood can be addressed via `drift_dm_obj$log_like_val`
#' * the AIC/BIC values can be addressed via `drift_dm_obj$ic_vals`
#'
#' @export
re_evaluate_model <- function(drift_dm_obj) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }
  drift_dm_obj$log_like_val <- calc_log_like(drift_dm_obj)

  drift_dm_obj$ic_vals <- calc_ic(
    ll = drift_dm_obj$log_like_val,
    k = length(drift_dm_obj$free_prms),
    n = length(unlist(drift_dm_obj$obs_data))
  )
  return(drift_dm_obj)
}

# ===== FUNCITONS FOR SETTING THINGS

#' Setting attributes of a drift_dm model
#'
#' @description Functions starting with `set_*` provide ways for modifying the
#' list underlying every object inheriting from `drift_dm`. Using the setter
#' methods is the highly recommended way of changing the attributes of a model
#' (see [dRiftDM::drift_dm] for a list of the built-in attributes).
#'
#' * `set_model_prms` for setting the parameters of the model that are
#'  declared as "free".
#'
#' * `set_free_prms` for declaring which parameters are "free" (i.e.,
#'  are allowed to vary or be modified)
#'
#' * `set_solver_settings` for modifying the settings relevant to the functions
#'  deriving the pdfs of the diffusion model
#'
#' * `set_obs_data` can be used to pass/set observed data
#'
#' @param drift_dm_obj an object inheriting from [dRiftDM::drift_dm]
#' @param new_model_prms a numeric vector of the same length as
#'  `drift_dm_obj$free_prms`, specifying the new values of the parameters that
#'  are allowed to vary
#' @param new_free_prms a character vector specifying the names of the
#'  parameters that are allowed to vary
#' @param names_prm_solve a character vector, possible entries are
#'  `solver`, `sigma`, `t_max`, `dt`, `dx` (Note that `solver` can only be
#'  `kfe` at the moment).
#' @param values_prm_solve numeric or character, defining the values to be set
#'  for `names_prm_solve`.
#' @param obs_data a [data.frame] which provides three columns: (1) `RT` for
#'  the response times, (2) `Error` for error coding (1 = error, 0 = correct),
#'  (3) `Cond` for specifying the conditions (see
#'  \code{vignette("use_ddm_models", "dRiftDM")} for more information).
#' @param eval_model logical, indicating whether [dRiftDM::re_evaluate_model]
#'  should be called after modifying the model. Default is `FALSE`. Note that if
#'  `eval_model` is set to `FALSE`, the attributes `log_like_val` and `ic_vals`
#'  are deleted from the model. Also, `eval_model = TRUE` only has an effect
#'  if the model provides data.
#'
#' @returns Returns the modified `drift_dm_obj` object.
#'
#' @export
set_model_prms <- function(drift_dm_obj, new_model_prms, eval_model = F) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }
  if (!is.logical(eval_model) | length(eval_model) != 1) {
    stop("eval_model must be logical")
  }

  if (!is.numeric(new_model_prms)) {
    stop("new_model_prms are not of type numeric")
  }
  if (length(new_model_prms) != length(drift_dm_obj$free_prms)) {
    stop("new_prms don't match the number of free_prms")
  }


  for (i in seq_along(drift_dm_obj$free_prms)) {
    drift_dm_obj$prms_model[[drift_dm_obj$free_prms[i]]] <- new_model_prms[i]
  }

  if (!is.null(drift_dm_obj$obs_data) & eval_model) {
    # ensure that everything is up-to-date
    drift_dm_obj <- re_evaluate_model(drift_dm_obj)
  } else {
    drift_dm_obj$log_like_val <- NULL
    drift_dm_obj$ic_vals <- NULL
  }

  return(drift_dm_obj)
}

#' @rdname set_model_prms
#' @export
set_free_prms <- function(drift_dm_obj, new_free_prms) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }

  # input checks
  name_prms_model <- names(drift_dm_obj$prms_model)
  matched_free_prms <-
    sapply(new_free_prms, function(x) {
      match.arg(x, name_prms_model)
    })
  matched_free_prms <- unname(matched_free_prms)
  if (any(matched_free_prms != new_free_prms)) {
    warning(
      "Some of the provided arguments did not match the parameters",
      " of the model. Automatically corrected.",
      " Please Double check the result"
    )
  }

  if (!is.character(matched_free_prms)) {
    stop("new_free_prms not of type character")
  }

  # ensure ordering and non-duplicates when setting the free parameters
  matched_free_prms <- name_prms_model[name_prms_model %in% matched_free_prms]
  drift_dm_obj$free_prms <- matched_free_prms

  # check model and return
  drift_dm_obj <- validate_drift_dm(drift_dm_obj)
  return(drift_dm_obj)
}

#' @rdname set_model_prms
#' @export
set_solver_settings <- function(drift_dm_obj, names_prm_solve, values_prm_solve,
                                eval_model = F) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }

  if (length(names_prm_solve) != length(values_prm_solve)) {
    stop("length of names_prm_solve and values_prm_solve don't match")
  }

  matched_names <- sapply(names_prm_solve, function(x) {
    match.arg(x, c("solver", "sigma", "t_max", "dx", "dt"))
  })
  matched_names <- unname(matched_names)
  if (any(matched_names != names_prm_solve)) {
    warning(
      "Some of the arguments in names_prm_solve did partially match with",
      " 'solver', 'dx', 'dt', 't_max', 'sigma'. Automatically corrected.",
      " Please Double check the result"
    )
  }

  if (!is.character(matched_names)) {
    stop("names_prm_solve not of type character")
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

  # ensure that nothing went wrong
  drift_dm_obj <- validate_drift_dm(drift_dm_obj)

  # ensure that everything is up-to-date
  if (!is.null(drift_dm_obj$obs_data) & eval_model) {
    drift_dm_obj <- re_evaluate_model(drift_dm_obj)
  } else {
    drift_dm_obj$log_like_val <- NULL
    drift_dm_obj$ic_vals <- NULL
  }
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
  check_raw_data(obs_data)

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
  rts_corr <- list()
  rts_err <- list()
  for (one_cond in drift_dm_obj$conds) {
    subDat <- obs_data[obs_data$Cond == one_cond, ]
    rts_corr[[one_cond]] <- subDat$RT[subDat$Error == 0]
    rts_err[[one_cond]] <- subDat$RT[subDat$Error == 1]

    if (length(rts_corr[[one_cond]]) == 0 & length(rts_err[[one_cond]]) == 0) {
      stop("Condition ", one_cond, " did not provide any RTs")
    }
  }
  drift_dm_obj$obs_data <- list(rts_corr = rts_corr, rts_err = rts_err)

  drift_dm_obj <- validate_drift_dm(drift_dm_obj)

  if (eval_model) {
    # ensure that everything is up-to-date
    drift_dm_obj <- re_evaluate_model(drift_dm_obj)
  } else {
    drift_dm_obj$log_like_val <- NULL
    drift_dm_obj$ic_vals <- NULL
  }

  return(drift_dm_obj)
}


check_raw_data <- function(obs_data) {
  # check if the provided data.frame provides all necessary things
  if (!is.data.frame(obs_data)) stop("obs_data argument is not a data frame")
  if (!("RT" %in% colnames(obs_data))) stop("no RT column in data frame")
  if (!("Error" %in% colnames(obs_data))) stop("no Error column in data frame")
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
  if (!is.numeric(obs_data$Error)) {
    warning(
      "Error column in the provided data frame is not of type numeric",
      " Trying to fix this by applying as.numeric() on the column"
    )
    obs_data$Error <- as.numeric(obs_data$Error)
  }
  if (min(obs_data$RT) < 0) stop("RTs are not >= 0")
  if (!all(unique(obs_data$Error) %in% c(0, 1))) {
    stop("Error column should only contain 0s and 1s")
  }
}


#' Setting components of a drift_dm model
#'
#' @description Functions starting with `set_*` provide ways for modifying the
#' list underlying every object inheriting from `drift_dm`. Using the setter
#' methods is the highly recommended way of changing the attributes of a model
#' (see [dRiftDM::drift_dm] for a list of the built-in attributes).
#'
#' * `set_mu_fun` can be used to set the function defining the drift rate
#'
#' * `set_mu_int_fun` can be used to set the function defining the integral
#'  of the drift rate
#'
#' * `set_x_fun` can be used to set the function defining the pdf of the
#'  starting condition
#'
#' * `set_b_fun` can be used to set the function defining the boundary
#'
#' * `set_dt_b_fun` can be used to set the first derivative of the function
#'  defining the boundary
#'
#' * `set_nt_fun` can be used to set the function providing the pdf of the
#'  non-decision time
#'
#' @param drift_dm_obj an object inheriting from [dRiftDM::drift_dm]
#'
#' @param mu_fun,mu_int_fun,x_fun,b_fun,dt_b_fun,nt_fun functions defining
#'  the respective components. See below for more details
#'
#' @details
#'
#' Please visit the \code{vignette("use_ddm_models", "dRiftDM")} for more in-depth
#' information on how to modify an object of type drift_dm.
#'
#' `mu_fun` and `mu_int_fun` provide the drift rate and its integral,
#' respectively, across the time space in a condition.
#'
#' `x_fun` provides a distribution of the starting point across the evidence
#' space.
#'
#' `b_fun` and `dt_b_fun` provide the values of the (symmetric) boundary and its
#' derivative, respectively, across the time space in a condition.
#'
#' `nt_fun` provides a distribution of the non-decision component across the
#' time space in one condition.
#'
#' All of the listed functions are stored in the list `comp_funs` of the
#' respective model.
#'
#' Each component function must take the model itself, the time or evidence
#' space, and a condition as arguments. These arguments are provided with values
#' when dRiftDM internally calls them (e.g., when calculating the pdfs, see
#' [dRiftDM::calc_pdfs])
#'
#' In order to work with `dRiftDM`, `mu_fun`,
#' `mu_int_fun`, `b_fun`, `dt_b_fun`, `nt_fun` must have the following
#' declaration: `my_fun = function(drift_dm_obj, t_vec, one_cond`). Here,
#' `drift_dm_obj` is an object inheriting from type drift_dm, `t_vec` is
#' the time space, going from 0 to `t_max` with length `nt + 1` (see
#' [dRiftDM::drift_dm]), and `one_cond` is of type character, indicating the
#' current condition. Each function must return a numeric vector
#' of the same length as `t_vec`. For `mu_fun`,
#' `mu_int_fun`, `b_fun`, `dt_b_fun` the returned values provide the
#' respective boundary/drift rate (and their derivative/integral) at every time
#' step \eqn{t}. For `nt_fun` the returned values provide the density of the
#' non-decision time across the time space (which get convoluted with the
#' pdfs when solving the model, see [dRiftDM::calc_pdfs])
#'
#' In order to work with `dRiftDM`, `x_fun` must have the following
#' declaration: `my_fun = function(drift_dm_obj, x_vec, one_cond`). Here,
#' `drift_dm_obj` is an object inheriting from type drift_dm, `x_vec` is
#' the evidence space, going from -1 to 1 with length `nx + 1` (see
#' [dRiftDM::drift_dm]), and `one_cond` is of type character, indicating the
#' current condition. Each function must return a numeric vector
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
#'
#' @export
set_mu_fun <- function(drift_dm_obj, mu_fun) {
  drift_dm_obj <- set_fun(
    drift_dm_obj = drift_dm_obj, fun = mu_fun,
    name = "mu_fun", depends_on = "t"
  )
  return(drift_dm_obj)
}

#' @rdname set_mu_fun
#' @export
set_mu_int_fun <- function(drift_dm_obj, mu_int_fun) {
  drift_dm_obj <- set_fun(
    drift_dm_obj = drift_dm_obj, fun = mu_int_fun,
    name = "mu_int_fun", depends_on = "t"
  )
  return(drift_dm_obj)
}

#' @rdname set_mu_fun
#' @export
set_x_fun <- function(drift_dm_obj, x_fun) {
  drift_dm_obj <- set_fun(
    drift_dm_obj = drift_dm_obj, fun = x_fun,
    name = "x_fun", depends_on = "x"
  )
  return(drift_dm_obj)
}

#' @rdname set_mu_fun
#' @export
set_b_fun <- function(drift_dm_obj, b_fun) {
  drift_dm_obj <- set_fun(
    drift_dm_obj = drift_dm_obj, fun = b_fun,
    name = "b_fun", depends_on = "t"
  )
  return(drift_dm_obj)
}


#' @rdname set_mu_fun
#' @export
set_dt_b_fun <- function(drift_dm_obj, dt_b_fun) {
  drift_dm_obj <- set_fun(
    drift_dm_obj = drift_dm_obj, fun = dt_b_fun,
    name = "dt_b_fun", depends_on = "t"
  )
  return(drift_dm_obj)
}

#' @rdname set_mu_fun
#' @export
set_nt_fun <- function(drift_dm_obj, nt_fun) {
  drift_dm_obj <- set_fun(
    drift_dm_obj = drift_dm_obj, fun = nt_fun,
    name = "nt_fun", depends_on = "t"
  )
  return(drift_dm_obj)
}


# internal function, which sets the relevant function.
# drift_dm_obj -> the model to modify
# fun -> the user-passed function
# name -> indicating the model component fun
# depends_on -> toggle for ensuring that x_vec/t_vec is provided
set_fun <- function(drift_dm_obj, fun, name, depends_on) {
  # user input checks
  if (!is.function(fun)) stop("provided *_fun argument is not a function")
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }
  arg_names <- names(as.list(args(fun)))
  if (arg_names[[1]] != "drift_dm_obj") {
    stop("the first argument of ", name, " must be 'drift_dm_obj'")
  }

  if (depends_on == "t") {
    if (arg_names[[2]] != "t_vec") {
      stop("the second argument of ", name, " must be 't_vec'")
    }
  } else {
    if (arg_names[[2]] != "x_vec") {
      stop("the second argument of ", name, " must be 'x_vec'")
    }
  }

  if (arg_names[[3]] != "one_cond") {
    stop("the third argument of ", name, " must be 'one_cond'")
  }

  # set the function
  drift_dm_obj$comp_funs[[name]] <- fun
  drift_dm_obj <- validate_drift_dm(drift_dm_obj)
  return(drift_dm_obj)
}




# ===== FUNCTIONS FOR SIMULATING DATA/TRIALS
#' Simulate trajectories/traces of a model
#'
#' @description
#' This function simulates single trajectories/traces of a diffusion model
#' (i.e., single evidence accumulation processes) using forward euler.
#'
#' Might come in handy when exploring the model's behavior or when
#' creating figures (see also [dRiftDM::plot_trace])
#'
#' @param drift_dm_obj an object inheriting from [dRiftDM::drift_dm].
#'
#' @param k numeric, the number of traces to simulate
#' @param one_cond character, a condition for which traces shall be simulated
#' @param add_x logical, indicating whether traces should contain a variable
#' starting point. If `TRUE`, samples from `b_fun` (see [dRiftDM::drift_dm]) are
#' drawn and added to each trace. Default is `FALSE`.
#' @param seed numerical, an optional seed for reproducable sampling
#'
#' @returns
#' For `k = 1`, the function returns a single vector of length `nt + 1` (where
#' `nt` is the number of steps in the discretization of time; see
#' [dRiftDM::drift_dm]).
#'
#' For `k = l` (with `l > 1`), the function returns a matrix of size
#' `(l, nt + 1)`.
#'
#' Evidence values where traces went beyond the boundary of the
#' model are set to NA before passing them back.
#'
#' @details
#' The algorithm for simulating traces is forward euler. See
#' \insertCite{Richteretal.2023;textual}{dRiftDM} and
#' \insertCite{Ulrichetal.2015;textual}{dRiftDM} (Appendix A) for more
#' information
#'
#' @references
#' \insertAllCited{}
#'
#'
#' @export
simulate_trace <- function(drift_dm_obj, k, one_cond, add_x = FALSE,
                           seed = NULL) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }

  if (!is.numeric(k) | k <= 0) {
    stop("k must be a numeric > 0")
  }
  if (!is.character(one_cond) | length(one_cond) != 1) {
    stop("one_cond must be a character vector of length 1")
  }
  if (!(one_cond %in% drift_dm_obj$conds)) {
    stop("one_cond not in the model's conds")
  }
  if (!is.logical(add_x) | length(add_x) != 1) {
    stop("add_x must be of type logical")
  }

  if (!is.null(seed)) {
    if (!is.numeric(seed) | length(seed) != 1) {
      stop("seed must be a single numeric")
    }
    withr::local_seed(seed)
  }

  # unpack arguments for easier usage
  t_max <- drift_dm_obj$prms_solve[["t_max"]]
  dt <- drift_dm_obj$prms_solve[["dt"]]
  nt <- drift_dm_obj$prms_solve[["nt"]]
  nx <- drift_dm_obj$prms_solve[["nx"]]
  sigma <- drift_dm_obj$prms_solve[["sigma"]]


  e_samples <- matrix(0, nrow = k, ncol = nt + 1) # create matrix for storage
  t_vec <- seq(0, t_max, length.out = nt + 1) # all time steps
  mu_vec <- drift_dm_obj$comp_funs$mu_fun(drift_dm_obj = drift_dm_obj, t_vec = t_vec, one_cond = one_cond)
  b_vec <- drift_dm_obj$comp_funs$b_fun(
    drift_dm_obj = drift_dm_obj,
    t_vec = t_vec, one_cond = one_cond
  )
  samp_x <- numeric(k) # storage for starting values

  if (add_x) {
    xx <- seq(-1, 1, length.out = nx + 1)
    pdf_x <- drift_dm_obj$comp_funs$x_fun(drift_dm_obj = drift_dm_obj, x_vec = xx, one_cond = one_cond)
    xx <- xx * b_vec[1]
    samp_x <- draw_from_pdf(a_pdf = pdf_x, x_def = xx, k = k)
  }

  e_samples <-
    sapply(1:k, function(one_k) {
      steps <- mu_vec * dt + sigma * sqrt(dt) * stats::rnorm(length(t_vec))
      acc_steps <- c(0, cumsum(steps) + samp_x[one_k])
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
#' @param seed numeric, an optional seed for reproducable sampling
#'
#' @returns
#' a data.frame containing the columns `RT`, `Error`, and `Cond`
#'
#' @description
#' Cdfs are derived from the model's pdfs and response times are drawn by
#' mapping samples from a uniform distribution (in \eqn{[0, 1]}) to the values
#' of the cdf. Note that sampled response times will correspond to the
#' the values of the time space (i.e., they will correspond to
#' `seq(0, t_max, dt)`, see [dRiftDM::drift_dm]).
#'
#' @export
simulate_data <- function(drift_dm_obj, n, seed = NULL) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }

  if (!is.numeric(n) | n <= 0) {
    stop("n must be a numeric > 0")
  }
  if (!is.null(seed)) {
    if (!is.numeric(seed) | length(seed) != 1) {
      stop("seed must be a single numeric")
    }
    withr::local_seed(seed)
  }

  t_max <- drift_dm_obj$prms_solve[["t_max"]]
  nt <- drift_dm_obj$prms_solve[["nt"]]
  t_vec <- seq(0, t_max, length.out = nt + 1)

  sim_data <- data.frame(RT = numeric(), Error = numeric(), Cond = character())
  for (one_cond in drift_dm_obj$conds) {
    # get pdf and n_u for cond
    pdfs <- calc_pdfs(
      drift_dm_obj = drift_dm_obj, one_cond = one_cond,
      solver = drift_dm_obj$solver
    )
    pdf_u <- pdfs$pdf_u
    pdf_l <- pdfs$pdf_l

    stopifnot(length(pdf_u) == length(t_vec))
    stopifnot(length(pdf_l) == length(t_vec))
    p_u <- sum(pdf_u) / (sum(pdf_u) + sum(pdf_l))
    n_u <- stats::rbinom(1, n, p_u)

    # sample upper pdf and lower pdf
    samp_u <- draw_from_pdf(a_pdf = pdf_u, x_def = t_vec, k = n_u)
    samp_l <- draw_from_pdf(a_pdf = pdf_l, x_def = t_vec, k = n - n_u)
    sim_data <- rbind(
      sim_data,
      data.frame(
        RT = c(samp_u, samp_l),
        Error = rep(c(0, 1), times = c(length(samp_u), length(samp_l))),
        Cond = one_cond
      )
    )
  }
  check_raw_data(sim_data)
  return(sim_data)
}
