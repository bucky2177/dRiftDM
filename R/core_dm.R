
# THE USER FUNCTION FOR CREATING A BASIC MODEL ----------------------------


#' Create a drift_dm object
#'
#' @description
#' This function creates an object of type drift_dm, which serves as the parent
#' class for all further created drift diffusion models. Its structure is the
#' backbone of the dRiftDM package and every child of the drift_dm class must
#' have the attributes of the parent class. Typically, users will not want to
#' create an object of drift_dm alone, as its use is very limited. Rather, they
#' will want an object of one of its child classes. See
#' \code{vignette("create_ddm_models", "dRiftDM")} for more information on how
#' to create/use/modify child classes.
#'
#' @param prms_model a named numeric vector of the model parameters. The names
#'  indicate the model's parameters, and the numeric entries provide the current
#'  parameter values.
#' @param conds a character vector, giving the names of the model's conditions.
#'  values within `conds` will be used when addressing the data and when
#'  deriving the model's predictions.
#' @param subclass a character string, with a name for the newly created
#'  diffusion model (e.g., `dmc_dm`). This will be the child class.
#' @param instr an optional character string, providing "instructions" for the
#'  underlying [dRiftDM::flex_prms] object.
#' @param obs_data an optional data.frame, providing a data set (see
#'  [dRiftDM::obs_data()] for more information).
#' @param sigma the diffusion constant. Default is `1`.
#' @param t_max the maximum of the time space. Default is set `3` (seconds).
#' @param dt,dx the step size of the time and evidence space discretization,
#'  respectively. Default is set to `.001` (which refers to seconds for dt).
#'  Note that these values are set conservatively per default. In many cases,
#'  users can increase the discretization.
#' @param solver a character string, specifying which approach to use for
#'  deriving the first passage time. Default is `kfe`, which provides access to
#'  the numerical discretization of the Kolmogorov Forward Equation.
#' @param mu_fun,mu_int_fun,x_fun,b_fun,dt_b_fun,nt_fun Optional custom
#'  functions defining the components of a diffusion model. See
#'  [dRiftDM::comp_funs()]. If an argument is `NULL`, dRiftDM falls
#'  back to the respective default function, which are document in
#'  [dRiftDM::comp_funs()].
#' @param b_coding an optional list, specifying how boundaries are coded. See
#'  [dRiftDM::b_coding()]. Default refers to accuracy coding.
#' @param round_digits integer, controls the number of digits shown for
#'  [dRiftDM::print.drift_dm()]. Default is `3`.
#' @param x an object of type `drift_dm`
#' @param ... additional parameters
#'
#' @returns A list with the parent class label `"drift_dm"` and the child class
#' label `<subclass>`. The list contains the following entries:
#'
#' * An instance of the class [dRiftDM::flex_prms] for controlling the model
#'   parameters. Provides information about the number of parameters, conditions
#'   etc.
#' * Parameters used for deriving the model predictions, [dRiftDM::prms_solve],
#'  containing the diffusion constant (`sigma`), the maximum of the time space
#'  (`t_max`), the evidence and space discretization (`dt` and `dx`,
#'  respectively), and the resulting number of steps for the time and evidence
#'  space discretization (`nt` and `nx`, respectively).
#' * A character string `solver`, indicating the method for deriving the model
#'   predictions.
#' * A list of functions called [dRiftDM::comp_funs], providing the components
#'   of the diffusion model (i.e., `mu_fun`, `mu_int_fun`, `x_fun`, `b_fun`,
#'  `dt_b_fun`, `nt_fun`). These functions are called in the depths of the
#'  package and will determine the behavior of the model
#'
#'  If (optional) observed data were passed via [dRiftDM::obs_data()],
#'  the list will contain an entry `obs_data`. This is a (nested) list with
#'  stored response times for the upper and lower boundary and with respect to
#'  each condition.
#'
#'  If the model has been evaluated (see [dRiftDM::re_evaluate_model()]), the
#'  list will additionally contain...
#'
#'  * ... the log likelihood; can be addressed via [dRiftDM::logLik.drift_dm()].
#'  * ... the PDFs of the first passage time; can be addressed via
#'  `drift_dm_obj$pdfs`.
#'
#'  Every model also has the attribute [dRiftDM::b_coding], which summarizes how
#'  the boundaries are labeled.
#'
#' @details
#'
#' To modify the entries of a model users can use the replacement methods and
#' the [dRiftDM::modify_flex_prms()] method . See
#' \code{vignette("use_ddm_models", "dRiftDM")} and
#' \code{vignette("create_ddm_models", "dRiftDM")} for more information.
#'
#' @seealso [dRiftDM::conds()], [dRiftDM::flex_prms()], [dRiftDM::prms_solve()],
#' [dRiftDM::solver()], [dRiftDM::obs_data()], [dRiftDM::comp_funs()],
#' [dRiftDM::b_coding()], [dRiftDM::coef()]
#'
#' @export
drift_dm <- function(prms_model, conds, subclass, instr = NULL, obs_data = NULL,
                     sigma = 1, t_max = 3, dt = .001, dx = .001, solver = "kfe",
                     mu_fun = NULL, mu_int_fun = NULL, x_fun = NULL,
                     b_fun = NULL, dt_b_fun = NULL, nt_fun = NULL,
                     b_coding = NULL) {

  # create the flex_prms object
  flex_prms_obj = flex_prms(object = prms_model, conds = conds, instr = instr)


  # create the prms_solve vector
  prms_solve <- c("sigma" = sigma, "t_max" = t_max, "dt" = dt, "dx" = dx)
  # calculate the number of discretization steps
  prms_solve["nt"] <- as.integer(
    prms_solve[["t_max"]] / prms_solve[["dt"]] + 1.e-8
  )
  prms_solve["nx"] <- as.integer(2 / prms_solve["dx"] + 1.e-8)


  # get default functions, if necessary
  comp_funs <- get_default_functions(
    mu_fun = mu_fun, mu_int_fun = mu_int_fun, x_fun = x_fun, b_fun = b_fun,
    dt_b_fun = dt_b_fun, nt_fun = nt_fun
  )


  # pass the arguments further down
  drift_dm_obj <- new_drift_dm(
    flex_prms_obj = flex_prms_obj,
    prms_solve = prms_solve,
    solver = solver,
    comp_funs = comp_funs,
    subclass = subclass,
    b_coding = b_coding,
    obs_data = obs_data
  )

  # validate the model to ensure everything is as expected and pass back
  drift_dm_obj <- validate_drift_dm(drift_dm_obj)
  return(drift_dm_obj)
}




# BACKEND FUNCTION FOR CREATING AND CHECKING DRIFT_DM OBJECT --------------

#' Create A DDM model - Internal
#'
#' This function takes all objects/vectors to create a ddm object
#'
#' @param flex_prms_obj  flex_prms object
#' @param prms_solve  vector with sigma, t_max, dt, dx, nt, nx
#' @param solver  string (e.g., kfe)
#' @param comp_funs a list of component functions
#' @param subclass string with model info label set for child class
#' @param b_coding optional list with b_coding (e.g., drift_dm_default_b_coding)
#' @param obs_data optional data.frame
#'
#' @details
#'
#' This function does not perform any input checks and just assembles all
#' arguments. Pre-wrangling of each argument is done in [dRiftDM::drift_dm()].
#' Checks are done done with [dRiftDM::validate_drift_dm()], called in
#' [dRiftDM::drift_dm()].
#'
#' @return
#' List with flex_prms_obj, prms_solve, solver, comp_funs. Attributes: class
#' info and b_encoding info. If obs_data is not null, then list of observed rts
#' see [dRiftDM::obs_data()].
#'
new_drift_dm <- function(flex_prms_obj, prms_solve, solver, comp_funs,
                         subclass, b_coding = NULL, obs_data = NULL) {

  # add everything
  drift_dm_obj <- list(
    flex_prms_obj = flex_prms_obj, prms_solve = prms_solve, solver = solver,
    comp_funs = comp_funs
  )
  class(drift_dm_obj) <- c(subclass, "drift_dm")

  # set encoding
  b_coding(drift_dm_obj) <- b_coding

  # add data if necessary
  if (!is.null(obs_data)) {
    obs_data(drift_dm_obj) <- obs_data
  }

  # return
  return(drift_dm_obj)
}


#' Validate a DDM object
#'
#' Performs basic checks to ensure everything is as expected with the model.
#' This function should be called whenever modifying a ddm object!
#'
#' @param drift_dm_obj the ddm object
#'
#' @details
#'
#' Checks:
#'
#'  * the flex_prms_object via [dRiftDM::validate_flex_prms()]
#'  * The prms_solve (that it is a named numeric vector
#'    [dRiftDM::check_if_named_numeric_vector()] with the expected entries) and
#'    that nt, nx make sense. This may adjust t_max if t_max is smaller than
#'    max(RT) of the observed data
#'  * The solver string (only a single string and that it refers to something
#'    that is actually implemented). If im_zero, then check if dirac delta
#'    on 0.
#'  * checks that the list comp_funs only contains functions and that each
#'    function provides the expected arguments
#'  * If PDFs exist, checks the names, lengths and data type
#'  * Checks that log_like_val (if it exists) is a single numeric.
#'  * Checks the data type name structure of obs_data
#'  * Checks the b_coding (column, u_name_value and l_name_value).
#'
#' @return
#'
#' the unmodified ddm object, after it passed all checks
#'
#'
validate_drift_dm <- function(drift_dm_obj) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }

  # check the flex_prms_obj
  validate_flex_prms(drift_dm_obj$flex_prms_obj)


  # check the prms_solve entry
  check_if_named_numeric_vector(
    x = drift_dm_obj$prms_solve,
    var_name = "prms_solve",
    labels = c("sigma", "t_max", "dt", "dx", "nx", "nt"),
    length = 6
  )

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
      prms_solve[["nt"]] <- as.integer(ceiling(max_rt / prms_solve[["dt"]]))
      prms_solve[["t_max"]] <- prms_solve[["nt"]] * prms_solve[["dt"]]
      drift_dm_obj$prms_solve <- prms_solve
    }
  }


  # check the entries of prms_solve
  prms_solve <- drift_dm_obj$prms_solve # for less intricate code
  if (prms_solve[["sigma"]] <= 0) stop("sigma in prms_solve must be positive")
  if (prms_solve[["t_max"]] <= 0) stop("t_max in prms_solve must be positive")
  if (prms_solve[["dt"]] <= 0) stop("dt in prms_solve must be positive")
  if (prms_solve[["dx"]] <= 0) stop("dx in prms_solve must be positive")
  if (prms_solve[["nt"]] <= 0) stop("nt in prms_solve must be positive")
  if (prms_solve[["nx"]] <= 0) stop("nx in prms_solve must be positive")
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



  # check if the solver entry is just a single string and if it makes sense
  if (!is.character(drift_dm_obj$solver) | length(drift_dm_obj$solver) != 1) {
    stop("solver in drift_dm_obj is not a single character/string")
  }

  if (!drift_dm_obj$solver %in% c("kfe", "im_zero")) {
    stop("solver should be either kfe or im_zero")
  }

  # check if im_zero, that x_fun provides a dirac delta on 0
  if (drift_dm_obj$solver == "im_zero") {

    comp_vals = comp_vals(drift_dm_obj)
    x_check = sapply(names(comp_vals), function(one_cond){
      obs_x_vals = comp_vals[[one_cond]]$x_vals

      x_vec = seq(-1, 1, length.out = drift_dm_obj$prms_solve[["nx"]] + 1)
      nec_x_vals = x_dirac_0(prms_model = NULL,
                             prms_solve = drift_dm_obj$prms_solve,
                             x_vec = x_vec,
                             one_cond = NULL, ddm_opts = NULL)
      return(isTRUE(all.equal(obs_x_vals, nec_x_vals)))

    }, simplify = T, USE.NAMES = T)


    if (any(!x_check)) {
      names_conds = names(which(!x_check))
      names_conds = paste(names_conds, collapse = ", ")
      warning("You selected im_zero for a solver, but the distribution of",
              " starting conditions (", names_conds, ") is different from ",
              " dRiftDM's x_dirac_0 function. Note that im_zero assumes that ",
              " evidence accumulation always starts at 0!")
    }
  }


  # ensure that each element in comp_funs is a function with the correct
  # arguments
  comp_names <- names(drift_dm_obj$comp_funs)
  nec_names <- c("mu_fun", "mu_int_fun", "x_fun", "b_fun", "dt_b_fun", "nt_fun")
  if (!all(comp_names %in% nec_names)) stop("unexpected entry in comp_funs")
  if (!all(nec_names %in% comp_names)) stop("some comp_funs are missing")

  for (one_name in nec_names) {
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
    if (!identical(names(drift_dm_obj$pdfs), conds(drift_dm_obj))) {
      stop("the pdf entry of drift_dm_obj is not labeled like the conditions")
    }

    check <- sapply(drift_dm_obj$pdfs, function(x) {
      names_check <- identical(names(x), c("pdf_u", "pdf_l"))
      length_check <-
        as.vector(sapply(x, length)) == drift_dm_obj$prms_solve[["nt"]] + 1
      length_check <- all(length_check)
      numeric_check <- all(as.vector(sapply(x, is_numeric)))
      return(list(names_check, length_check, numeric_check))
    })
    if (!all(unlist(check[1, ]))) {
      stop("pdfs within a condition are not named pdf_u and pdf_l")
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
    if (!is_numeric(drift_dm_obj$log_like_val) |
      length(drift_dm_obj$log_like_val) != 1) {
      stop("log_like val in drift_dm_obj is not a single numeric")
    }
  }


  # check the obs_data list
  # check pdfs
  if (!is.null(drift_dm_obj$obs_data)) {
    if (!identical(names(drift_dm_obj$obs_data$rts_u), conds(drift_dm_obj))) {
      stop("the rts_u entry of obs_data is not labeled like the conditions")
    }
    if (!identical(names(drift_dm_obj$obs_data$rts_l), conds(drift_dm_obj))) {
      stop("the rts_l entry of obs_data is not labeled like the conditions")
    }

    check <- sapply(drift_dm_obj$obs_data, function(x) {
      all(as.vector(sapply(x, is_numeric)))
    })
    if (!all(check)) {
      stop("rts in obs_data are not of type numeric")
    }
  }


  # check boundary encoding
  # check encoding
  b_coding <- attr(drift_dm_obj, "b_coding")
  if (!is.character(b_coding$column) | length(b_coding$column) != 1) {
    stop("b_coding_column is not a single character")
  }

  if (class(b_coding$u_name_value) != class(b_coding$l_name_value)) {
    stop("u_name_value and l_name_value in b_coding are not of the same type")
  }

  if (length(b_coding$u_name_value) != 1 | length(b_coding$l_name_value) != 1) {
    stop("u_name_value or l_name_value in b_coding are not of length 1")
  }
  names_u <- names(b_coding$u_name_value)
  if (is.null(names_u)) {
    stop("u_name_value in b_coding is not a named vector")
  }

  names_l <- names(b_coding$l_name_value)
  if (is.null(names_l)) {
    stop("l_name_value in b_coding is not a named vector")
  }


  return(drift_dm_obj)
}




# DEFAULT FUNCTIONS FOR THE DIFFERENT COMPONENTS OF A DDM -----------------


standard_drift <- function() {
  return(3)
}
standard_boundary <- function() {
  return(0.5)
}
standard_nt <- function() {
  return(0.3)
}


#' Get default/fall back component functons
#'
#' If arguments are provided that are not NULL, the respective argument is
#' simply returned. If it is NULL, then a default/fall back component function
#' is returned for the respective component. This function is called to fill up
#' non-specified component functions when calling [dRiftDM::drift_dm()].
#'
#' @param mu_fun drift rate function
#' @param mu_int_fun integral drift rate function
#' @param x_fun starting point function
#' @param b_fun boundary function
#' @param dt_b_fun derivative of boundary function
#' @param nt_fun non-decision time function
#'
#' @return
#'
#' a list of `mu_fun` to `n_fun` with either the supplied component functions
#' or the added/filled in default component functions (if an argument is NULL).
#'
#' @details
#'
#' defaults...
#'
#'  * mu_fun -> constant drift rate of 3 (i.e., vector of 0s)
#'  * mu_int_fun -> constant drift rate of 3 (i.e., vector of 3 times t_vec)
#'  * x_fun -> dirac delta on zero [dRiftDM::x_dirac_0()]
#'  * b_fun -> constant boundary of 0.5 (i.e., vector of 0.5s)
#'  * dt_b_fun -> derivate of constant boundary (i.e., vector of 0s).
#'    [dRiftDM::dt_b_constant()]
#'  * nt_fun -> constant non-decision time of 0.3 (i.e., vector for dirac delta
#'    on 0.5).
#'
#'
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



# FUNCTION FOR ENSURING EVERYTHING IS UP-TO-DATE --------------------------


#' Re-evaluate the model
#'
#' Updates the PDFs of a model. If [dRiftDM::obs_data] are set to the model, the
#' log-likelihood is also updated.
#'
#' @param drift_dm_obj an object of type [dRiftDM::drift_dm]
#' @param eval_model logical, indicating if the model should be evaluated or not.
#'  If `False`, PDFs and the log-likelihood value are deleted from the model.
#'  Default is `True`.
#'
#' @returns Returns the passed `drift_dm_obj` object, after (re-)calculating
#' the PDFs and (if observed data is set) the log-likelihood.
#'
#' * the PDFs an be addressed via `drift_dm_obj$pdfs`
#' * the log-likelihood can be addressed via `drift_dm_obj$log_like_val`
#'
#' Note that if `re_evaluate` model is called before observed data was set,
#' the function silently updates the `pdfs`, but not `log_like_val`.
#'
#' @details
#' More in-depth information about the mathematical details for
#' deriving the PDFs can be found in
#' \insertCite{Richteretal.2023;textual}{dRiftDM}
#'
#' @seealso [dRiftDM::drift_dm()]
#'
#' @export
re_evaluate_model <- function(drift_dm_obj, eval_model = T, only_log_like = F) {

  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm") 
  }
  stopifnot(is.logical(eval_model) & !is.na(eval_model))
  
  # pass back if no evaluation is requested
  if (!eval_model) {
    # set all fit indices and the pdfs to NULL
    drift_dm_obj$log_like_val <- NULL
    drift_dm_obj$pdfs <- NULL
    return(drift_dm_obj)
  }

  # unpack values and create time and evidence vector
  prms_solve <- drift_dm_obj$prms_solve
  x_vec <- seq(-1, 1, length.out = prms_solve[["nx"]] + 1)
  t_vec <- seq(0,  prms_solve[["t_max"]], length.out = prms_solve[["nt"]] + 1)

  # get the PDFs
  pdfs <- calc_pdfs(drift_dm_obj = drift_dm_obj, x_vec = x_vec, t_vec = t_vec, 
                    prms_solve = prms_solve, solver = solver)

  obs_data = drift_dm_obj$obs_data

  # update log_like_val and pass back
  log_like_val <- calc_log_like(
    pdfs = pdfs, t_vec = t_vec,
    obs_data = obs_data,
    conds = conds(drift_dm_obj)
  )
  if (only_log_like) return(log_like_val)

  drift_dm_obj$log_like_val = log_like_val
  drift_dm_obj$pdfs <- pdfs
  return(drift_dm_obj)
}


# ===== REPLACEMENT FUNCTiONS FOR SETTING THINGS TO A MODEL =============


### replace flex_prms ####

#' @rdname flex_prms
#' @export
`flex_prms<-` <- function(object, ..., value) {
  UseMethod("flex_prms<-")
}

#' @rdname flex_prms
#' @export
`flex_prms<-.drift_dm` <- function(object, ..., eval_model = F, value) {

  object$flex_prms_obj = value # object is the drift_dm object

  # ensure that everything is up-to-date
  object <- re_evaluate_model(
    drift_dm_obj = object,
    eval_model = eval_model
  )

  # ensure that nothing went wrong
  object <- validate_drift_dm(object)

  return(object)
}


### replace prms_solve ####

#' @rdname prms_solve
#' @export
`prms_solve<-` <- function(object, ..., value) {
  UseMethod("prms_solve<-")
}

#' @rdname prms_solve
#' @export
`prms_solve<-.drift_dm` <- function(object, ..., eval_model = F, value) {

  # silently strip away nt and nx
  value = value[!(names(value) %in% c("nt", "nx"))]

  if (length(value) > 4) {
    stop("too many supplied values")
  }


  # unpack input (input checks are done in set_one_solver_Setting)
  names_prm_solve <- names(value)
  values_prm_solve <- unname(value)

  if (is.null(names_prm_solve)) {
    warning("supplied value vector is not named. Returning unmodified object.")
  }

  # set all desired arguments one by one as dt and t_max both influence nt
  for (i in seq_along(names_prm_solve)) {
    object <- set_one_solver_setting(
      drift_dm_obj = object,
      name_prm_solve = names_prm_solve[i],
      value_prm_solve = values_prm_solve[i]
    )
  }

  # ensure that everything is up-to-date
  object <- re_evaluate_model(
    drift_dm_obj = object,
    eval_model = eval_model
  )

  # ensure that nothing went wrong
  object <- validate_drift_dm(object)

  return(object)
}



### replace solver ####

#' @rdname solver
#' @export
`solver<-` <- function(object, ..., value) {
  UseMethod("solver<-")
}

#' @rdname solver
#' @export
`solver<-.drift_dm` <- function(object, ..., eval_model = F, value) {

  object <- set_one_solver_setting(
    drift_dm_obj = object,
    name_prm_solve = "solver",
    value_prm_solve = unname(value)
  )

  # ensure that everything is up-to-date
  object <- re_evaluate_model(
    drift_dm_obj = object,
    eval_model = eval_model
  )

  # ensure that nothing went wrong
  object <- validate_drift_dm(object)

  return(object)
}


### replace obs_data ####

#' @rdname obs_data
#' @export
`obs_data<-` <- function(object, ..., value) {
  UseMethod("obs_data<-")
}

#' @rdname obs_data
#' @export
`obs_data<-.drift_dm` <- function(object, ..., eval_model = F, value) {

  stopifnot(is.data.frame(value) || is.null(value))

  # object is the model object, value the data.frame
  if (is.null(value)) {
    object$obs_data <- value
  } else {

    # ensure that the conditions match
    if (!("Cond" %in% colnames(value)))
      stop("No Cond column found in supplied data.frame")
    model_conds = conds(object)
    data_conds = conds(value)

    if (!all(model_conds %in% data_conds)) {
      stop(
        "At least one of the model's conditions is not part of the Cond",
        " column of the provided data.frame"
      )
    }

    if (!all(data_conds %in% model_conds)) {
      warning(
        "The Cond column in the supplied data.frame provides a condition that is",
        " not listed in the model's conditions. This condition will be ignored"
      )
    }

    # unpack b_coding
    b_coding <- attr(object, "b_coding")

    # add rts to the model (select only those conditions that are in the model)
    obs_data_rt_list = obs_data_to_rt_lists(obs_data = value, b_coding = b_coding)
    obs_data_rt_list = lapply(obs_data_rt_list, function(one_rts_list){
      return(one_rts_list[model_conds])
    })
    object$obs_data = obs_data_rt_list
  }

  # ensure that everything is up-to-date
  object <- re_evaluate_model(drift_dm_obj = object, eval_model = eval_model)

  # ensure that nothing went wrong
  object <- validate_drift_dm(object)
  return(object)
}



## replace  comp_funs ######

#' @rdname comp_funs
#' @export
`comp_funs<-` <- function(object, ..., value) {
  UseMethod("comp_funs<-")
}

#' @rdname comp_funs
#' @export
`comp_funs<-.drift_dm` <- function(object, ..., eval_model = F, value) {

  # user input checks object is the model, value the list of functions
  if (!is.list(value)) stop("provided input is not a list")
  if (length(value) == 0) stop("provided input is empty")
  if (is.null(names(value))) stop("entries in input are not named")

  names_all_funs <- names(value)

  # iterate over the list
  for (one_fun_name in names_all_funs) {
    # ensure a reasonable name
    one_fun_name <- match.arg(
      one_fun_name,
      choices = c("mu_fun", "mu_int_fun", "x_fun", "b_fun", "dt_b_fun",
                  "nt_fun")
    )
    if (!is.function(value[[one_fun_name]])) {
      stop(one_fun_name, " in input is not a function")
    }
    # set the function
    object$comp_funs[[one_fun_name]] <- value[[one_fun_name]]
  }


  # ensure that everything is up-to-date
  object <- re_evaluate_model(drift_dm_obj = object, eval_model = eval_model)


  # validate
  object <- validate_drift_dm(object)

  return(object)
}



## replace b_coding #####

#' @rdname b_coding
#' @export
`b_coding<-` <- function(object, ..., value) {
  UseMethod("b_coding<-")
}

#' @rdname b_coding
#' @export
`b_coding<-.drift_dm` <- function(object, ..., value) {

  if (is.null(value)) {
    value = drift_dm_default_b_coding()
  }

  # objectx is model, value the b_coding
  attr(object, "b_coding") <- check_b_coding(value)

  # ensure that everything is up-to-date not necessary, as b_encoding
  # is irrelevant for deriving PDFs and log_likelihood

  # validate
  object <- validate_drift_dm(object)

  return(object)
}




## replace coef ####

#' @rdname coef.drift_dm
#' @export
`coef<-` <- function(object, ..., value) {
  UseMethod("coef<-")
}

#' @rdname coef.drift_dm
#' @export
`coef<-.drift_dm` <- function(object, ..., eval_model = F, value) {

  # some input checks
  # find the maximum number of parameters
  n_prms = get_number_prms(object$flex_prms_obj)
  if (length(value) != n_prms)
    stop("input vector has an unexpected number of entries")

  # resort if named numeric vector
  if (!is.null(names(value))) {
    exp_names = names(coef(object))
    value = value[exp_names]
  }

  # check if valid numerics
  if (!is_numeric(value)) {
    stop("value does not provide valid values. Check the names and values of ",
         "the supplied vector")
  }

  # object is the model, value a numeric vector
  object$flex_prms_obj = x2prms_vals(x = unname(value),
                                     flex_prms_obj = object$flex_prms_obj)

  # ensure that everything is up-to-date
  object <- re_evaluate_model(drift_dm_obj = object, eval_model = eval_model)

  # ensure that nothing went wrong
  object <- validate_drift_dm(object)

  return(object)
}



# INTERNAL SETTER FUNCTIONS -----------------------------------------------


#' Set one specific aspect of the solver settings
#'
#' Internal function to update one aspect of `prms_solve` or `solver`.
#'
#' @param drift_dm_obj an object of type [dRiftDM::drift_dm]
#' @param name_prm_solve which aspect to address? ("sigma", "t_max", "dx", "dt",
#' "solver")
#' @param value_prm_solve either a single numeric or character string
#'
#' @details
#' Ensures that the supplied values are reasonable and that `nx` and `nt` are
#' updated. The functions [dRiftDM::prms_solve<-] and
#' [dRiftDM::solver<-] pass their arguments forward to this function.
#'
#'
#' @returns the updated un-evauated (!) drift_dm_obj object
#'
set_one_solver_setting <- function(drift_dm_obj, name_prm_solve,
                                   value_prm_solve) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }


  # input checks
  if (!is.character(name_prm_solve)) {
    stop("name_prm_solve must be of type character")
  }

  name_prm_solve = match.arg(name_prm_solve,
                             c("solver", "sigma", "t_max", "dx", "dt"))


  if (name_prm_solve != "solver" && !is_numeric(value_prm_solve)) {
    stop("supplied value must be a valid numeric")
  }
  if (name_prm_solve == "solver" && !is.character(value_prm_solve)) {
    stop("supplied value must be of type character")
  }

  if (length(value_prm_solve) != 1) {
    stop("supplied value must be of length 1")
  }


  # if desired, set solver
  if (name_prm_solve == "solver") {
    drift_dm_obj$solver <- value_prm_solve
  }

  # if desired, set sigma
  if (name_prm_solve == "sigma") {
    drift_dm_obj$prms_solve[["sigma"]] <- value_prm_solve
  }


  # if desired, set t_max or dt
  if (name_prm_solve == "t_max" | name_prm_solve == "dt") {
    prms_solve <- drift_dm_obj$prms_solve
    prms_solve[[name_prm_solve]] <- value_prm_solve
    prms_solve["nt"] <- as.integer(
      prms_solve[["t_max"]] / prms_solve[["dt"]] + 1.e-8
    )
    drift_dm_obj$prms_solve <- prms_solve
  }

  # if desired, set dx
  if (name_prm_solve == "dx") {
    prms_solve <- drift_dm_obj$prms_solve
    prms_solve[["dx"]] <- value_prm_solve
    prms_solve["nx"] <- as.integer(2 / prms_solve["dx"] + 1.e-8)
    drift_dm_obj$prms_solve <- prms_solve
  }

  return(drift_dm_obj)
}


# ===== FUNCTIONS THAT WERE PRIMARILY WRITTEN FOR SET FUNCTIONS ================


#' Check the Observed Data
#'
#' Checks a data set that is considered an "observed data set". Used in the
#' internals of dRiftDM.
#'
#' @param obs_data a [data.frame]
#' @param b_coding_column a single string, indicating which column of `obs_data`
#'  indicates how each RT corresponds to the boundaries.
#' @param u_value,l_value the value within the `b_coding_column` column that
#'  specifies the upper/lower boundary
#'
#' @return the `obs_data` for convenience (with edits as listed under Details).
#'
#' @details
#'  Checks:
#'   * if `obs_data` is a data.frame
#'   * For missing Values, and drops rows with missing values
#'   * if "RT", `b_coding_column`, and "Cond" column are present
#'   * if "Cond" is of type character, and if not casts it to character
#'   * if RT is of type numeric, and of not casts it to numeric
#'   * RTs are >= 0
#'   * that the values in `b_coding_column` match with u_value and l_value
#'     (casts the column if necessary)
#'   * if `b_coding_column` has only 1 or 2 unique values
#'   * When IDs are present, if each ID has values on each condition. At the same
#'   time unused factor levels are dropped [dRiftDM::drop_levels_ID_column]
#'
#'
check_raw_data <- function(obs_data, b_coding_column, u_value, l_value) {

  if (!is.data.frame(obs_data)) stop("obs_data argument is not a data frame")

  # check for missing values and drop them
  n_prev = nrow(obs_data)
  obs_data = stats::na.omit(obs_data)
  if (nrow(obs_data) != n_prev) {
    warning("Found missing values, removed automatically.")
  }

  # check if the provided data.frame provides all necessary things
  if (!("RT" %in% colnames(obs_data))) stop("no RT column in data frame")
  if (!(b_coding_column %in% colnames(obs_data))) {
    stop("no ", b_coding_column, " column in data frame")
  }
  if (!("Cond" %in% colnames(obs_data))) stop("no Cond column in data frame")


  # check if Cond and RT are character and numeric >= 0, respectively.
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


  # check if the data type and value of u_value matches with the values
  # observed in the b coding column. If not, tries to cast the column to
  # the class of u_value
  if (!isTRUE(all.equal(class(u_value), class(l_value)))) {
    stop ("u_value and l_value must be of the same type")
  }

  type_obs_b_coding <- class(obs_data[[b_coding_column]])
  type_b_coding <- class(u_value)

  if (!isTRUE(all.equal(type_obs_b_coding, type_b_coding))) {
    warning(
      "column ", b_coding_column, " in obs_data expected to be of type ",
      type_b_coding, ", but it is of type ", type_obs_b_coding,
      " trying to fix this by using as.character(), followed by as.",
      type_b_coding, "()"
    )
    # cast to character (in case someone supplies a factor)
    obs_data[[b_coding_column]] <- as.character(obs_data[[b_coding_column]])
    as_function <- get(paste0("as.", type_b_coding))
    obs_data[[b_coding_column]] <- as_function(obs_data[[b_coding_column]])
  }


  # check if there are only one or two entries in b_cond_column and that these
  # match with u_value and l_value
  unique_b_obs = unique(obs_data[[b_coding_column]])
  if (!(length(unique_b_obs) %in% c(1,2))) {
    stop("Only one or two unique values in ", b_coding_column, " are allowed")
  }

  if (!all(unique_b_obs %in% c(u_value, l_value))) {
    stop(
      b_coding_column, " column should only contain ",
      u_value, " or ", l_value
    )
  }

  # check if there is an ID column and if each subject provides observations
  # for all conditions
  if ("ID" %in% colnames(obs_data)) {
    obs_data = drop_levels_ID_column(obs_data) # drops unused factor levels
    id_cond_table <- table(obs_data$ID, obs_data$Cond)
    idx_0 <- which(id_cond_table == 0, arr.ind = T)

    if (nrow(idx_0) > 0) {
      which_ids <- paste(rownames(id_cond_table)[idx_0[, 1]], collapse = ", ")
      stop("ID(s) ", which_ids, " do not provide RTs for all conditions")
    }
  }
  return(obs_data)
}


#' Maybe droplevels of ID column
#'
#' This function takes a data frame with an ID colmumn, and drops the unused
#' levels from the ID column if it is factor; in this case a warning is
#' ushered
#'
#' @param some_data a data.frame with an ID column
#'
#' @returns
#'
#' if the ID column is not of type factor, then the unmodified object is
#' returned.
#'
#' if the ID column is of type factor, [droplevels] is applied, and if levels
#' were dropped, a warning is ushered
#'
drop_levels_ID_column = function(some_data) {

  stopifnot(is.data.frame(some_data))
  stopifnot("ID" %in% colnames(some_data))

  if (!is.factor(some_data$ID)) {
    return(some_data)
  }

  n_prev = length(levels(some_data$ID))
  some_data$ID = droplevels(some_data$ID)
  n_after = length(levels(some_data$ID))
  if (n_prev != n_after) {
    warning("Found unused factor levels, removed automatically")
  }
  return(some_data)
}


#' Disassemble an Observed Data set
#'
#' Takes a data.frame with columns RT, Cond, and `column` matching with
#' [dRiftDM::b_coding], and disassembles it into a list of rts
#'
#' @param obs_data a data.frame wth columns RT, Cond, and `column` matching
#'  `b_coding`
#' @param b_coding a boundary coding list (see [dRiftDM::b_coding])
#'
#' @details
#' performs checks on `b_coding` ([dRiftDM::check_b_coding]) and
#' `obs_data` ([dRiftDM::check_raw_data]) before disassembling the data set.
#'
#' @returns
#' a list of rts with entries
#'
#'  * rts_u -> containing a list with names according to the values in Cond
#'  * rts_l -> containing a list with names according to the values in Cond
#'
#'
obs_data_to_rt_lists = function(obs_data, b_coding = NULL) {


  # set default
  if (is.null(b_coding)) {
    b_coding = drift_dm_default_b_coding()
  }

  # check if everything is ok
  b_coding = check_b_coding(b_coding)
  b_column = b_coding$column
  u_name_value = b_coding$u_name_value
  l_name_value = b_coding$l_name_value

  # check the data to ensure everything is there
  obs_data <- check_raw_data(
    obs_data,
    b_coding_column = b_column,
    u_value = u_name_value,
    l_value = l_name_value
  )

  # get all conditions in the data frame and iterate through all conds....
  all_conds = unique(obs_data$Cond)
  rts_u <- list()
  rts_l <- list()
  for (one_cond in all_conds) {
    sub_dat <- obs_data[obs_data$Cond == one_cond, ]
    rts_u[[one_cond]] <- sub_dat$RT[sub_dat[[b_column]] == u_name_value]
    rts_l[[one_cond]] <- sub_dat$RT[sub_dat[[b_column]] == l_name_value]
  }

  return(list(rts_u = rts_u, rts_l = rts_l))
}


#' Check a B_Coding
#'
#' Checks if a list satisfies the requirements to be considered a valid
#' [dRiftDM::b_coding]
#'
#' @param b_coding a named list
#'
#' @details
#' Checks for...
#'
#' * input being a list
#' * list is of length three and provides the names "column", "u_name_value",
#' "l_name_value",
#' * if b_coding$column provides a single string
#' * if b_coding$u_name_value and b_coding$l_name_value provides a single
#'   named value of type character or numeric, and that both are of the same
#'   type
#'
#'
#' @returns the unmodified list for convenience
#'
check_b_coding = function(b_coding) {

  # check general outline of b_coding
  if (!is.list(b_coding)) {
    stop("b_coding is not a list")
  }

  if (length(b_coding) != 3) {
    stop("b_coding has more entries than expected")
  }

  exp_names <- c("column", "u_name_value", "l_name_value")
  if (!all(names(b_coding) %in% exp_names)) {
    stop(
      "unexpected entries in b_coding. Expected column, u_name_value,",
      " l_name_value, found ", paste(names(b_coding), collapse = ", ")
    )
  }

  # check for reasonable column value
  column = b_coding$column
  if (!is.character(column) | length(column) != 1) {
    stop ("column entry in b_coding must be a character vector of length 1")
  }


  # check for data type of u_name_val and l_name_value
  u_name_value = b_coding$u_name_value
  l_name_value = b_coding$l_name_value

  name_u = names(u_name_value)
  name_l = names(l_name_value)

  if (is.null(name_l) | is.null(name_u)) {
    stop ("u_name_value and l_name_value must be named")
  }
  if (!is_numeric(u_name_value) & !is.character(u_name_value)) {
    stop ("u_name_value must be either of type character or a valid numeric")
  }
  if (!is_numeric(l_name_value) & !is.character(l_name_value)) {
    stop ("l_name_value must be either of type character or a valid numeric")
  }
  if (length(l_name_value) != 1 | length(u_name_value) != 1) {
    stop ("l_name_value and u_name_value must be of length 1")
  }
  if (!isTRUE(all.equal(class(u_name_value), class(l_name_value)))) {
    stop ("u_name_value and l_name_value must be of the same type")
  }

  # pass back
  return(b_coding)
}


# ===== EXTRACTOR FUNCTIONS FOR GETTING THINGS FROM A MODEL =============

### flex_prms ####
# ==> see core_flex_prms.R


### all conditions ####

#' The Conditions of an Object
#'
#' Extract the conditions from a (supported) object.
#'
#' @param object an `R` object, see details
#' @param ... additional arguments.
#'
#' @details
#' `conds()` is a generic accessor function. The default methods get the
#' "conditions" that are present in an object. Currently supported objects:
#'
#'  * [dRiftDM::drift_dm]
#'  * `fits_ids_dm` (see [dRiftDM::load_fits_ids])
#'  * [data.frame]
#'  * `traces_dm_list` (see [dRiftDM::simulate_traces])
#'
#' @returns
#' `NULL` or a character vector with the conditions. `NULL` is given if the
#' object has no conditions (e.g., when a data.frame has no `Cond` column).
#'
#' @note
#' There is no respective replacement function for `conds()`. If users want to
#' modify the conditions of a [dRiftDM::drift_dm] model, they should create a
#' new [dRiftDM::flex_prms] object and subsequently set it to the model.
#' This is because there is no meaningful way to know for the package how the
#' model shall behave for the newly introduced condition(s).
#'
#' @seealso [dRiftDM::drift_dm()]
#'
#' @export
conds <- function(object, ...) {
  UseMethod("conds")
}

#' @rdname conds
#' @export
conds.drift_dm <- function(object, ...) {
  return(rownames(object$flex_prms_obj$prms_matrix))
}

#' @rdname conds
#' @export
conds.fits_ids_dm <- function(object, ...) {
  conds(object$drift_dm_fit_info$drift_dm_obj)
}


#' @rdname conds
#' @export
conds.data.frame <- function(object, ...) {
  return(unique(object$Cond))
}

#' @rdname conds
#' @export
conds.traces_dm_list <- function(object, ...) {
  return(names(object))
}


### prms_solve ####


#' The Parameters for Deriving Model Predictions
#'
#' Functions to get or set the "solver settings" of an object. This includes the
#' diffusion constant and the discretization of the time and evidence space.
#'
#' @param object an object of type [dRiftDM::drift_dm] or `fits_ids_dm`
#'  (see [dRiftDM::load_fits_ids]).
#'
#' @param ... additional arguments (i.e., `eval_model`).
#'
#' @param value a named numeric vector providing new values for the `prms_solve`
#' vector (see [dRiftDM::drift_dm()]).
#'
#' @param eval_model logical, indicating if the model should be re-evaluated or
#'  not when updating the solver settings (see [dRiftDM::re_evaluate_model]).
#'  Default is `FALSE`.
#'
#' @details
#' `prms_solve()` is a generic accessor function, and `prms_solve<-()` is a
#' generic replacement function. The default methods get and set the "solver
#' settings".
#'
#' It is possible to update parts of the "solver setttings" (i.e., parts of the
#' underlying `prms_solve` vector). However, modifying `"nx"` or `"nt"` is not
#' allowed! Any attempts to modify the respective entries will silently fail
#' (no explicit error/warning etc. is ushered).
#'
#' @returns
#' For `prms_solve()` the vector `prms_solve` (see [dRiftDM::drift_dm()]).
#'
#' For `prms_solve<-()` the updated [dRiftDM::drift_dm] object.
#'
#' @note
#' There is only a replacement function for [dRiftDM::drift_dm] objects. This is
#' because replacing the solver settings after the model has been fitted (i.e.,
#' for a `fits_ids_dm` object) doesn't make sense.
#'
#' @seealso [dRiftDM::drift_dm()]
#'
#' @export
prms_solve <- function(object, ...) {
  UseMethod("prms_solve")
}

#' @rdname prms_solve
#' @export
prms_solve.drift_dm <- function(object, ...) {
  return(object$prms_solve)
}

#' @rdname prms_solve
#' @export
prms_solve.fits_ids_dm <- function(object, ...) {
  prms_solve(object$drift_dm_fit_info$drift_dm_obj)
}


### solver ####


#' The Solver for Deriving Model Predictions
#'
#' Functions to get or set the "solver" of an object.
#'
#' @param object an object of type [dRiftDM::drift_dm] or `fits_ids_dm`
#'  (see [dRiftDM::load_fits_ids]).
#'
#' @param ... additional arguments (i.e., `eval_model`).
#'
#' @param value a single character string, providing the new "solver" (i.e.,
#'  approach to derive the first passage time; see [dRiftDM::drift_dm()]).
#'
#' @param eval_model logical, indicating if the model should be re-evaluated or
#'  not when updating the solver (see [dRiftDM::re_evaluate_model]). Default is
#'  `False`.
#'
#' @details
#' `solver()` is a generic accessor function, and `solver<-()` is a
#' generic replacement function. The default methods get and set the "solver".
#'
#' @returns
#' For `solve()` the string `solver` (see [dRiftDM::drift_dm()]).
#'
#' For `solver<-()` the updated [dRiftDM::drift_dm] object.
#'
#' @note
#' There is only a replacement function for [dRiftDM::drift_dm] objects. This is
#' because replacing the approach for deriving PDFs after the model has been
#' fitted (i.e., for a `fits_ids_dm` object) doesn't make sense.
#'
#' @seealso [dRiftDM::drift_dm()]
#'
#'
#' @export
solver <- function(object, ...) {
  UseMethod("solver")
}

#' @rdname solver
#' @export
solver.drift_dm <- function(object, ...) {
  return(object$solver)
}

#' @rdname solver
#' @export
solver.fits_ids_dm <- function(object, ...) {
  solver(object$drift_dm_fit_info$drift_dm_obj)
}


### obs_data ####

#' The Observed Data
#'
#' Functions to get or set the "observed data" of an object.
#'
#' @param object an object of type [dRiftDM::drift_dm] or `fits_ids_dm`
#'  (see [dRiftDM::load_fits_ids]).
#'
#' @param ... additional arguments passed down to the specific method.
#'
#' @param value a [data.frame] which provides three columns: (1) `RT` for
#'  the response times, (2) a column for boundary coding according to the
#'  model's [dRiftDM::b_coding()], (3) `Cond` for specifying the conditions.
#'
#' @param eval_model logical, indicating if the model should be re-evaluated or
#'  not when updating the solver settings (see [dRiftDM::re_evaluate_model]).
#'  Default is `False`.
#'
#' @param messaging logical, indicating if messages shall be ushered or not.
#'
#' @details
#' `obs_data()` is a generic accessor function, and `obs_data<-()` is a
#' generic replacement function. The default methods get and set the "observed
#' data". Their behavior, however, may be a bit unexpected.
#'
#' In [dRiftDM::drift_dm] objects, the observed data are not stored as a
#' [data.frame]. Instead, any supplied observed data set is disassembled into
#' RTs for the upper and lower boundary and with respect to the different
#' conditions (ensures more speed and easier programming in the depths of the
#' package). Yet, `obs_data()` returns a `data.frame` for [dRiftDM::drift_dm]
#' objects. This implies that `obs_data()` does not merely access
#' the observed data, but re-assembles it. Consequently, a returned [data.frame]
#' for the observed data is likely sorted differently than the [data.frame] that
#' was originally set to the model via `obs_data<-()`. Also, when the originally
#' supplied data set provided more conditions than the model, the unused
#' conditions will not be part of the returned [data.frame].
#'
#' For `fits_ids_dm` (see [dRiftDM::load_fits_ids]), the observed data are
#' stored as a [data.frame] in the general fit procedure info. This is the
#' [data.frame] that `obs_data()` will return. Thus, the returned [data.frame]
#' will match with the [data.frame] that was initially supplied to
#' [dRiftDM::estimate_model_ids], although with unused conditions being dropped.
#'
#' In theory, it is possible to update parts of the "observed data". However,
#' because `obs_data()` returns a re-assembled [data.frame] for
#' [dRiftDM::drift_dm] objects, great care has to be taken with respect to the
#' ordering of the argument `value`. A message is ushered to remind the user
#' that the returned [data.frame] may be sorted differently than expected.
#'
#' @returns
#' For `obs_data()` a (re-assembled) [data.frame] of the observed data. A
#' message is ushered to remind the user that the returned [data.frame] may
#' be sorted differently than expected.
#'
#' For `obs_data<-()` the updated [dRiftDM::drift_dm] object.
#'
#' @note
#' There is only a replacement function for [dRiftDM::drift_dm] objects. This is
#' because replacing the observed data after the model has been fitted (i.e.,
#' for a `fits_ids_dm` object) doesn't make sense.
#'
#' @seealso [dRiftDM::drift_dm()]
#'
#' @export
obs_data <- function(object, ...) {
  UseMethod("obs_data")
}


# re-assembles the observed data -> changes order!
#' @rdname obs_data
#' @export
obs_data.drift_dm <- function(object, ..., messaging = T) {


  if (is.null(object$obs_data))
    return(NULL)

  if (messaging) {
    message("Extracting observed data from the model object. Remember that the",
            " result may be sorted differently than expect!")
  }

  # rebuild the data frame
  model_conds = conds(object)
  b_coding = attr(object, "b_coding")
  b_column = b_coding$column
  value_u = unname(b_coding$u_name_value)
  value_l = unname(b_coding$l_name_value)

  data_list = lapply(model_conds, function(one_cond){

    rts_u = object$obs_data$rts_u[[one_cond]]
    rts_l = object$obs_data$rts_l[[one_cond]]

    cond_data <- data.frame(RT = c(rts_u, rts_l))
    cond_data[[b_column]] <-
      rep(c(value_u, value_l), times = c(length(rts_u), length(rts_l)))
    cond_data$Cond <- one_cond

    return(cond_data)
  })

  return(do.call("rbind", data_list))
}


# extracts saved observed data file
#' @rdname obs_data
#' @export
obs_data.fits_ids_dm <- function(object, ...) {
  return(object$drift_dm_fit_info$obs_data_ids)
}



## comp_funs ######

#' The Component Functions of A Model
#'
#' Functions to get or set the "component functions" of an object. The component
#' functions are a list of functions providing the drift rate, boundary,
#' starting point distribution, and non-decision time distribution They are at
#' the heart of the package and shape the model's behavior.
#'
#' @param object an object of type [dRiftDM::drift_dm] or `fits_ids_dm`
#'  (see [dRiftDM::load_fits_ids]).
#'
#' @param ... additional arguments passed down to the specific method.
#'
#' @param value a named list which provides the component functions to set
#'  (see Details)
#'
#' @param eval_model logical, indicating if the model should be re-evaluated or
#'  not when updating the component funtions (see [dRiftDM::re_evaluate_model]).
#'  Default is `False`.
#'
#' @details
#' `comp_funs()` is a generic accessor function, and `comp_funs<-()` is a
#' generic replacement function. The default methods get and set the "component
#' functions". The component functions are a list of functions, with the
#' following names (see also \code{vignette("create_ddm_models", "dRiftDM")} for
#' examples):
#'
#' * `mu_fun` and `mu_int_fun`, provide the drift rate and its integral,
#' respectively, across the time space.
#'
#' * `x_fun` provides a distribution of the starting point across the evidence
#' space.
#'
#' * `b_fun` and `dt_b_fun` provide the values of the upper decision boundary
#' and its derivative, respectively, across the time space. It is assumed that
#' boundaries are symmetric.
#'
#' * `nt_fun` provides a distribution of the non-decision component across the
#' time space.
#'
#' All of the listed functions are stored in the list `comp_funs` of the
#' respective model (see also [dRiftDM::drift_dm()]).
#'
#' Each component function must take the model's parameters (i.e., one row of
#' `prms_matrix`), the parameters for deriving the PDFs, the time or evidence
#' space, a condition, and a list of optional values as arguments.
#' These arguments are provided with values when `dRiftDM` internally calls them.
#'
#' In order to work with `dRiftDM`, `mu_fun`, `mu_int_fun`, `b_fun`,
#' `dt_b_fun`, and `nt_fun` must have the following declaration:
#' `my_fun = function(prms_model, prms_solve, t_vec, one_cond, ddm_opts`). Here,
#' `prms_model` is one row of `prms_matrix`, [dRiftDM::prms_solve] the
#' parameters relevant for deriving the PDFs, `t_vec` the time space, going from
#' 0 to `t_max` with length `nt + 1` (see [dRiftDM::drift_dm]), and
#' `one_cond` a single character string, indicating the current condition.
#' Finally `dmm_opts` may contain additional values.
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
#' Here, `x_vec` is the evidence space, going from -1 to 1 with length `nx + 1`
#' (see [dRiftDM::drift_dm]). Each function must return a numeric vector
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
#' by the evidence accumulation process at time \eqn{t=0}. This is a PDF
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
#' distribution across the time space will be convoluted with the PDFs derived
#' from the diffusion process.
#'
#' In psychology, the non-decision time captures time-requirements outside the
#' central decision process, such as stimulus perception and motor execution.
#'
#' The default function when calling `drift_dm()` returns a dirac
#' delta on  \eqn{t = 0.3}.
#'
#'
#' @returns
#' For `comp_funs()` the list of component functions.
#'
#' For `comp_funs<-()` the updated [dRiftDM::drift_dm] object.
#'
#' @note
#' There is only a replacement function for [dRiftDM::drift_dm] objects. This is
#' because replacing the component functions after the model has been fitted
#' (i.e., for a `fits_ids_dm` object) doesn't make sense.
#'
#' @seealso [dRiftDM::drift_dm()]
#'
#'
#' @export
comp_funs <- function(object, ...) {
  UseMethod("comp_funs")
}


# extract the component functions
#' @rdname comp_funs
#' @export
comp_funs.drift_dm <- function(object, ...) {
  return(object$comp_funs)
}


# extracts saved observed data file
#' @rdname comp_funs
#' @export
comp_funs.fits_ids_dm <- function(object, ...) {
  return(comp_funs(object$drift_dm_fit_info$drift_dm_obj))
}


## b_coding ######

#' The Coding of the Boundaries
#'
#' Functions to get or set the "boundary coding" of an object.
#'
#' @param object an object of type [dRiftDM::drift_dm] or `fits_ids_dm`
#'  (see [dRiftDM::load_fits_ids]).
#'
#' @param value a named list, specifying how boundaries are coded (see Details).
#' @param ... additional arguments.
#'
#' @details
#' `b_coding()` is a generic accessor function, and `b_coding<-()` a
#' generic replacement function. The default methods get and set the "boundary
#' coding", which is an attribute of [dRiftDM::drift_dm] model.
#'
#' The boundary coding summarizes which response time belongs to which boundary
#' and how the boundaries shall be "labeled". The list specifies three entries:
#'
#'  * `column`, contains a single character string, indicating which column
#'  in an observed data set codes the boundaries.
#'
#'  * `u_name_value`, contains a numeric or character vector of length 1. The
#'  name of this vector gives a label for the upper boundary, and the entry
#'  gives the value stored in `obs_data[[column]]` coding the upper boundary.
#'
#'   * `l_name_value`, contains a numeric or character vector of length 1. The
#'  name of this vector gives a label for the lower boundary, and the entry
#'  gives the value stored in `obs_data[[column]]` coding the lower boundary.
#'
#' The package `dRiftDM` has a default boundary coding:
#'
#'  * `column` = "Error"
#'  * `u_name_value` = c("corr" = 0)
#'  * `l_name_value` = c("err" = 1)
#'
#'  Thus, per default, dRiftDM assumes that any observed data set has a column
#'  "Error", providing the values 0 and 1 for the upper and lower boundary,
#'  respectively. The upper and lower boundaries are labeled "corr" and "err",
#'  respectively. These labels are used, for example, when calculating
#'  statistics (see [dRiftDM::calc_stats]).
#'
#'  When calling `b_coding<-()` with `value = NULL`, the default "accuracy"
#'  coding is evoked
#'
#' @seealso [dRiftDM::drift_dm()]
#'
#'
#' @export
b_coding <- function(object, ...) {
  UseMethod("b_coding")
}


# extract the b_coding
#' @rdname b_coding
#' @export
b_coding.drift_dm <- function(object, ...) {
  return(attr(object, "b_coding"))
}


# extracts b_coding in saved files
#' @rdname b_coding
#' @export
b_coding.fits_ids_dm <- function(object, ...) {
  return(b_coding(object$drift_dm_fit_info$drift_dm_obj))
}




## coef, AIC, BIC, logLik #####
# see extended_s3_methods




# Internal Getter Functions -----------------------------------------------


#' Evaluate all Component Functions
#'
#' Gets/calculates all values provided by the component functions of a
#' [dRiftDM::drift_dm] object
#'
#' @param drift_dm_obj an object of type [dRiftDM::drift_dm]
#' @param x_vec optional, the discretized evidence space
#' @param t_vec optional, the discretized time space
#' @param nx,nt,dx,dt optional, the steps and step sizes of each space
#' @param t_max the maximum time of the time space.
#'
#' @returns
#' If solver "kfe", a named list with entries "mu_vals", "x_vals", "b_vals",
#' "dt_b_vals", "nt_vals".
#'
#' If solver "im_*", the returned list will also contain "mu_int_vals".
#'
#' @details
#' arguments are optional, because they can be extracted from the model.
#' However, supplying these are faster than creating them.
#'
#'
comp_vals = function(drift_dm_obj, x_vec = NULL, t_vec = NULL,
                     nt = NULL, dt = NULL, nx = NULL, dx = NULL, 
                     prms_solve = NULL, solver = NULL, prms_matrix = NULL) {

  # unpack values
  if (is.null(nt)) nt <- drift_dm_obj$prms_solve[["nt"]]
  if (is.null(dt)) dt <- drift_dm_obj$prms_solve[["dt"]]
  
  if (is.null(nx)) nx <- drift_dm_obj$prms_solve[["nx"]]
  if (is.null(dx)) dx <- drift_dm_obj$prms_solve[["dx"]]

  if (is.null(x_vec)) x_vec <- seq(-1, 1, length.out = nx + 1)
  if (is.null(t_vec)) t_vec <- seq(0, drift_dm_obj$prms_solve[["t_max"]], 
                                   length.out = nt + 1)

  
  if (is.null(prms_solve)){
    prms_solve = drift_dm_obj$prms_solve
  }
  
  if (is.null(solver)){
    solver = drift_dm_obj$solver
  }

  if (is.null(prms_matrix)) {
    prms_matrix <- drift_dm_obj$flex_prms_obj$prms_matrix
  }
  
  # get the functions to call
  if (solver == "kfe") {
    comp_fun_names <- c("mu_fun", "x_fun", "b_fun", "dt_b_fun", "nt_fun")
  } else if (solver == "im_zero") {
    comp_fun_names <- c("mu_fun", "mu_int_fun", "x_fun", "b_fun", "dt_b_fun",
                        "nt_fun")
  } else {
    stop("requested solver ", solver, " not implemented")
  }

  # get conds, ddm_opts, and comp_funs
  conds <- rownames(prms_matrix)
  ddm_opts = drift_dm_obj$ddm_opts
  comp_funs <- drift_dm_obj$comp_funs
  

  # iterate over conds and get all model components
  all_comp_vecs <- sapply(conds, function(one_cond) {

    prms_model = prms_matrix[one_cond,]

    one_set_comp_vecs <- sapply(comp_fun_names, function(name_comp_fun) {

      if (name_comp_fun == "x_fun") {
        vals <- comp_funs[[name_comp_fun]](prms_model,
                                           prms_solve,
                                           x_vec,
                                           one_cond,
                                           ddm_opts)

      } else {
        vals <- comp_funs[[name_comp_fun]](prms_model,
                                           prms_solve,
                                           t_vec,
                                           one_cond,
                                           ddm_opts)
      }

      # checks
      # for all: numeric values and no nas or Infs
      if (!is.numeric(vals)) {
        stop("function for ", name_comp_fun,
             " provided non-numeric values, condition ", one_cond)
      }

      if (any(is.infinite(vals)) | any(is.na(vals))) {
        stop("function for ", name_comp_fun,
             "provided infinite values or NAs, condition ", one_cond)
      }

      # for boundary and densities, no negative values
      if (name_comp_fun %in% c("nt_fun", "x_fun", "b_fun")) {
        if (min(vals) < 0) {
          stop("function for ", name_comp_fun, " provided negative values, ",
               "condition ", one_cond)
        }
      }

      # for densities, must roughly integrate to 1
      if (name_comp_fun %in% c("nt_fun", "x_fun")) {
        if (name_comp_fun == "nt_fun") discr = dt
        if (name_comp_fun == "x_fun") discr = dx

        if (abs(sum(vals) * discr - 1) > drift_dm_medium_approx_error()) {
          stop("function for ", name_comp_fun, " doesn't integrate to 1, ",
               "condition ", one_cond)
        }
      }

      length_check = ifelse(name_comp_fun == "x_fun", nx + 1, nt + 1)

      if (length(vals) != length_check) {
        stop("function for ", name_comp_fun, " provided an unexpected ",
             "number of values, condition ", one_cond)
      }
      return(vals)
    }, USE.NAMES = T, simplify = F)
    names(one_set_comp_vecs) <- sub(
      pattern = "fun", replacement = c("vals"),
      x = names(one_set_comp_vecs)
    )
    return(one_set_comp_vecs)
  }, USE.NAMES = T, simplify = F)

  return(all_comp_vecs)
}




#' Unique Conditions-Parameter Combinations
#'
#' This is a helper function. It searches through the `linear_internal_list` of
#' the stored [dRiftDM::flex_prms] object, and keeps the first unique appearance
#' of parameters. For example, when the parameter muc is equal for comp, neutral,
#' and incomp, the function will provide the info "muc" and "comp", thus
#' dropping incomp and neutral, where the parameter is the same.
#'
#' @param drift_dm_obj an object of type [dRiftDM::drift_dm]
#'
#' @returns a matrix with two rows. Each column contains a combination of the
#' parameter name and the condition that can be considered unique. Parameter
#' names are stored in the first row, condition labels in the second.
#'
prms_cond_combo = function(drift_dm_obj) {

  # get all prm_conds that are not 0 or an expression
  linear_list = drift_dm_obj$flex_prms_obj$linear_internal_list
  flatten_linear = lapply(linear_list, function(x){
    lapply(x, function(y){
      if (check_digit_larger_0(y))
        return(y)
      else
        return(NULL)
    })
  })

  # unlist and drop duplicate values
  flatten_linear = unlist(flatten_linear)
  flatten_linear = flatten_linear[!duplicated(flatten_linear)]
  stopifnot(!is.unsorted(flatten_linear))

  # get labels by splitting the colnames of the flattened list
  prm_conds = sapply(names(flatten_linear), \(x) strsplit(x, "\\.")[[1]])
  prm_conds = as.matrix(prm_conds)
  return(unname(prm_conds))
}



# ===== FUNCTIONS FOR SIMULATING DATA/TRIALS ==============
#' Simulate Trajectories/Traces of a Model
#'
#' @description
#' Simulates single trajectories/traces of a model
#' (i.e., evidence accumulation processes) using forward Euler.
#'
#' Might come in handy when exploring the model's behavior or when
#' creating figures (see also [dRiftDM::plot.traces_dm_list])
#'
#' @param object an object of type [dRiftDM::drift_dm] or `fits_ids_dm` (see
#'  [dRiftDM::load_fits_ids]).
#' @param ... additional arguments passed forward to the respective method.
#'
#' @param k numeric, the number of traces to simulate per condition. Can be a
#' named numeric vector, to specify different number of traces per condition.
#' @param conds optional character vector, conditions for which traces shall be
#' simulated. If `NULL`, then traces for all conditions are simulated.
#' @param add_x logical, indicating whether traces should contain a
#' variable starting point. If `TRUE`, samples from `x_fun` (see
#' [dRiftDM::comp_vals]) are added to each trace. Default is `FALSE`.
#' @param sigma optional numeric, providing a value >= 0 for the diffusion
#'  constant "sigma" to temporally override [dRiftDM::prms_solve]. Useful for
#'  exploring the model without noise.
#' @param seed optional numerical, a seed for reproducible sampling
#' @param unpack logical, indicating if the traces shall be "unpacked" (see
#'  also [dRiftDM::unpack_traces] and the return value below).
#'
#' @param x an object of type `traces_dm_list` or `traces_dm`, resulting from a
#' call to `simulate_traces`.
#' @param round_digits integer, indicating the number of decimal places (round)
#'  to be used when printing out the traces (default is 3).
#' @param print_steps integer, indicating the number of steps to show when
#' printing out traces (default is 5).
#' @param print_k integer, indicating how many traces shall be shown when
#' printing out traces (default is 4).
#'
#'
#' @details
#' `simulate_traces()` is a generic function, applicable to objects of type
#' [dRiftDM::drift_dm] or `fits_ids_dm` (see [dRiftDM::load_fits_ids]).
#'
#' For [dRiftDM::drift_dm] objects, `simulate_traces()` performs the simulation
#' on the parameter values currently set (see
#' [dRiftDM::coef.drift_dm()]).
#'
#' For `fits_ids_dm` objects, `simulate_traces()` first extracts the model and
#' all parameter values for all IDs (see [dRiftDM::coef.fits_ids_dm()]).
#' Subsequently, simulations are based on the averaged parameter values.
#'
#' The algorithm for simulating traces is forward euler. See
#' \insertCite{Richteretal.2023;textual}{dRiftDM} and
#' \insertCite{Ulrichetal.2015;textual}{dRiftDM} (Appendix A) for more
#' information.
#'
#' @returns
#' `simulate_traces()` returns either a list of type `traces_dm_list`, or
#' directly the plain traces as matrices across conditions (if `unpack = T`).
#' If the model has only one condition (and `unpack = T`), then the matrix of
#' traces for this one condition is directly returned.
#'
#' The returned list has as many entries as conditions requested. For example,
#' if only one condition is requested via the `conds` argument, then the list is
#' of length 1 (if `unpack = F`). If `conds` is set to `NULL` (default), then
#' the list will have as many entries as conditions specified in the supplied
#' `object` (see also [dRiftDM::conds]). If `unpack = F`, the list contains an
#' additional attribute with the time space.
#'
#' Each matrix of traces has `k` rows and `nt + 1` columns, stored as an
#' array of size (`k`, `nt + 1`). Note that `nt` is the number of steps in the
#' discretization of time; see [dRiftDM::drift_dm]. If `unpack = F`, the array
#' is of type `traces_dm`. It contains some additional attributes about
#' the time space, the drift rate, the boundary, and the added starting values.
#'
#'
#' @note
#' Evidence values with traces beyond the boundary of the model are set to NA
#' before passing them back.
#'
#' The reason why `simulate_traces` passes back an object of type
#' `traces_dm_list` (instead of simply a list of arrays) is to provide a
#' [dRiftDM::plot.traces_dm_list] and [dRiftDM::print.traces_dm_list] function.
#'
#' Users can unpack the traces even after calling `simulate_traces()` using
#' [dRiftDM::unpack_traces()].
#'
#' @seealso [dRiftDM::unpack_traces()], [dRiftDM::plot.traces_dm_list()]
#'
#'
#' @export
simulate_traces <- function(object, k, ...) {
  UseMethod("simulate_traces")
}

#' @rdname simulate_traces
#' @export
simulate_traces.drift_dm <- function(object, k, ..., conds = NULL, add_x = F,
                                     sigma = NULL, seed = NULL, unpack = F) {

  if (!is.null(seed)) {
    if (!is.numeric(seed) | length(seed) != 1) {
      stop("seed must be a single numeric")
    }
    withr::local_preserve_seed()
    set.seed(seed)
  }

  if (is.null(conds)) {
    conds <- conds(object)
  }

  # get and check the ks (numeric check done in simulate_one_traces)
  ks = k
  if (length(ks) == 1) {
    ks = rep(ks, length(conds))
  }

  if (length(ks) != length(conds)) {
    stop("number of values in k must match with the number of conditions")
  }

  if (is.null(names(ks))) {
    names(ks) = conds
  }

  if (!all(conds %in% names(ks))) {
    stop("names in k don't match with the names for each condition")
  }




  # call internal function per conditions and pass back
  all_samples <- sapply(conds, function(one_cond) {
    simulate_traces_one_cond(
      drift_dm_obj = object, k = ks[one_cond],
      one_cond = one_cond, add_x = add_x, sigma = sigma
    )
  }, USE.NAMES = T, simplify = F)


  # give it a class
  class(all_samples) <- c("traces_dm_list")


  # save the time vector as it is not condition dependent
  attr(all_samples, "t_vec") = attr(all_samples[[1]], "t_vec")

  # unpack if desired
  if (unpack) {
    all_samples = unpack_traces(all_samples, unpack = unpack)
  }

  # and pass back
  return(all_samples)
}

#' @rdname simulate_traces
#' @export
simulate_traces.fits_ids_dm <- function(object, k, ...) {

  # get mean parameter values
  all_coefs = coef(object, select_unique = T)
  all_coefs = all_coefs[, colnames(all_coefs) != "ID"]
  mean_coefs = colMeans(all_coefs)

  # stick them into the model
  dm_obj = object$drift_dm_fit_info$drift_dm_obj
  coef(dm_obj) <- mean_coefs

  # simulate and pass back
  traces_obj = simulate_traces(dm_obj, k = k, ...)
  return(traces_obj)
}



# internal, to avoid large nesting
#' Simulate Traces for One Conditions
#'
#' The function simulates traces with forward Euler. It is the backend function
#' to `simulate_traces`.
#'
#' @param drift_dm_obj a model of type [dRiftDM::drift_dm]
#' @param k a single numeric, the number of traces to simulate
#' @param one_cond a single character string, specifying which condition shall
#'  be simulated
#' @param add_x a single logical, indicating if starting values shall be added
#'  or not. Sometimes, when visualizing the model, one does not want to have
#'  the starting values.
#' @param sigma a single numeric, to override the "sigma" in
#' [dRiftDM::prms_solve]
#'
#' @returns
#' An array of size k times `nt + 1`. The array becomes an object of type
#' `traces_dm`, which allows for easier printing with [dRiftDM::print.traces_dm].
#' Furthermore, each object has the additional attributes:
#'   * "t_vec" -> the time space from 0 to t_max
#'   * "mu_vals" -> the drift rate values by mu_fun
#'   * "b_vals" -> the boundary values by b_fun
#'   * "samp_x" -> the values of the starting points (which are always added to
#'   the traces in the array).
#'
simulate_traces_one_cond = function(drift_dm_obj, k, one_cond, add_x, sigma) {

  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }

  if (!is.logical(add_x) | length(add_x) != 1) {
    stop("add_x must be of type logical")
  }

  if (!is.character(one_cond) | length(one_cond) != 1) {
    stop("one_cond must be a character vector of length 1")
  }
  if (!(one_cond %in% conds(drift_dm_obj))) {
    stop(one_cond, " not in the model's conds")
  }

  if (!is_numeric(k) || k <= 0) {
    stop("k must be numeric > 0")
  }

  # unpack arguments for easier usage
  dt <- drift_dm_obj$prms_solve[["dt"]]
  nt <- drift_dm_obj$prms_solve[["nt"]]
  nx <- drift_dm_obj$prms_solve[["nx"]]
  t_max <- drift_dm_obj$prms_solve[["t_max"]]

  if (is.null(sigma)) {
    sigma <- drift_dm_obj$prms_solve[["sigma"]]
  }
  if (!is_numeric(sigma) | sigma < 0) {
    stop("sigma must be a numeric >= 0")
  }

  # get component function's values
  all_vals = comp_vals(drift_dm_obj)
  mu_vals <- all_vals[[one_cond]]$mu_vals
  b_vals <- all_vals[[one_cond]]$b_vals

  samp_x <- numeric(k) # storage for starting values
  if (add_x) {
    x_vec <- seq(-1, 1, length.out = nx + 1)
    x_vals <- all_vals[[one_cond]]$x_vals
    xx <- x_vec * b_vals[1]
    samp_x <- draw_from_pdf(a_pdf = x_vals, x_def = xx, k = k)
  }


  # now simulate values
  e_samples <-
    sapply(1:k, function(one_k) {
      steps <- mu_vals * dt + sigma * sqrt(dt) * stats::rnorm(nt + 1)
      acc_steps <- c(0, cumsum(steps)) + samp_x[one_k]
      acc_steps <- acc_steps[-length(acc_steps)] # discard last step
      idx_fP <- which(abs(acc_steps) >= b_vals)[1] # get first passage index
      if (is.na(idx_fP)) {
        warning("no boundary hit when simulating trial")
        return(acc_steps)
      }
      if (idx_fP > 0 & idx_fP < length(acc_steps)) {
        acc_steps[(idx_fP + 1):length(acc_steps)] <- NA
      }
      return(acc_steps)
    })


  # make it a class and pass back
  e_samples = t(e_samples)
  class(e_samples) = "traces_dm"
  attr(e_samples, "t_vec") = seq(0, t_max, length.out = nt + 1)
  attr(e_samples, "mu_vals") = mu_vals
  attr(e_samples, "b_vals") = b_vals
  attr(e_samples, "samp_x") = samp_x

  return(e_samples)
}


#' Unpack/Destroy Traces Objects
#'
#' @description
#'
#' [dRiftDM::simulate_traces()] provides a list of type `traces_dm_list`,
#' containing arrays of type `traces_dm`. The respective classes were created
#' to ensure convenient plotting and printing, but they are not really
#' necessary. If users want to create their own figures or access the values of
#' the simulated traces, the data types can even mask the underlying properties.
#'
#' The goal of `unpack_traces()` is to provide a convenient way to strip away
#' the attributes of `traces_dm_list` and `traces_dm` objects.
#'
#' @param object an object of type [dRiftDM::drift_dm] or `fits_ids_dm` (see
#'  [dRiftDM::load_fits_ids])
#'
#' @param ... further arguments passed on to the respective method.
#'
#' @param unpack logical, indicating if the `traces_dm` objects
#'  shall be unpacked. Default is `TRUE`.
#'
#' @param conds optional character, indicating specific condition(s). The
#' default `NULL` will lead to `conds = conds(object)`. Thus, per default all
#' conditions are accessed.
#'
#' @details
#' `unpack_traces()` is a generic function to strip away the "unnecessary"
#' information of `traces_dm_list` and `traces_dm` objects. These objects are
#' created when calling [dRiftDM::simulate_traces()].
#'
#' For `traces_dm_list`, `unpack_traces()` returns the
#' requested conditions (see the argument `conds`). The result contains
#' objects of type `traces_dm` if `unpack = FALSE`. For `unpack = TRUE`,
#' the result contains the plain arrays with the traces.
#'
#' @returns
#'
#' For `traces_dm_list`, the returned value is a list, if `conds` specifies more
#' than one condition. For example, if `conds = c("foo", "bar")`, then the
#' returned value is a list with the two (named) entries "foo" and "bar". If
#' the returned list would only have one entry (either because the
#' `traces_dm_list` has only one condition, see [dRiftDM::conds], or because a
#' user explicitly requested only one condition), then the underlying
#' array or `traces_dm` object is returned directly.
#'
#' For `traces_dm`, `unpack_traces()` returns an array with the traces, if
#' `unpack=TRUE`. If `unpack=FALSE`, the unmodified object is returned.
#'
#' @export
unpack_traces <- function(object, ...) {
  UseMethod("unpack_traces")
}

# just unpack raw traces
#' @rdname unpack_traces
#' @export
unpack_traces.traces_dm <- function(object, ..., unpack = T) {

  if (unpack) {
    all_attr = attributes(object)
    save_dims = all_attr$dim
    attributes(object) = NULL
    attr(object, "dim") = save_dims
  }

  return(object)
}

# unpack
#' @rdname unpack_traces
#' @export
unpack_traces.traces_dm_list <- function(object, ..., unpack = T,
                                         conds = NULL) {


  # default is all conds
  if (is.null(conds)) {
    conds = names(object)
  }
  conds = match.arg(conds, names(object), several.ok = T)

  # iterate across all
  traces = sapply(conds, function(x){
    unpack_traces(object[[x]], unpack = unpack)
  }, simplify = F, USE.NAMES = T)

  if (length(conds) == 1) {
    return(traces[[1]])
  } else {
    return(traces)
  }
}




#' Simulate Synthetic Responses
#'
#' This function simulates data based on the provided model. To this end,
#' random samples from the predicted PDFs are drawn via approximate inverse CDF
#' sampling.
#'
#' @param object an object inheriting from [dRiftDM::drift_dm].
#' @param ... further arguments passed on to other functions, including the
#' function [dRiftDM::simulate_values]. If users want to use a different
#' distribution than uniform for [dRiftDM::simulate_values], they must provide
#' the additional arguments (e.g., `means` and `sds`) in a format like
#' `lower/upper`.
#'
#' @param n numeric, the number of trials per condition to draw. If a single
#' numeric, then each condition will have `n` trials. Can be a (named) numeric
#' vector with the same length as there are conditions to allow a different
#' number of trials per condition.
#' @param k numeric larger than 0, indicating how many data sets shall
#' be simulated. If > 1, then it is only effective when specifying
#' `lower/upper`.
#' @param lower,upper vectors or a list, specifying the simulation space for
#' each parameter of the model (see Details). Only relevant for `k > 1`
#' @param df_prms an optional data.frame providing the parameters
#' that should be used for simulating the data. `df_prms` must provide column
#' names matching with (`coef(object, select_unique = T)`), plus a column `ID`
#' that will identify each simulated data set.
#' @param seed a single numeric, an optional seed for reproducible sampling
#' @param verbose an integer, indicating if information about the progress
#'  should be displayed. 0 -> no information, 1 -> a progress bar.
#'  Default is 1. Only effective when `k > 1`.
#'
#' @returns
#' The return value depends on whether a user specifies `lower/upper` or
#' `df_prms`. If none of these are specified and if `k = 1`, then a
#' [data.frame] containing the columns `RT`, `Error`, and `Cond` is returned.
#'
#' If `lower/upper` or `df_prms` are provided, then a list with entries
#' `synth_data` and `prms` is returned. The entry `synth_data` contains a
#' [data.frame], with the columns `RT`, `<b_column>`, `Cond`, and `ID` (the name
#' of the second column, `<b_column>`, depends on the [dRiftDM::b_coding] of the
#' model object). The entry `prms` contains a data.frame with an `ID` column and
#' the parameters used for simulating each synthetic data set.
#'
#' @details
#' `simulate_data` is a generic function for simulating data based on
#' approximate inverse CDF sampling. CDFs are derived from the model's PDFs and
#' data is drawn by mapping samples from a uniform distribution
#' (in \eqn{[0, 1]}) to the values of the CDF. Note that sampled response times
#' will correspond to the values of the time space (i.e., they will correspond
#' to `seq(0, t_max, dt)`, see [dRiftDM::drift_dm]).
#'
#' For `drift_dm` objects, the behavior of `simulate_data` depends on `k`. If
#' `k = 1` and no `lower/upper` or `df_prms` arguments are supplied, then the
#' parameters currently set to the model are used to generate
#' the synthetic data. If `k > 1`, then `k` parameter combinations are either
#' randomly drawn via [dRiftDM::simulate_values] or gathered from the provided
#' data.frame `df_prms`, and then data is simulated for each parameter
#' combination.
#'
#' When specifying `lower/upper`, parameter combinations are simulated via
#' [dRiftDM::simulate_values]. This comes in handy for simple parameter recovery
#' exercises. If `df_prms` is specified, then the parameter combinations from
#' this [data.frame] is used. Note that the column names in `df_prms` must match
#' with the (unique) parameter combinations of the model
#' (see `print(coef(object))`)
#'
#' ## Details on how to specify `lower/upper`.
#'
#' When users want to simulate data with `k > 1` and `lower/upper`, then
#' parameter values have to be drawn. One great aspect about the
#' [dRiftDM::flex_prms] object within each [dRiftDM::drift_dm] model, is that
#' users can easily allow certain parameters to vary freely across conditions.
#' Consequently, the actual number of parameters varies with the settings of
#' the [dRiftDM::flex_prms] object. In many cases, however, the simulation space
#' for a parameter is the same across conditions. For instance, in a model, the
#' parameter "mu" may vary across the conditions "easy", "medium", or "hard",
#' but the lower/upper limits are the same across conditions.
#' To avoid that users always have to re-specify the simulation space via the
#' `lower/upper` arguments, the `lower` and `upper` arguments refer to the
#' parameter labels, and `dRiftDM` figures out how to map these to all
#' parameters that vary across conditions.
#'
#' Here is an example: Assume you have the model with parameters
#' "A" and "B", and the conditions "foo" and "bar". Now assume that "A" is
#' allowed to vary for "foo" and "bar". Thus, there are actually three
#' parameters; "A~foo", "A~bar", and "B". `dRiftDM`, however, can help with
#' this. If we provide `lower = c(A = 1, B = 2)`, `upper = c(A = 3, B = 4)`,
#' `simulate_data` checks the model, and creates the vectors
#' `temp_lower = c(1,1,2)` and `temp_upper = c(3,3,4)` as a basis to simulate
#' the parameters.
#'
#' Users have three options to specify the simulation space:
#'
#' * Plain numeric vectors (not very much recommended). In this case,
#' `lower/upper` must be sorted in accordance with the free parameters in the
#' `flex_prms_obj` object (call `print(<model>)` and have a look at the
#' `Unique Parameters` output)
#'
#' * Named numeric vectors. In this case `lower/upper` have to provide labels
#' in accordance with the parameters that are considered "free" at least once
#' across conditions.
#'
#' * The most flexible way is when `lower/upper` are lists. In this case, the
#' list requires an entry called "default_values" which specifies the named or
#' plain numeric vectors as above. If the list only contains this entry, then
#' the behavior is as if `lower/upper` were already numeric vectors. However,
#' the `lower/upper` lists can also provide entries labeled as specific
#' conditions, which contain named (!) numeric vectors with parameter labels.
#' This will modify the value for the upper/lower parameter space with respect
#' to the specified parameters in the respective condition.
#'
#'
#'
#' @note
#' A function for `fits_ids_dm` will be provided in the future.
#'
#' @export
simulate_data <- function(object, ...) {
  UseMethod("simulate_data")
}

#' @export
#' @rdname simulate_data
simulate_data.drift_dm <- function(object, ..., n, k = 1, lower = NULL,
                                   upper = NULL, df_prms = NULL, seed = NULL,
                                   verbose = 1) {

  # general input checks
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

  if (!is.numeric(k) | k <= 0) {
    stop("k must be a numeric > 0")
  }

  # check what users specified ...
  case_sim = !is.null(lower) | !is.null(upper)
  case_use = !is.null(df_prms)

  # if only one data set is required and no lower/upper or df_prms,
  # then call simulate_one_data_set directly
  if (k == 1 & !case_sim & !case_use) {
    return(simulate_one_data_set(drift_dm_obj = object, n = n))
  }

  # otherwise conduct checks on what to do
  if (case_sim & case_use) {
    stop("Please specify only lower/upper OR df_prms, not both")
  }

  if (!case_sim & !case_use) {
    stop("Please specify lower/upper OR df_prms")
  }

  # get free_prms (needed further below for checks and actual simulation call)
  free_prms = names(coef(object, select_unique = T))


  # input checks on df_prms (lower/upper are checked further below)
  if (case_use) {
    if (!is.data.frame(df_prms)) {
      stop("df_prms must be a data.frame")
    }
    if (nrow(df_prms) <= 0) {
      stop("df_prms must provide at least one row with prms")
    }
    if (!("ID" %in% colnames(df_prms))) {
      stop("no ID column found in df_prms")
    }
    if (!setequal(setdiff(names(df_prms), "ID"), free_prms)) {
      stop(
        "columns indicating parameters in df_prms don't match",
        " with the (unique) parameters of the model object (see coef(<model>))"
      )
    }

    sim_prms = df_prms[c("ID", free_prms)]
  }


  # otherwise draw parameter values
  if (case_sim) {
    dots = list(...)
    distr = dots$distr
    means = dots$means
    sds = dots$sds

    if (!is.null(distr) && distr == "tnorm"){
      # exploting the function a bit :)
      m_s = get_lower_upper_smart(drift_dm_obj = object, lower = means,
                                  upper = sds, labels = T)
      means = m_s$lower
      sds = m_s$upper
    }

    l_u = get_lower_upper_smart(drift_dm_obj = object, lower = lower,
                                upper = upper, labels = T)
    sim_prms = simulate_values(lower = l_u$lower, upper = l_u$upper, k = k,
                               cast_to_data_frame = T,
                               add_id_column = "numeric", distr = distr,
                               means = means, sds = sds)
    sim_prms = sim_prms[c("ID", free_prms)]
  }

  # check if prms data.frame has only numeric values for the parameters
  check_numeric = sapply(sim_prms[names(sim_prms) != "ID"], is_numeric)
  if (!all(check_numeric)) {
    stop("Parameter values must be valid numbers")
  }

  # create a progress bar if desired
  if (verbose == 1) {
    pb <- progress::progress_bar$new(
      format = "simulating [:bar] :percent; done in: :eta",
      total = nrow(sim_prms), clear = FALSE, width = 60
    )
    pb$tick(0)
  }

  # iterate through all requested data (i.e., iterate along k)
  all_sim_data <- lapply(1:nrow(sim_prms), function(one_k) {

    # set the new parameter values
    one_prm_set = sim_prms[one_k,]
    new_prm_values = one_prm_set[names(one_prm_set) != "ID"]
    stopifnot(names(free_prms) == names(new_prm_values))
    coef(object, eval_model = T) <- as.numeric(new_prm_values)

    # then simulate
    one_sim_dat <- simulate_one_data_set(drift_dm_obj = object, n = n)
    one_sim_dat$ID <- one_prm_set$ID # use any cond to set ID
    if (verbose == 1) pb$tick()
    return(one_sim_dat)
  })

  all_sim_data <- do.call("rbind", all_sim_data)
  return(list(synth_data = all_sim_data, prms = sim_prms))
}



#' Simulate one data set
#'
#' Function that simulates a single data based on a model (using the prms set
# in the model object)
#'
#' @param drift_dm_obj a [dRiftDM::drift_dm] object
#' @param n numeric, specifying the number of trials per condition. Can be
#' single numeric, or a (named) numeric vector with the same length as
#' conds(drift_dm_obj)
#'
#' @returns A data.frame with the columns "RT", "<b_column>", and "Cond".
#'
simulate_one_data_set <- function(drift_dm_obj, n) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }

  # get all ns
  all_conds = conds(drift_dm_obj)

  if (length(n) == 1) {
    n = rep(n, length(all_conds))
  }

  if (length(n) != length(all_conds)) {
    stop("n must have as many entries as there are conditions")
  }

  if (is.null(names(n))) {
    names(n) = all_conds
  }

  if (!all(all_conds %in% names(n))) {
    stop("n doesn't specify names for each condition")
  }

  if (!is_numeric(n) || any(n <= 0)) {
    stop("n must be numeric > 0")
  }

  # get the time space for draw_from_pdf
  t_max <- drift_dm_obj$prms_solve[["t_max"]]
  nt <- drift_dm_obj$prms_solve[["nt"]]
  t_vec <- seq(0, t_max, length.out = nt + 1)


  # re_evaluate if necessary
  if (is.null(drift_dm_obj$pdfs)) {
    drift_dm_obj <- re_evaluate_model(
      drift_dm_obj = drift_dm_obj,
      eval_model = T
    )
  }

  # get b_coding to label the simulate data frame correctly
  b_coding <- attr(drift_dm_obj, "b_coding")

  # simulate the data across conditions
  sim_data = lapply(all_conds, function(one_cond) {
    # get the n for cond
    one_n = n[[one_cond]]

    # get pdf and n_u for cond
    pdf_u <- drift_dm_obj$pdfs[[one_cond]]$pdf_u
    pdf_l <- drift_dm_obj$pdfs[[one_cond]]$pdf_l

    p_u <- sum(pdf_u) / (sum(pdf_u) + sum(pdf_l))
    n_u <- stats::rbinom(1, one_n, p_u)

    # sample upper pdf and lower pdf
    samp_u <- draw_from_pdf(a_pdf = pdf_u, x_def = t_vec, k = n_u)
    samp_l <- draw_from_pdf(a_pdf = pdf_l, x_def = t_vec, k = one_n - n_u)

    cond_data <- data.frame(RT = c(samp_u, samp_l))
    cond_data[[b_coding$column]] <-
      rep(c(b_coding$u_name_value, b_coding$l_name_value),
        times = c(length(samp_u), length(samp_l))
      )
    cond_data$Cond <- one_cond
    return(cond_data)
  })

  # bind everything, check and pass back
  sim_data = do.call(rbind, sim_data)
  sim_data = check_raw_data(sim_data,
    b_coding_column = b_coding$column,
    u_value = b_coding$u_name_value,
    l_value = b_coding$l_name_value
  )
  return(sim_data)
}
