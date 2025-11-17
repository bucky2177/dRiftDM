# THE USER FUNCTION FOR CREATING A BASIC MODEL ----------------------------

#' Create a drift_dm object
#'
#' @description
#' This function creates an object of type drift_dm, which serves as the parent
#' class for all further created drift diffusion models (all of which have
#' a child class label, e.g., `dmc_dm`). The objects created by `drift_dm()` are
#' the backbone of the dRiftDM package. For a list of all pre-built models, see
#' \code{vignette("dRiftDM", "dRiftDM")}.
#'
#'
#' @param prms_model a named numeric vector of the model parameters. The names
#'  indicate the model's parameters, and the numeric entries provide the current
#'  parameter values.
#' @param conds a character vector, giving the names of the model's conditions.
#'  values within `conds` will be used when addressing the data and when
#'  deriving the model's predictions.
#' @param subclass a character string, with a name for the newly created
#'  diffusion model (e.g., `my_dmc_dm`). This will be the child class.
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
#'  deriving the first passage time. Options are `kfe` or `im_zero`. Default is
#'  `kfe`, which provides access to the numerical discretization of the
#'  Kolmogorov Forward Equation.
#' @param cost_function a character string, specifying the cost function used
#'  during estimation. Options are `neg_log_like` (negative log-likelihood),
#'  `rmse` (root-mean-squared error). Default is
#'  `neg_log_like`.
#' @param mu_fun,mu_int_fun,x_fun,b_fun,dt_b_fun,nt_fun Optional custom
#'  functions defining the components of a diffusion model. See
#'  [dRiftDM::comp_funs()]. If an argument is `NULL`, dRiftDM falls
#'  back to the respective default functions, which are documented in
#'  [dRiftDM::comp_funs()].
#' @param b_coding an optional list, specifying how boundaries are coded. See
#'  [dRiftDM::b_coding()]. Default refers to accuracy coding.
#' @param round_digits integer, controls the number of digits shown for
#'  [dRiftDM::print.drift_dm()]. Default is `3`.
#' @param x an object of type `drift_dm`
#' @param ... additional parameters
#'
#' @returns For `drift_dm()`, a list with the parent class label `"drift_dm"`
#' and the child class label `<subclass>`. The list contains the following
#' entries:
#'
#' * An instance of the class [dRiftDM::flex_prms] for controlling the model
#'   parameters. Provides information about the number of parameters, conditions
#'   etc.
#' * Parameters used for deriving the model predictions, [dRiftDM::prms_solve],
#'  containing the diffusion constant (`sigma`), the maximum of the time space
#'  (`t_max`), the evidence and space discretization (`dt` and `dx`,
#'  respectively), and the resulting number of steps for the time and evidence
#'  space discretization (`nt` and `nx`, respectively).
#' * A character string [dRiftDM::solver], indicating the method for deriving the model
#'   predictions.
#' * A character string [dRiftDM::cost_function], indicating the cost function
#'   used for model estimation.
#' * A list of functions called [dRiftDM::comp_funs], providing the components
#'   of the diffusion model (i.e., `mu_fun`, `mu_int_fun`, `x_fun`, `b_fun`,
#'  `dt_b_fun`, `nt_fun`). These functions are called in the depths of the
#'  package and will determine the behavior of the model
#'
#'  If (optional) observed data were passed via [dRiftDM::obs_data()],
#'  the list will contain an entry `obs_data`. This is a (nested) list with
#'  stored response times for the upper and lower boundary and with respect to
#'  each condition. If the cost function is a summary statistic requiring
#'  quantiles, CAFs, etc., the model also contains the entries `stats_agg` and
#'  `stats_agg_info`. The former is a (nested) list with descriptive statistics.
#'  The latter contains information about the descriptive statistics (e.g., the
#'  quantile levels).
#'
#'  If the model has been evaluated (see [dRiftDM::re_evaluate_model()]), the
#'  list will contain...
#'
#'  * ... the cost value; can be addressed via [dRiftDM::cost_value()].
#'  * ... the PDFs of the first passage time; can be addressed via
#'  [dRiftDM::pdfs()].
#'
#'  If the model was estimated (which includes its evaluation), the list
#'  will contain `estimate_info`. This entry contains a convergence flag
#'  (`conv_flag`, logical) and the `optimizer` (a string).
#'
#'  Finally, if arbitrary R objects were passed via [dRiftDM::ddm_opts()] (to
#'  access these objects when evaluating the component functions) the list will
#'  contain an entry `ddm_opts`.
#'
#'  Every model also has the attribute [dRiftDM::b_coding], which summarizes how
#'  the boundaries are labeled.
#'
#' For `print.drift_dm()`, the supplied `drift_dm` object `x` (invisible return).
#'
#' @details
#'
#' To modify the entries of a model users can use the replacement methods and
#' the [dRiftDM::modify_flex_prms()] method (see also
#' \code{vignette("dRiftDM", "dRiftDM")} and
#' \code{vignette("customize_ddms", "dRiftDM")}).
#'
#' @examples
#' # Plain call, with default component functions -----------------------------
#' # create parameter and condition vectors
#' prms <- c(muc = 4, b = 0.5)
#' conds <- c("one", "two")
#'
#' # then call the backbone function (note that we don't provide any component
#' # functions, so dRiftDM uses the default functions as documented in
#' # comp_funs())
#' my_model <- drift_dm(prms_model = prms, conds = conds, subclass = "example")
#' print(my_model)
#'
#' @seealso [dRiftDM::conds()], [dRiftDM::flex_prms()], [dRiftDM::prms_solve()],
#' [dRiftDM::solver()], [dRiftDM::obs_data()], [dRiftDM::comp_funs()],
#' [dRiftDM::b_coding()], [dRiftDM::coef()], [dRiftDM::pdfs()]
#'
#' @export
drift_dm <- function(
  prms_model,
  conds,
  subclass,
  instr = NULL,
  obs_data = NULL,
  sigma = 1,
  t_max = 3,
  dt = .001,
  dx = .001,
  solver = "kfe",
  cost_function = "neg_log_like",
  mu_fun = NULL,
  mu_int_fun = NULL,
  x_fun = NULL,
  b_fun = NULL,
  dt_b_fun = NULL,
  nt_fun = NULL,
  b_coding = NULL
) {
  # check the subclass label
  if (!is.character(subclass) | length(subclass) != 1) {
    stop("subclass is not a single character string")
  }
  # .. don't allow users to create models with labels identical to the
  # pre-built models
  #  -> get the calling function's environment
  caller_env_name <- environmentName(environment(sys.function(-1)))
  ext_call <- caller_env_name != "dRiftDM"

  if (subclass %in% drift_dm_pre_built_models() & ext_call) {
    stop(
      "The model name ",
      subclass,
      " (provided by the subclass argument) ",
      "clashes with the name of a pre-built model of dRiftDM. ",
      "From dRiftDM version 0.3.x onward, this is no longer allowed. Fix ",
      "this error by using a different name... maybe '",
      paste("my", subclass, sep = "_"),
      "'?"
    )
  }

  # create the flex_prms object
  flex_prms_obj <- flex_prms(object = prms_model, conds = conds, instr = instr)

  # match solver
  solver <- match.arg(solver, c("kfe", "im_zero"))

  # match cost_function
  cost_function <- match.arg(cost_function, drift_dm_cost_functions())

  # get default functions, if necessary
  comp_funs <- get_default_functions(
    mu_fun = mu_fun,
    mu_int_fun = mu_int_fun,
    x_fun = x_fun,
    b_fun = b_fun,
    dt_b_fun = dt_b_fun,
    nt_fun = nt_fun
  )

  # pass the arguments further down
  drift_dm_obj <- new_drift_dm(
    flex_prms_obj = flex_prms_obj,
    sigma = sigma,
    t_max = t_max,
    dt = dt,
    dx = dx,
    solver = solver,
    comp_funs = comp_funs,
    cost_function = cost_function,
    subclass = subclass,
    b_coding = b_coding,
    obs_data = obs_data
  )

  # and pass back
  return(drift_dm_obj)
}


# BACKEND FUNCTION FOR CREATING AND CHECKING DRIFT_DM OBJECT --------------

#' Create a DDM model --- internal
#'
#' This function assembles all components to create a `drift_dm` object.
#'
#' @param flex_prms_obj  a flex_prms object.
#' @param sigma  the diffusion noise (`sigma`).
#' @param t_max  the maximum trial duration (`t_max`).
#' @param dt  the temporal step size (`dt`).
#' @param dx  the evidence step size (`dx`).
#' @param solver  a string identifying the solver (e.g., `"kfe"`).
#' @param comp_funs a list of component functions.
#' @param cost_function  a string, defining how to compute the fit cost.
#' @param subclass  a string with model info label set for the child class.
#' @param b_coding  an optional list with boundary coding
#'   (e.g., `drift_dm_default_b_coding()`).
#' @param obs_data  an optional `data.frame` with observed data.
#'
#' @details
#'
#' We do not perform input checks here; we just assemble the object. Any
#' pre-wrangling is done in [dRiftDM::drift_dm()]. Checks are performed by
#' [dRiftDM::validate_drift_dm()], which is called indirectly via the
#' setters (e.g., `prms_solve()` and `obs_data()`).
#'
#' @return
#' A list with elements `flex_prms_obj`, `prms_solve`, `solver`, `comp_funs`,
#' and `cost_function`. The object has class attributes
#' `c(subclass, "drift_dm")` and an attribute `"b_coding"` containing the
#' boundary coding. If `obs_data` is not `NULL`, the observed data are attached
#' via [dRiftDM::obs_data()].
#'
#' @seealso [dRiftDM::drift_dm()], [dRiftDM::validate_drift_dm()],
#' [dRiftDM::obs_data()], [dRiftDM::drift_dm_default_b_coding()],
#' [dRiftDM::prms_solve()].
#'
#' @keywords internal
new_drift_dm <- function(
  flex_prms_obj,
  sigma,
  t_max,
  dt,
  dx,
  solver,
  comp_funs,
  cost_function,
  subclass,
  b_coding = NULL,
  obs_data = NULL
) {
  # create the prms_solve vector (with place-holder; prms_solve()<- is used
  # below to ensure internal consistency of dx, dt, and t_max)
  # -> the 50 is arbitrary and will be replaced
  prms_solve <- c(
    sigma = sigma,
    t_max = t_max,
    dt = t_max / 50L,
    dx = 2 / 50L,
    nt = 51L,
    nx = 51L
  )

  # add everything
  drift_dm_obj <- list(
    flex_prms_obj = flex_prms_obj,
    prms_solve = prms_solve,
    solver = solver,
    comp_funs = comp_funs,
    cost_function = cost_function
  )
  class(drift_dm_obj) <- c(subclass, "drift_dm")

  # set encoding
  if (is.null(b_coding)) {
    b_coding <- drift_dm_default_b_coding()
  }
  attr(drift_dm_obj, "b_coding") <- check_b_coding(b_coding)

  # update prms_solve while ensuring internal inconsistency
  # -> calls validate_drift_dm
  prms_solve(drift_dm_obj)[c("dt", "dx")] <- c(dt, dx)

  # add data if necessary
  # -> calls validate_drift_dm
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
#'  * The flex_prms_object via [dRiftDM::validate_flex_prms()]
#'  * The prms_solve (that it is a named numeric vector
#'    [dRiftDM::check_if_named_numeric_vector()] with the expected entries) and
#'    that nt, nx make sense. This may adjust t_max if t_max is smaller than
#'    max(RT) of the observed data
#'  * The solver string (only a single string and that it refers to something
#'    that is actually implemented). If im_zero, then check if dirac delta
#'    on 0.
#'  * The cost_function string (only a single string and that it refers to
#'    something that is actually implemented).
#'  * If cost_function is a summary statistic, that the objects stats_agg
#'    exists and has the correct structure.
#'  * That the list comp_funs only contains functions and that each
#'    function provides the expected arguments
#'  * If PDFs exist, the names, lengths and data type
#'  * If cost_val exists, if it is a single numeric
#'  * If obs_data exists, the data type, names, and structure
#'  * The b_coding (column, u_name_value and l_name_value).
#'
#' @return
#'
#' the ddm object, after it passed all checks. Usually, it will be unmodified.
#' The only exception is when the observed RTs are larger than `t_max`. Then,
#' the returned ddm object has a new `t_max`that covers the largest RTs.
#'
#' @keywords internal
validate_drift_dm <- function(drift_dm_obj) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }

  # check the flex_prms_obj
  validate_flex_prms(drift_dm_obj$flex_prms_obj)

  # check the obs_data list (to ensure that obs_data only contains valid RTs)
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
      stop(
        "rts in obs_data are not of type numeric or contain",
        " missing/infinite values"
      )
    }
  }

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
        "RTs in obs_data are larger than the maximum time in prms_solve. ",
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
  if (prms_solve[["sigma"]] <= 0) {
    stop("sigma in prms_solve must be positive")
  }
  if (prms_solve[["t_max"]] <= 0) {
    stop("t_max in prms_solve must be positive")
  }
  if (prms_solve[["dt"]] <= 0) {
    stop("dt in prms_solve must be positive")
  }
  if (prms_solve[["dx"]] <= 0) {
    stop("dx in prms_solve must be positive")
  }
  if (prms_solve[["nt"]] <= 0) {
    stop("nt in prms_solve must be positive")
  }
  if (prms_solve[["nx"]] <= 0) {
    stop("nx in prms_solve must be positive")
  }
  if (abs(prms_solve[["nx"]] - as.integer(prms_solve[["nx"]])) != 0) {
    stop("nx must not have decimal places")
  }
  if (abs(prms_solve[["nt"]] - as.integer(prms_solve[["nt"]])) != 0) {
    stop("nt must not have decimal places")
  }
  if (
    abs(prms_solve[["dt"]] * prms_solve[["nt"]] - prms_solve[["t_max"]]) >=
      drift_dm_approx_error()
  ) {
    stop("Final timeline not nt times dt. Check the dt and t_max values!")
  }
  if (
    abs(prms_solve[["dx"]] * prms_solve[["nx"]] - 2) >= drift_dm_approx_error()
  ) {
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

  if (!(drift_dm_obj$solver %in% c("kfe", "im_zero"))) {
    stop("solver should be either kfe or im_zero")
  }

  # check if im_zero, that x_fun provides a dirac delta on 0
  if (drift_dm_obj$solver == "im_zero") {
    if (!isTRUE(all.equal(drift_dm_obj$comp_funs$x_fun, x_dirac_0))) {
      warning(
        "You selected im_zero for a solver, but the distribution of",
        " starting conditions is not identical to dRiftDM's x_dirac_0()",
        " function. Note that im_zero assumes that evidence accumulation",
        " always starts at 0!"
      )
    }
  }

  # ensure that each element in comp_funs is a function with the correct
  # arguments
  comp_names <- names(drift_dm_obj$comp_funs)
  nec_names <- c("mu_fun", "mu_int_fun", "x_fun", "b_fun", "dt_b_fun", "nt_fun")
  if (!all(comp_names %in% nec_names)) {
    stop("unexpected entry in comp_funs")
  }
  if (!all(nec_names %in% comp_names)) {
    stop("some comp_funs are missing")
  }

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

  # check the cost value and function string
  cost_function <- drift_dm_obj$cost_function
  if (!is.character(cost_function) | length(cost_function) != 1) {
    stop("cost_function in drift_dm_obj is not a single character string")
  }

  pos_cost_functions <- drift_dm_cost_functions()
  if (!(cost_function %in% pos_cost_functions)) {
    stop(
      "cost_function should be one of ",
      paste(pos_cost_functions[-length(pos_cost_functions)], collapse = ", "),
      ", or ",
      pos_cost_functions[length(pos_cost_functions)]
    )
  }

  # check if users have accuracy coding with when cost_function is "rmse"
  if (cost_function == "rmse") {
    if (
      !isTRUE(all.equal(b_coding(drift_dm_obj), drift_dm_default_b_coding()))
    ) {
      warning(
        "The cost function is set to 'rmse', but the boundaries are not ",
        "accuracy-coded (or differ from dRiftDM's default coding). The RMSE ",
        "statistic uses quantiles of the upper boundary and CAFs (which are ",
        "ratios of the PDF mass at the upper vs. lower boundary). Please check ",
        "whether this is appropriate in your case."
      )
    }
  }

  # check cost_value
  cv <- drift_dm_obj$cost_value
  if (!is.null(cv)) {
    if (!is.numeric(cv) || anyNA(cv) || length(cv) != 1) {
      stop("cost_value in drift_dm_obj is not a single numeric")
    }
  }

  # check if the stats_agg entry is present and has the expected properties
  # if the cost_function is a summary function.. here I also check if
  # the stats_agg_info entry is reasonable
  stats_agg <- drift_dm_obj$stats_agg
  if (cost_function == "neg_log_like") {
    stopifnot(is.null(stats_agg))
  }

  if (cost_function != "neg_log_like" & !is.null(stats_agg)) {
    # check names and data types
    if (!identical(names(stats_agg), conds(drift_dm_obj))) {
      stop(
        "the stats_agg entry of drift_dm_obj is not labeled like the ",
        "conditions"
      )
    }

    if (cost_function == "rmse") {
      exp_names <- c("quantiles_corr", "cafs")
    }

    check <- sapply(
      stats_agg,
      function(x, exp_names) {
        names_check <- identical(names(x), exp_names)
        numeric_check <- all(sapply(x, is_numeric))
        return(c(names_check, numeric_check))
      },
      exp_names = exp_names
    )

    if (!all(check[1, ])) {
      stop(
        "the entries of one condition in stats_agg are not named ",
        paste(exp_names, collapse = " and ")
      )
    }
    if (!all(check[2, ])) {
      stop("the entries of one condition in stats_agg are not of type numeric")
    }

    # check if stats_agg_info is present (if cost function is a summary statistic)
    stats_agg_info <- drift_dm_obj$stats_agg_info
    if (is.null(stats_agg_info)) {
      stop(
        "cost_function is ",
        cost_function,
        ", but there is no",
        " stats_agg_info entry in drift_dm_obj"
      )
    }
    if (!identical(names(stats_agg_info), conds(drift_dm_obj))) {
      stop(
        "the stats_agg_info entry of drift_dm_obj is not labeled like the ",
        "conditions"
      )
    }

    # check if the stats_agg_info file is structured correctly
    if (cost_function == "rmse") {
      check_bins <- sapply(stats_agg_info, \(x) !is.null(x$n_bins))
      stopifnot(all(check_bins))
      check_probs <- sapply(stats_agg_info, \(x) !is.null(x$probs_corr))
      stopifnot(all(check_probs))
    }

    # check if the stats_agg_info file matches with the vectors stored in
    # stats_agg
    if (cost_function == "rmse") {
      lens_quantiles <- sapply(stats_agg, \(x) length(x$quantiles_corr))
      n_quantiles <- sapply(stats_agg_info, \(x) length(x$probs_corr))
      stopifnot(all(lens_quantiles == n_quantiles))

      lens_cafs <- sapply(stats_agg, \(x) length(x$cafs))
      n_bins <- sapply(stats_agg_info, \(x) x$n_bins)
      stopifnot(all(lens_cafs == n_bins))
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

  # check boundary encoding
  b_coding <- attr(drift_dm_obj, "b_coding")
  check_b_coding(b_coding)

  # check estimate_info
  estimate_info <- drift_dm_obj$estimate_info
  if (!is.null(estimate_info)) {
    exp_entries <- c("conv_flag", "optimizer", "message", "n_iter", "n_eval")
    if (!identical(names(estimate_info), exp_entries)) {
      stop("estimate_info has unexpected entries")
    }
    conv_flag <- estimate_info$conv_flag
    if (!is.logical(conv_flag) || length(conv_flag) != 1) {
      stop("conv_flag must be a single logical value")
    }
    optimizer <- estimate_info$optimizer
    if (!is.character(optimizer) || length(optimizer) != 1) {
      stop("optimizer must be a single string")
    }
    message <- estimate_info$message
    if (
      !(is.null(message) || (is.character(message) && length(message) == 1))
    ) {
      stop("message must be a single string")
    }
    n_iter <- estimate_info$n_iter
    if (!is.numeric(n_iter) || length(n_iter) != 1) {
      stop("n_iter must be a single numeric")
    }
    n_eval <- estimate_info$n_eval
    if (!is.numeric(n_eval) || length(n_eval) != 1) {
      stop("n_eval must be a single numeric")
    }
  }

  # check that there aren't any unexpected entries or attributes
  expected_names <- c(
    "flex_prms_obj",
    "prms_solve",
    "solver",
    "comp_funs",
    "pdfs",
    "obs_data",
    "cost_function",
    "cost_value",
    "stats_agg",
    "stats_agg_info",
    "ddm_opts",
    "estimate_info"
  )
  if (!all(names(drift_dm_obj) %in% expected_names)) {
    stop("the model contains unexpected entries")
  }

  if (
    !all(names(attributes(drift_dm_obj)) == c("names", "class", "b_coding"))
  ) {
    stop("the model contains unexpected attributes")
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


#' Get default/fall back component functions
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
#' a list of `mu_fun`, `mu_int_fun`, `x_fun`, `b_fun`, `dt_b_fun`, and
#' `nt_fun`, with either the supplied component functions
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
#' @keywords internal
get_default_functions <- function(
  mu_fun = NULL,
  mu_int_fun = NULL,
  x_fun = NULL,
  b_fun = NULL,
  dt_b_fun = NULL,
  nt_fun = NULL
) {
  if (is.null(mu_fun)) {
    mu_fun <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
      # a constant drift rate
      mu <- standard_drift()
      if (!is.numeric(t_vec) | length(t_vec) <= 1) {
        stop("t_vec is not a numeric vector with more than one entry")
      }
      mu <- rep(mu, length(t_vec))
      return(mu)
    }
  }

  if (is.null(mu_int_fun)) {
    mu_int_fun <- dummy_t
  }

  if (is.null(x_fun)) {
    x_fun <- x_dirac_0
  }

  if (is.null(b_fun)) {
    b_fun <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
      # constant boundary
      b <- standard_boundary()
      if (!is.numeric(t_vec) | length(t_vec) <= 1) {
        stop("t_vec is not a numeric vector with more than one entry")
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
        stop("t_vec is not a numeric vector with more than one entry")
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
      mu_fun = mu_fun,
      mu_int_fun = mu_int_fun,
      x_fun = x_fun,
      b_fun = b_fun,
      dt_b_fun = dt_b_fun,
      nt_fun = nt_fun
    )
  )
}


# FUNCTION FOR ENSURING EVERYTHING IS UP-TO-DATE --------------------------

#' Re-evaluate the model
#'
#' Updates the PDFs of a model. If observed data is available (e.g., via the
#' [dRiftDM::obs_data] entry or the `stats_agg` entry; depending on the
#' [dRiftDM::cost_function], see also [dRiftDM::drift_dm()]), the
#' [dRiftDM::cost_value] is also updated.
#'
#' @param drift_dm_obj an object of type [dRiftDM::drift_dm]
#' @param eval_model logical, indicating if the model should be evaluated or not.
#'  If `FALSE`, PDFs and the value of the cost function are deleted from the
#'  model. Default is `True`.
#'
#' @returns Returns the passed `drift_dm_obj` object, after (re-)calculating
#' the PDFs and (if observed data is set) the cost_value.
#'
#' * the PDFs an be addressed via `drift_dm_obj$pdfs`
#' * the cost_value can be addressed via `drift_dm_obj$cost_value`
#'
#' Note that if `re_evaluate` model is called before observed data was set,
#' the function silently updates the `pdfs`, but not `cost_value`.
#'
#' @details
#' More in-depth information about the mathematical details for
#' deriving the PDFs can be found in
#' \insertCite{Richteretal.2023;textual}{dRiftDM}
#'
#' @examples
#' # choose a pre-built model (e.g., the Ratcliff model)
#' # and set the discretization as needed
#' my_model <- ratcliff_dm()
#'
#' # then calculate the model's predicted PDF
#' my_model <- re_evaluate_model(my_model)
#' str(my_model$pdfs) # show the structure of the attached pdfs
#'
#' # if you want the cost_function, make sure some data is attached to the
#' # model (see also the documentation of obs_data())
#' obs_data(my_model) <- ratcliff_synth_data # this data set comes with dRiftDM
#' my_model <- re_evaluate_model(my_model)
#' str(my_model$pdfs)
#' print(my_model$cost_value)
#'
#' @seealso [dRiftDM::drift_dm()]
#'
#' @export
re_evaluate_model <- function(drift_dm_obj, eval_model = TRUE) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }
  stopifnot(is.logical(eval_model) & !is.na(eval_model))

  # pass back if no evaluation is requested
  if (!eval_model) {
    # set all fit indices and the pdfs to NULL
    drift_dm_obj$cost_value <- NULL
    drift_dm_obj$pdfs <- NULL
    return(drift_dm_obj)
  }

  # unpack values and create time and evidence vector
  prms_solve <- drift_dm_obj$prms_solve
  nt <- prms_solve[["nt"]]
  nx <- prms_solve[["nx"]]
  dt <- prms_solve[["dt"]]
  dx <- prms_solve[["dx"]]
  t_max <- prms_solve[["t_max"]]
  stopifnot(nt == t_max / dt)
  stopifnot(nx == 2 / dx)
  x_vec <- seq(-1, 1, length.out = nx + 1)
  t_vec <- seq(0, t_max, length.out = nt + 1)

  # get the PDFs
  pdfs <- calc_pdfs(
    drift_dm_obj = drift_dm_obj,
    x_vec = x_vec,
    t_vec = t_vec,
    prms_solve = prms_solve
  )

  # get the requested cost_value
  if (drift_dm_obj$cost_function == "neg_log_like") {
    cost_value <- calc_log_like(
      pdfs = pdfs,
      t_vec = t_vec,
      obs_data = drift_dm_obj$obs_data
    )
    # if NULL leave as is (otherwise -1.0 * NULL yields numeric())
    if (!is.null(cost_value)) {
      cost_value <- -1.0 * cost_value
    }
  }

  if (drift_dm_obj$cost_function == "rmse") {
    cost_value <- calc_rmse_eval(
      pdfs = pdfs,
      t_vec = t_vec,
      dt = dt,
      stats_agg = drift_dm_obj$stats_agg,
      stats_agg_info = drift_dm_obj$stats_agg_info
    )
  }

  drift_dm_obj$cost_value <- cost_value
  drift_dm_obj$pdfs <- pdfs
  return(drift_dm_obj)
}


#' Remove flags added when calling estimate_classical
#'
#' This is a small internal function that ensures that the flags added
#' after estimating a model via [dRiftDM::estimate_classical()] are removed. It
#' is used when replacing settings of a model after it has been estimated.
#'
#' @param drift_dm_obj a [dRiftDM::drift_dm] object
#'
#' @returns the model without the list entry `estimate_info`
#' @keywords internal
remove_estimate_info <- function(drift_dm_obj) {
  drift_dm_obj$estimate_info <- NULL
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
`flex_prms<-.drift_dm` <- function(object, ..., eval_model = FALSE, value) {
  object$flex_prms_obj <- value # object is the drift_dm object

  # ensure that everything is up-to-date
  object <- remove_estimate_info(object)
  object <- re_evaluate_model(drift_dm_obj = object, eval_model = eval_model)

  # ensure that nothing went wrong
  object <- validate_drift_dm(object)

  return(object)
}


### replace conds ####

#' @rdname conds
#' @export
`conds<-` <- function(object, ..., value) {
  UseMethod("conds<-")
}

#' @rdname conds
#' @export
`conds<-.drift_dm` <- function(
  object,
  ...,
  eval_model = FALSE,
  messaging = TRUE,
  value
) {
  # detach data (if present)
  msg_string <- "resetting parameter specifications"
  if (!is.null(object$obs_data)) {
    msg_string <- paste(msg_string, "and removing attached data from the model")
    object$obs_data <- NULL
  }

  if (messaging) {
    message(msg_string)
  }

  # create new flex_prms object
  first_row_vals <- object$flex_prms_obj$prms_matrix[1, ]
  new_flex_prms <- flex_prms(object = first_row_vals, conds = value, ...)
  flex_prms(object) <- new_flex_prms

  # ensure that everything is up-to-date
  object <- remove_estimate_info(object)
  object <- re_evaluate_model(drift_dm_obj = object, eval_model = eval_model)

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
`prms_solve<-.drift_dm` <- function(object, ..., eval_model = FALSE, value) {
  # silently strip away nt and nx
  value <- value[!(names(value) %in% c("nt", "nx"))]

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
  object <- remove_estimate_info(object)
  object <- re_evaluate_model(drift_dm_obj = object, eval_model = eval_model)

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
`solver<-.drift_dm` <- function(object, ..., eval_model = FALSE, value) {
  object <- set_one_solver_setting(
    drift_dm_obj = object,
    name_prm_solve = "solver",
    value_prm_solve = unname(value)
  )

  # ensure that everything is up-to-date
  object <- remove_estimate_info(object)
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
`obs_data<-.drift_dm` <- function(object, ..., eval_model = FALSE, value) {
  stopifnot(is.data.frame(value) | is.null(value))

  # object is the model object, value the data.frame
  if (is.null(value)) {
    object$obs_data <- value
  } else {
    # ensure that the conditions match
    if (!("Cond" %in% colnames(value))) {
      stop("No Cond column found in supplied data.frame")
    }
    model_conds <- conds(object)
    data_conds <- conds(value)

    if (!all(model_conds %in% data_conds)) {
      stop(
        "At least one of the model's conditions is not part of the Cond",
        " column of the provided data.frame"
      )
    }

    if (!all(data_conds %in% model_conds)) {
      warning(
        "The Cond column in the supplied data.frame provides a condition that",
        " is not listed in the model's conditions. This condition will be",
        " ignored"
      )
    }

    # unpack b_coding
    b_coding <- attr(object, "b_coding")

    # add rts to the model (select only those conditions that are in the model)
    obs_data_rt_list <- obs_data_to_rt_lists(
      obs_data = value,
      b_coding = b_coding
    )
    obs_data_rt_list <- lapply(obs_data_rt_list, function(one_rts_list) {
      return(one_rts_list[model_conds])
    })
    object$obs_data <- obs_data_rt_list
  }

  # create/update stats_agg
  object <- update_stats_agg(
    drift_dm_obj = object,
    which_cost_function = cost_function(object),
    ...
  )

  # ensure that everything is up-to-date
  object <- remove_estimate_info(object)
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
`comp_funs<-.drift_dm` <- function(object, ..., eval_model = FALSE, value) {
  # user input checks object is the model, value the list of functions
  if (!is.list(value)) {
    stop("provided input is not a list")
  }
  if (length(value) == 0) {
    stop("provided input is empty")
  }
  if (is.null(names(value))) {
    stop("entries in input are not named")
  }

  names_all_funs <- names(value)

  # iterate over the list
  for (one_fun_name in names_all_funs) {
    # ensure a reasonable name
    one_fun_name <- match.arg(
      one_fun_name,
      choices = c(
        "mu_fun",
        "mu_int_fun",
        "x_fun",
        "b_fun",
        "dt_b_fun",
        "nt_fun"
      )
    )
    if (!is.function(value[[one_fun_name]])) {
      stop(one_fun_name, " in input is not a function")
    }
    # set the function
    object$comp_funs[[one_fun_name]] <- value[[one_fun_name]]
  }

  # ensure that everything is up-to-date
  object <- remove_estimate_info(object)
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
    value <- drift_dm_default_b_coding()
  }

  # objectx is model, value the b_coding
  attr(object, "b_coding") <- check_b_coding(value)

  # ensure that everything is up-to-date not necessary, as b_encoding
  # is irrelevant for deriving PDFs

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
`coef<-.drift_dm` <- function(object, ..., eval_model = FALSE, value) {
  # some input checks
  # find the maximum number of parameters
  n_prms <- get_number_prms(object$flex_prms_obj)
  if (length(value) != n_prms) {
    stop("input vector has an unexpected number of entries")
  }

  # resort if named numeric vector
  if (!is.null(names(value))) {
    exp_names <- names(coef(object))
    value <- value[exp_names]
  }

  # check if valid numerics
  if (!is_numeric(value)) {
    stop(
      "value does not provide valid values. Check the names and values of ",
      "the supplied vector"
    )
  }

  # object is the model, value a numeric vector
  object$flex_prms_obj <- x2prms_vals(
    x = unname(value),
    flex_prms_obj = object$flex_prms_obj
  )

  # ensure that everything is up-to-date
  object <- remove_estimate_info(object)
  object <- re_evaluate_model(drift_dm_obj = object, eval_model = eval_model)

  # ensure that nothing went wrong
  object <- validate_drift_dm(object)

  return(object)
}


## replace ddm_opts ####

#' @rdname ddm_opts
#' @export
`ddm_opts<-` <- function(object, ..., value) {
  UseMethod("ddm_opts<-")
}

#' @rdname ddm_opts
#' @export
`ddm_opts<-.drift_dm` <- function(object, ..., eval_model = FALSE, value) {
  # attach it to the model
  object$ddm_opts <- value

  # ensure that everything is up-to-date
  object <- remove_estimate_info(object)
  object <- re_evaluate_model(drift_dm_obj = object, eval_model = eval_model)

  # ensure that nothing went wrong
  object <- validate_drift_dm(object)

  return(object)
}


## replace cost_function #####

#' @rdname cost_function
#' @export
`cost_function<-` <- function(object, ..., value) {
  UseMethod("cost_function<-")
}

#' @rdname cost_function
#' @export
`cost_function<-.drift_dm` <- function(object, ..., eval_model = FALSE, value) {
  value <- match.arg(value, drift_dm_cost_functions())

  # attach it to the model
  object$cost_function <- value

  # create/update stats_agg
  object <- update_stats_agg(
    drift_dm_obj = object,
    which_cost_function = value,
    ...
  )

  # ensure that everything is up-to-date
  object <- remove_estimate_info(object)
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
#' @returns the updated un-evaluated (!) drift_dm_obj object
#'
#' @keywords internal
set_one_solver_setting <- function(
  drift_dm_obj,
  name_prm_solve,
  value_prm_solve
) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }

  # input checks
  if (!is.character(name_prm_solve)) {
    stop("name_prm_solve must be of type character")
  }

  name_prm_solve <- match.arg(
    name_prm_solve,
    c("solver", "sigma", "t_max", "dx", "dt")
  )

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
    if (value_prm_solve == "im_zero") {
      warning(
        "When solver = 'im_zero', use a small 'dt'; 'im_zero' does not yet ",
        "support dynamic time stepping. It will usually run slower than the ",
        "'kfe' solver."
      )
    }
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

    # check if the time line works out
    dt <- prms_solve[["dt"]]
    t_max <- prms_solve[["t_max"]]
    check <- round((t_max / dt), 1.e8) %% 1 < drift_dm_approx_error()
    if (!check) {
      t_max_new <- ceiling(t_max / dt) * dt
      message(
        "Current `t_max` (",
        t_max,
        ") is not an integer multiple of `dt` (",
        dt,
        "). Adjusting `t_max` to ",
        t_max_new,
        " to ensure consistency. ",
        "Please check if this is fine with you."
      )
      prms_solve[["t_max"]] <- t_max_new
    }
    prms_solve["nt"] <- as.integer(
      prms_solve[["t_max"]] / prms_solve[["dt"]] + 1.e-8
    )
    # re_calc t_max to ensure tmax/nt works out
    prms_solve[["t_max"]] <- prms_solve[["nt"]] * dt
    drift_dm_obj$prms_solve <- prms_solve
  }

  # if desired, set dx
  if (name_prm_solve == "dx") {
    prms_solve <- drift_dm_obj$prms_solve
    dx <- value_prm_solve
    span <- 2.0 # standardized evidence range length: [-1;1] => 2
    tol <- drift_dm_approx_error()
    q <- span / dx # desired number of intervals

    if (abs(q - round(q)) >= tol) {
      pos_dxs <- 1:9999 / 10000
      pos_dxs <- pos_dxs[(span / pos_dxs) %% 1 == 0]
      dx_new <- pos_dxs[which.min(abs(pos_dxs - dx))]
      message(
        "`dx` (",
        dx,
        ") does not divide the standardized evidence space ",
        "([-1;1]) evenly. Adjusting `dx` to ",
        dx_new,
        ". Please check ",
        "if this is fine with you."
      )
      prms_solve[["dx"]] <- dx_new
    } else {
      prms_solve[["dx"]] <- dx
    }

    prms_solve["nx"] <- as.integer(2.0 / prms_solve["dx"] + 1.e-8)
    drift_dm_obj$prms_solve <- prms_solve
  }

  return(drift_dm_obj)
}


#' Update aggregated statistics in a `drift_dm` object
#'
#' Internal function that creates or updates the aggregated statistics
#' (`stats_agg` and `stats_agg_info`) in a [dRiftDM::drift_dm] object, depending
#' on the specified cost function. For maximum likelihood estimation
#' (`"neg_log_like"`), aggregated statistics are removed, because the raw
#' RTs are used directly.
#'
#' @param drift_dm_obj a `drift_dm` object.
#' @param which_cost_function a character string, indicating which cost function
#'   is used. Must be one of [dRiftDM::drift_dm_cost_functions()].
#' @param probs optional numeric vector of probabilities for quantile
#'   calculation. If `NULL`, defaults are taken from
#'   [dRiftDM::drift_dm_default_probs()].
#' @param n_bins an optional integer, giving the number of bins for the CAFs.
#'   If `NULL`, defaults are taken from [dRiftDM::drift_dm_default_n_bins()].
#'
#' @returns the input `drift_dm_obj`, with its `stats_agg` and
#'   `stats_agg_info` entries updated or removed, depending on the cost
#'   function and availability of observed data
#'
#' @note This function is called by [dRiftDM::obs_data()] and
#'   [dRiftDM::cost_function()]
#'
#' @keywords internal
#'
#' @seealso [dRiftDM::obs_data()], [dRiftDM::cost_function()],
#'   [dRiftDM::drift_dm()]
update_stats_agg <- function(
  drift_dm_obj,
  which_cost_function,
  probs = NULL,
  n_bins = NULL
) {
  # perform input checks
  which_cost_function <- match.arg(
    which_cost_function,
    drift_dm_cost_functions()
  )

  # functions to check the probs and n_bins arguments (for easier reuse)
  check_probs <- function(probs) {
    if (!is.numeric(probs) | length(probs) < 2) {
      stop("probs must a valid numeric vector of length > 1")
    }
    if (min(probs) <= 0 | max(probs) >= 1) {
      stop("argument probs must be in the range ]0, 1[")
    }
    return(probs)
  }

  check_n_bins <- function(n_bins) {
    if (!is_numeric(n_bins) | length(n_bins) != 1) {
      stop("n_bins must a valid numeric")
    }
    if (n_bins <= 1) {
      stop("n_bins nust be larger than 1")
    }
    return(n_bins)
  }

  # if negative log_like -> remove stats_agg (because ML uses raw rts,
  # not some summary stats)
  if (which_cost_function == "neg_log_like") {
    drift_dm_obj$stats_agg_info <- NULL
    drift_dm_obj$stats_agg <- NULL
    return(drift_dm_obj)
  }

  # if no data is set to the model, remove stats_agg and stats_agg_info and
  # return the model
  if (is.null(drift_dm_obj$obs_data)) {
    drift_dm_obj$stats_agg_info <- NULL
    drift_dm_obj$stats_agg <- NULL
    return(drift_dm_obj)
  }

  all_conds <- conds(drift_dm_obj)

  # create or update the stats_agg_info entry
  if (which_cost_function == "rmse") {
    # if it doesn't exist, create it
    if (is.null(drift_dm_obj$stats_agg_info)) {
      if (is.null(probs)) {
        probs <- drift_dm_default_probs()
      }
      probs <- check_probs(probs)

      if (is.null(n_bins)) {
        n_bins <- drift_dm_default_n_bins()
      }
      n_bins <- check_n_bins(n_bins)

      stats_agg_info <- sapply(
        all_conds,
        \(one_cond) {
          return(list(n_bins = n_bins, probs_corr = probs))
        },
        simplify = FALSE,
        USE.NAMES = TRUE
      )
      drift_dm_obj$stats_agg_info <- stats_agg_info
    }

    # if it exists, update it
    # update n_bins when provided
    if (!is.null(n_bins)) {
      n_bins <- check_n_bins(n_bins)
      for (one_cond in all_conds) {
        drift_dm_obj$stats_agg_info[[one_cond]]$n_bins <- n_bins
      }
    }

    # update probs when provided
    if (!is.null(probs)) {
      probs <- check_probs(probs)
      for (one_cond in all_conds) {
        drift_dm_obj$stats_agg_info[[one_cond]]$probs_corr <- probs
      }
    }
  }

  # (re-)create the stats_agg_info entry
  if (which_cost_function == "rmse") {
    stats_agg <- sapply(
      all_conds,
      \(one_cond) {
        rts_u <- drift_dm_obj$obs_data$rts_u[[one_cond]]
        rts_l <- drift_dm_obj$obs_data$rts_l[[one_cond]]

        quantiles <- calc_quantiles_obs(
          rts_u = rts_u,
          rts_l = rts_l,
          one_cond = one_cond,
          probs = drift_dm_obj$stats_agg_info[[one_cond]]$probs
        )[["Quant_U"]]

        cafs <- calc_cafs_obs(
          rts_u = rts_u,
          rts_l = rts_l,
          one_cond = one_cond,
          n_bins = drift_dm_obj$stats_agg_info[[one_cond]]$n_bins
        )[["P_U"]]

        return(list(quantiles_corr = quantiles, cafs = cafs))
      },
      simplify = FALSE,
      USE.NAMES = TRUE
    )
    drift_dm_obj$stats_agg <- stats_agg
  }

  # return the model
  return(drift_dm_obj)
}

# ===== FUNCTIONS THAT WERE PRIMARILY WRITTEN FOR SET FUNCTIONS ================

#' Check and Reduce the Observed Data
#'
#' Checks a data set that is considered an "observed data set". Used in the
#' internals of dRiftDM. When calling this function, unncessary column names
#' are stripped away.
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
#'   * if "RT", `b_coding_column`, and "Cond" column are present
#'   * when IDs are present, if each ID has values on each condition. At the same
#'   time unused factor levels are dropped [dRiftDM::drop_levels_ID_column]
#'   * If all columns are there, the data set is reduced to the relevant ones
#'   * for missing Values, and drops rows with missing values
#'   * if "Cond" is of type character, and if not casts it to character
#'   * if RT is of type numeric, and of not casts it to numeric
#'   * RTs are >= 0
#'   * that the values in `b_coding_column` match with u_value and l_value
#'     (casts the column if necessary)
#'   * if `b_coding_column` has only 1 or 2 unique values
#'
#' @keywords internal
check_reduce_raw_data <- function(obs_data, b_coding_column, u_value, l_value) {
  if (!is.data.frame(obs_data)) {
    stop("obs_data argument is not a data frame")
  }

  # check if the provided data.frame provides all necessary things
  if (!("RT" %in% colnames(obs_data))) {
    stop("no RT column in data frame")
  }
  if (!(b_coding_column %in% colnames(obs_data))) {
    stop("no ", b_coding_column, " column in data frame")
  }
  if (!("Cond" %in% colnames(obs_data))) {
    stop("no Cond column in data frame")
  }

  # check if there is an ID column
  id_present <- "ID" %in% colnames(obs_data)

  # reduce to relevant cols
  obs_data <- obs_data[c(if (id_present) "ID", "RT", b_coding_column, "Cond")]

  # check for missing values and drop them
  n_prev <- nrow(obs_data)
  obs_data <- stats::na.omit(obs_data)
  if (nrow(obs_data) != n_prev) {
    warning("Found missing values, removed automatically.")
  }

  # check if each subject provides observations for all conditions
  if (id_present) {
    obs_data <- drop_levels_ID_column(obs_data) # drops unused factor levels
    id_cond_table <- table(obs_data$ID, obs_data$Cond)
    idx_0 <- which(id_cond_table == 0, arr.ind = TRUE)

    if (nrow(idx_0) > 0) {
      ids <- rownames(id_cond_table)[idx_0[, 1]]
      n_ids <- length(ids)
      add <- ""
      if (n_ids > drift_dm_n_id_trunc_warn()) {
        n_ids <- drift_dm_n_id_trunc_warn()
        add <- ", (and more)"
      }
      which_ids <- paste(paste(ids[1:n_ids], collapse = ", "), add)
      stop("ID(s) ", which_ids, "do not provide RTs for all conditions")
    }
  }

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
  if (min(obs_data$RT) < 0) {
    stop("RTs are not >= 0")
  }

  # check if the data type and value of u_value matches with the values
  # observed in the b coding column. If not, tries to cast the column to
  # the class of u_value
  if (!isTRUE(all.equal(class(u_value), class(l_value)))) {
    stop("u_value and l_value must be of the same type")
  }

  type_obs_b_coding <- class(obs_data[[b_coding_column]])
  type_b_coding <- class(u_value)

  if (!isTRUE(all.equal(type_obs_b_coding, type_b_coding))) {
    warning(
      "column ",
      b_coding_column,
      " in obs_data expected to be of type ",
      type_b_coding,
      ", but it is of type ",
      type_obs_b_coding,
      " trying to fix this by using as.character(), followed by as.",
      type_b_coding,
      "()"
    )
    # cast to character (in case someone supplies a factor)
    obs_data[[b_coding_column]] <- as.character(obs_data[[b_coding_column]])
    as_function <- get(paste0("as.", type_b_coding))
    obs_data[[b_coding_column]] <- as_function(obs_data[[b_coding_column]])
  }

  # check if there are only one or two entries in b_cond_column and that these
  # match with u_value and l_value
  unique_b_obs <- unique(obs_data[[b_coding_column]])
  if (!(length(unique_b_obs) %in% c(1, 2))) {
    stop("Only one or two unique values in ", b_coding_column, " are allowed")
  }

  if (!all(unique_b_obs %in% c(u_value, l_value))) {
    stop(
      b_coding_column,
      " column should only contain ",
      u_value,
      " or ",
      l_value
    )
  }
  return(obs_data)
}


#' Maybe droplevels of ID column
#'
#' This function takes a data frame with an ID colmumn, and drops the unused
#' levels from the ID column if it is factor; in this case a warning is
#' thrown
#'
#' @param some_data a data.frame with an ID column
#'
#' @returns
#'
#' if the ID column is not of type factor, then the unmodified object is
#' returned.
#'
#' if the ID column is of type factor, [droplevels] is applied, and if levels
#' were dropped, a warning is thrown
#'
#' @keywords internal
drop_levels_ID_column <- function(some_data) {
  stopifnot(is.data.frame(some_data))
  stopifnot("ID" %in% colnames(some_data))

  if (!is.factor(some_data$ID)) {
    return(some_data)
  }

  n_prev <- length(levels(some_data$ID))
  some_data$ID <- droplevels(some_data$ID)
  n_after <- length(levels(some_data$ID))
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
#' performs checks on `b_coding` ([dRiftDM::check_b_coding]) and checks/reduces
#' `obs_data` ([dRiftDM::check_reduce_raw_data]) before disassembling the data
#' set.
#'
#' @returns
#' a list of rts with entries
#'
#'  * rts_u -> containing a list of numeric vectors, with names according to the
#'             values in Cond
#'  * rts_l -> containing a list of numeric vectors, with names according to the
#'             values in Cond
#'
#' @keywords internal
obs_data_to_rt_lists <- function(obs_data, b_coding = NULL) {
  # set default
  if (is.null(b_coding)) {
    b_coding <- drift_dm_default_b_coding()
  }

  # check if everything is ok
  # b_coding <- check_b_coding(b_coding)
  b_column <- b_coding$column
  u_name_value <- b_coding$u_name_value
  l_name_value <- b_coding$l_name_value

  # check the data to ensure everything is there and reduce to relevant cols
  obs_data <- check_reduce_raw_data(
    obs_data,
    b_coding_column = b_column,
    u_value = u_name_value,
    l_value = l_name_value
  )

  # get all conditions in the data frame and iterate through all conds....
  all_conds <- unique(obs_data$Cond)
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
#' @keywords internal
check_b_coding <- function(b_coding) {
  # check general outline of b_coding
  if (!is.list(b_coding)) {
    stop("b_coding is not a list")
  }

  if (length(b_coding) != 3) {
    stop("each b_coding list must provide three entries")
  }

  exp_names <- c("column", "u_name_value", "l_name_value")
  if (!all(names(b_coding) %in% exp_names)) {
    stop(
      "unexpected entries in b_coding. Expected: column, u_name_value,",
      " l_name_value. Found: ",
      paste(names(b_coding), collapse = ", ")
    )
  }

  # check for reasonable column value
  column <- b_coding$column
  if (!is.character(column) | length(column) != 1) {
    stop("column entry in b_coding must be a character vector of length 1")
  }

  # check for data type of u_name_val and l_name_value
  u_name_value <- b_coding$u_name_value
  l_name_value <- b_coding$l_name_value

  name_u <- names(u_name_value)
  name_l <- names(l_name_value)

  if (is.null(name_l) | is.null(name_u)) {
    stop("u_name_value and l_name_value must be named")
  }
  if (!is_numeric(u_name_value) & !is.character(u_name_value)) {
    stop("u_name_value must be either of type character or a valid numeric")
  }
  if (!is_numeric(l_name_value) & !is.character(l_name_value)) {
    stop("l_name_value must be either of type character or a valid numeric")
  }
  if (length(l_name_value) != 1 | length(u_name_value) != 1) {
    stop("l_name_value and u_name_value must be of length 1")
  }
  if (!isTRUE(all.equal(class(u_name_value), class(l_name_value)))) {
    stop("u_name_value and l_name_value must be of the same type")
  }
  if (is_empty(u_name_value) | is_empty(l_name_value)) {
    stop(
      "u_name_value and l_name_value can't be empty.",
      " Check if you have entered an empty string or an empty numeric."
    )
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
#' @param ... additional arguments passed forward.
#' @param eval_model logical, indicating if the model should be re-evaluated or
#'  not when updating the conditions (see [dRiftDM::re_evaluate_model]).
#'  Default is `FALSE`.
#' @param messaging logical, indicating if messages shall be displayed or not.
#' @param value a character vector, providing labels for the model's new
#'  conditions.
#'
#' @details
#' `conds()` is a generic accessor function and `conds<-()` is a
#' generic replacement function. The replacement method currently only supports
#' [dRiftDM::drift_dm] objects. The default methods get and set the conditions
#' of an object.
#'
#' When replacing the conditions of a [dRiftDM::drift_dm] object, a
#' new [dRiftDM::flex_prms] object is created and then set to the model,
#' resetting all parameter specifications and setting all parameter
#' values to those of the previously first condition.
#' In addition, if data was attached to the model, the data is removed.
#' This is because there is no meaningful way for dRiftDM to know how the model
#' should behave for the newly introduced condition(s), and how these new
#' conditions relate to the old ones. Messages reminding the user of this
#' behavior are displayed per default.
#'
#' @returns
#' For `conds()` `NULL` or a character vector with the conditions. `NULL` is
#' given if the object has no conditions (e.g., when a data.frame has no `Cond`
#' column).
#'
#' For `conds<-()` the updated [dRiftDM::drift_dm] object.
#'
#'
#' @examples
#' # get a pre-built model to demonstrate the conds() function
#' my_model <- dmc_dm()
#' conds(my_model)
#'
#' # accessor functions also work with other object types provided by dRiftDM
#' # (simulated traces; see the documentation of the respective function)
#' some_traces <- simulate_traces(my_model, k = 1)
#' conds(some_traces)
#'
#' # get an exemplary fits_ids_dm object (see estimate_model_ids)
#' fits <- get_example_fits("fits_ids_dm")
#' conds(fits)
#'
#' # also works with data.frames that have a "Cond" column
#' conds(dmc_synth_data)
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
conds.fits_agg_dm <- function(object, ...) {
  return(conds(object$drift_dm_obj))
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
#' @param object an object of type [dRiftDM::drift_dm], `fits_ids_dm`, or
#' `fits_agg_dm` (see [dRiftDM::estimate_dm()]).
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
#' (no explicit error/warning etc. is thrown).
#'
#' @returns
#' For `prms_solve()` the vector `prms_solve` (see [dRiftDM::drift_dm()]).
#'
#' For `prms_solve<-()` the updated [dRiftDM::drift_dm] object.
#'
#' @note
#' There is only a replacement function for [dRiftDM::drift_dm] objects. This is
#' because replacing the solver settings after the model has been fitted (e.g.,
#' for a `fits_ids_dm` object) doesn't make sense.
#'
#' @examples
#' # get some default model to demonstrate the prms_solve() functions
#' my_model <- ratcliff_dm()
#' # show the discretization and scaling of the model
#' prms_solve(my_model)
#' # partially modify these settings
#' prms_solve(my_model)[c("dx", "dt")] <- c(0.005)
#' prms_solve(my_model)
#'
#' # accessor method also available for fits_ids_dm objects
#' # (see estimate_model_ids)
#' # get an exemplary fits_ids_dm object
#' fits <- get_example_fits("fits_ids_dm")
#' prms_solve(fits)
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

#' @rdname prms_solve
#' @export
prms_solve.fits_agg_dm <- function(object, ...) {
  prms_solve(object$drift_dm_obj)
}

### solver ####

#' The Solver for Deriving Model Predictions
#'
#' Functions to get or set the "solver" of an object. The "solver" controls
#' the method for deriving the model's first passage time (i.e., its predicted
#' PDFs).
#'
#' @param object an object of type [dRiftDM::drift_dm], `fits_ids_dm`, or
#' `fits_agg_dm` (see [dRiftDM::estimate_dm()]).
#'
#' @param ... additional arguments (i.e., `eval_model`).
#'
#' @param value a single character string, providing the new "solver" (i.e.,
#'  approach to derive the first passage time; see [dRiftDM::drift_dm()]).
#'
#' @param eval_model logical, indicating if the model should be re-evaluated or
#'  not when updating the solver (see [dRiftDM::re_evaluate_model]). Default is
#'  `FALSE`.
#'
#' @details
#' `solver()` is a generic accessor function, and `solver<-()` is a
#' generic replacement function. The default methods get and set the "solver".
#'
#' The "solver" indicates the approach with which the PDFs of a model are
#' calculated. Supported options are "kfe" and "im_zero" (method based on the
#' Kolmogorov-Forward-Equation or on integral equations, respectively). Note
#' that "im_zero" is only supported for models that assume a fixed starting
#' point from 0.
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
#' @examples
#' # get some default model to demonstrate the solver() functions
#' my_model <- ratcliff_dm()
#' solver(my_model)
#' # change to the integral approach
#' solver(my_model) <- "im_zero"
#' solver(my_model)
#'
#' # accessor method also available for fits_ids_dm objects
#' # (see estimate_model_ids)
#' # get an exemplary fits_ids_dm object
#' fits <- get_example_fits("fits_ids_dm")
#' solver(fits)
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

#' @rdname solver
#' @export
solver.fits_agg_dm <- function(object, ...) {
  solver(object$drift_dm_obj)
}


### obs_data ####

#' The Observed Data
#'
#' Functions to get or set the "observed data" of an object.
#'
#' @param object an object of type [dRiftDM::drift_dm], `fits_ids_dm`, or
#' `fits_agg_dm` (see [dRiftDM::estimate_dm()]).
#'
#' @param ... additional arguments passed down to the specific method.
#'
#' @param value a [data.frame] which provides three columns: (1) `RT` for
#'  the response times, (2) a column for boundary coding according to the
#'  model's [dRiftDM::b_coding()], (3) `Cond` for specifying the conditions.
#'
#' @param eval_model logical, indicating if the model should be re-evaluated or
#'  not when updating the solver settings (see [dRiftDM::re_evaluate_model]).
#'  Default is `FALSE`.
#'
#' @param messaging logical, indicating if messages shall be displayed or not.
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
#' ordering of the argument `value`. A message is displayed to remind the user
#' that the returned [data.frame] may be sorted differently than expected.
#'
#' @returns
#' For `obs_data()` a [data.frame] of the observed data. The method
#'  `obs_data.drift_dm()` per default displays a message to remind the user that
#' the returned [data.frame] is likely sorted differently than expected.
#'
#' For `obs_data<-()` the updated [dRiftDM::drift_dm] object.
#'
#' @note
#' There is only a replacement function for [dRiftDM::drift_dm] objects. This is
#' because replacing the observed data after the model has been fitted (i.e.,
#' for a `fits_ids_dm` object) doesn't make sense.
#'
#' @examples
#' # Set some data to a model -------------------------------------------------
#' my_model <- dmc_dm() # DMC is pre-built and directly available
#' # synthetic data suitable for DMC; comes with dRiftDM
#' some_data <- dmc_synth_data
#' obs_data(my_model) <- some_data
#'
#' # Extract data from a model ------------------------------------------------
#' head(obs_data(my_model))
#'
#' # Important: ---------------------------------------------------------------
#' # The returned data.frame may be sorted differently than the one initially
#' # supplied.
#' some_data <- some_data[sample(1:nrow(some_data)), ] #' # shuffle the data set
#' obs_data(my_model) <- some_data
#' all.equal(obs_data(my_model), some_data)
#' # so don't do obs_data(my_model)["Cond"] <- ...
#'
#' # Addition: ----------------------------------------------------------------
#' # accessor method also available for fits_ids_dm objects
#' # (see estimate_model_ids)
#' # get an exemplary fits_ids_dm object
#' fits <- get_example_fits("fits_ids_dm")
#' head(obs_data(fits))
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
obs_data.drift_dm <- function(object, ..., messaging = TRUE) {
  if (is.null(object$obs_data)) {
    return(NULL)
  }

  if (messaging) {
    message(
      "Extracting observed data from a model object. Remember that the",
      " result may be sorted differently than expect!"
    )
  }

  # rebuild the data frame
  model_conds <- conds(object)
  b_coding <- attr(object, "b_coding")
  b_column <- b_coding$column
  value_u <- unname(b_coding$u_name_value)
  value_l <- unname(b_coding$l_name_value)

  data_list <- lapply(model_conds, function(one_cond) {
    rts_u <- object$obs_data$rts_u[[one_cond]]
    rts_l <- object$obs_data$rts_l[[one_cond]]

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

# extracts saved observed data file
#' @rdname obs_data
#' @export
obs_data.fits_agg_dm <- function(object, ...) {
  return(object$obs_data_ids)
}


## comp_funs ######

#' The Component Functions of A Model
#'
#' Functions to get or set the "component functions" of an object. The component
#' functions are a list of functions providing the drift rate, boundary,
#' starting point distribution, and non-decision time distribution They are at
#' the heart of the package and shape the model's behavior.
#'
#' @param object an object of type [dRiftDM::drift_dm], `fits_ids_dm`, or
#' `fits_agg_dm` (see [dRiftDM::estimate_dm()]).
#'
#' @param ... additional arguments passed down to the specific method.
#'
#' @param value a named list which provides the component functions to set
#'  (see Details)
#'
#' @param eval_model logical, indicating if the model should be re-evaluated or
#'  not when updating the component funtions (see [dRiftDM::re_evaluate_model]).
#'  Default is `FALSE`.
#'
#' @details
#' `comp_funs()` is a generic accessor function, and `comp_funs<-()` is a
#' generic replacement function. The default methods get and set the "component
#' functions". The component functions are a list of functions, with the
#' following names (see also \code{vignette("customize_ddms", "dRiftDM")} for
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
#' @examples
#' # get a pre-built model for demonstration
#' my_model <- ratcliff_dm()
#' names(comp_funs(my_model))
#'
#' # direct replacement (see customize_ddms for a more information on
#' # how to write custom component functions)
#' # 1. Choose a uniform non-decision time from the pre-built component_shelf()
#' nt_uniform <- component_shelf()$nt_uniform
#' # swap it in
#' comp_funs(my_model)[["nt_fun"]] <- nt_uniform
#'
#' # now update the flex_prms object to ensure that this model has the required
#' # parameters
#' prms <- c(muc = 3, b = 0.6, non_dec = 0.3, range_non_dec = 0.05)
#' conds <- "null"
#' new_flex_prms <- flex_prms(prms, conds = conds)
#' flex_prms(my_model) <- new_flex_prms
#'
#' # accessor method also available for fits_ids_dm objects
#' # (see estimate_model_ids)
#' # get an exemplary fits_ids_dm object
#' fits <- get_example_fits("fits_ids_dm")
#' names(comp_funs(fits))
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

# extracts saved observed data file
#' @rdname comp_funs
#' @export
comp_funs.fits_agg_dm <- function(object, ...) {
  return(comp_funs(object$drift_dm_obj))
}


## b_coding ######

#' The Coding of the Boundaries
#'
#' Functions to get or set the "boundary coding" of an object.
#'
#' @param object an object of type [dRiftDM::drift_dm], `fits_ids_dm`, or
#' `fits_agg_dm` (see [dRiftDM::estimate_dm()]).
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
#' @returns
#' For `b_coding()` a list containing the boundary coding
#' For `b_coding<-()` the updated `drift_dm` or `fits_ids_dm` object
#'
#' @examples
#' # show the default accuracy coding of dRiftDM
#' my_model <- ratcliff_dm() # get a pre-built model
#' b_coding(my_model)
#'
#' # can be modified/replaced
#' b_coding(my_model)[["column"]] <- "Response"
#'
#' # accessor method also available for fits_ids_dm objects
#' # get an exemplary fits_ids_dm object (see estimate_model_ids)
#' fits <- get_example_fits("fits_ids_dm")
#' names(b_coding(fits))
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


#' @rdname b_coding
#' @export
b_coding.fits_agg_dm <- function(object, ...) {
  return(b_coding(object$drift_dm_obj))
}


## ddm_opts ######

#' Optional Arguments for the Component Functions
#'
#' Functions to get or set the optional, user-defined R objects attached
#' to a model object.
#'
#' @param object an object of type [dRiftDM::drift_dm] or
#' `fits_agg_dm` (see [dRiftDM::estimate_dm()]).
#'
#' @param ... additional arguments passed down to the specific method.
#'
#' @param value an arbitrary R object.
#'
#' @param eval_model logical, indicating if the model should be re-evaluated or
#'  not after attaching the arbitrary R object to the model
#'  (see [dRiftDM::re_evaluate_model]). Default is `FALSE`.
#'
#' @details
#'
#' When deriving model predictions, the model's component functions
#' (see [dRiftDM::comp_funs()]) are evaluated and the returned values are
#' passed forward to dedicated numerical methods implemented in dRiftDM.
#' To allow users to access arbitrary R objects within their custom component
#' functions, models may contain a `ddm_opts` entry (see also
#' [dRiftDM::drift_dm()] and the end of
#' \code{vignette("customize_ddms", "dRiftDM")} for an example).
#'
#' `ddm_opts()` is a generic accessor function, and `ddm_opts<-()` is a
#' generic replacement function. The default methods get and set the optional
#' R object.
#'
#' @returns
#' For `ddm_opts()` the optional R object that was once supplied by the user, or
#' `NULL`.
#'
#' For `ddm_opts<-()` the updated [dRiftDM::drift_dm] object.
#'
#'
#' @examples
#' # get a pre-built model for demonstration
#' a_model <- ratcliff_dm()
#' ddm_opts(a_model) <- "Hello World"
#' ddm_opts(a_model)
#'
#' @seealso [dRiftDM::drift_dm()], [dRiftDM::comp_funs()]
#'
#'
#' @export
ddm_opts <- function(object, ...) {
  UseMethod("ddm_opts")
}


# extract the R object
#' @rdname ddm_opts
#' @export
ddm_opts.drift_dm <- function(object, ...) {
  return(object$ddm_opts)
}


#' @rdname ddm_opts
#' @export
ddm_opts.fits_agg_dm <- function(object, ...) {
  ddm_opts(object$drift_dm_obj)
}


## pdfs ######

#' Access the Probability Density Functions of a Model
#'
#' Functions to obtain the probability density functions (PDFs) of a model.
#' These PDFs represent the convolution of the first-passage-time (decision
#' time) with the non-decision time.
#'
#' @param object an object of type [dRiftDM::drift_dm] or
#' `fits_agg_dm` (see [dRiftDM::estimate_dm()]).
#'
#' @param ... additional arguments passed down to the specific method.
#'
#'
#' @details
#'
#' If the model has not been evaluated, [dRiftDM::re_evaluate_model()] is
#' called before returning the PDFs.
#'
#' @returns
#' A list with the entries:
#' - `pdfs`, contains another named list with entries corresponding to the
#'   conditions of the model (see [dRiftDM::conds()]). Each of these elements
#'   is another named list, containing the entries `pdf_u` and `pdf_l`, which
#'   are numeric vectors for the PDFs of the upper and lower boundary,
#'   respectively.
#' - `t_vec`, containing a numeric vector of the time domain.
#'
#'
#' @examples
#' # get a pre-built model for demonstration purpose
#' a_model <- dmc_dm()
#' str(pdfs(a_model))
#'
#' @seealso [dRiftDM::drift_dm()], [dRiftDM::re_evaluate_model()],
#' [dRiftDM::conds()]
#'
#'
#' @export
pdfs <- function(object, ...) {
  UseMethod("pdfs")
}


# extract the pdfs
#' @rdname pdfs
#' @export
pdfs.drift_dm <- function(object, ...) {
  if (is.null(object$pdfs)) {
    object <- re_evaluate_model(object)
  }
  slvr <- prms_solve(object)
  t_vec <- seq(0, slvr["t_max"], length.out = slvr[["nt"]] + 1)
  returned_list <- list(
    pdfs = object$pdfs,
    t_vec = t_vec
  )
  return(returned_list)
}

#' @rdname pdfs
#' @export
pdfs.fits_agg_dm <- function(object, ...) {
  pdfs(object$drift_dm_obj)
}


## cost_function and cost value #####

#' Access/Replace the Cost Function Label and Access the Cost Function Value
#'
#' Functions to access/replace the cost function label of a `dRiftDM object` and
#' to access the current cost function value.
#' The cost function label codes which cost function is used during estimation
#' (e.g., the negative log-likelihood). The cost function value indicates the
#' current value of the cost function given the current set of parameters and
#' the data.
#'
#' @param object an object of type [dRiftDM::drift_dm], `fits_ids_dm`, or
#' `fits_agg_dm` (see [dRiftDM::estimate_dm()]).
#' @param eval_model logical, indicating if the model should be re-evaluated or
#'  not when updating the conditions (see [dRiftDM::re_evaluate_model]).
#'  Default is `FALSE`.
#' @param value a character string, providing the cost function label
#' (options are `"neg_log_like"` or `"rmse"`)
#' @param ... additional arguments passed down to [dRiftDM::update_stats_agg()]
#' when setting the cost function label.
#'
#'
#' @returns
#' - `cost_function()` returns a single character string, specifying the used
#'  cost function
#'
#' - `cost_function<-()` returns the model object with the updated cost
#'   function.
#'
#' - `cost_value()` returns a single numeric if `object` is of type `drift_dm`
#'   or `fits_agg_dm`. If there is no data attached to an object of type
#'   `drift_dm`, the function returns `NULL`. If `object` is of type
#'   `fits_ids_dm`, the function returns a [data.frame] with all cost values
#'   across participants.
#'
#'
#' @examples
#' # get a pre-built model for demonstration purpose
#' a_model <- ratcliff_dm(obs_data = ratcliff_synth_data)
#' cost_function(a_model)
#' cost_value(a_model)
#'
#' # switch the default cost function to rmse
#' cost_function(a_model) <- "rmse"
#' out <- estimate_dm(a_model, verbose = 0, messaging = FALSE)
#' # -> the model was estimated using the RMSE statistic
#'
#'
#' @seealso [dRiftDM::drift_dm()], [dRiftDM::re_evaluate_model()]
#'
#'
#' @export
cost_function <- function(object, ...) {
  UseMethod("cost_function")
}

#' @rdname cost_function
#' @export
cost_function.drift_dm <- function(object, ...) {
  return(object$cost_function)
}

#' @rdname cost_function
#' @export
cost_function.fits_ids_dm <- function(object, ...) {
  return(cost_function(object$drift_dm_fit_info$drift_dm_obj))
}

#' @rdname cost_function
#' @export
cost_function.fits_agg_dm <- function(object, ...) {
  return(cost_function(object$drift_dm_obj))
}


#' @rdname cost_function
#' @export
cost_value <- function(object, ...) {
  UseMethod("cost_value")
}

#' @rdname cost_function
#' @export
cost_value.drift_dm <- function(object, ...) {
  if (
    is.null(object$cost_value) &&
      (!is.null(object$obs_data) || !is.null(object$stats_agg))
  ) {
    object <- re_evaluate_model(object)
  }
  return(object$cost_value)
}

#' @rdname cost_function
#' @export
cost_value.fits_ids_dm <- function(object, ...) {
  all_vals <- sapply(object$all_fits, \(x) x$cost_value)
  res <- data.frame(ID = names(object$all_fits), Cost_Value = all_vals)
  res$ID <- try_cast_integer(res$ID)
  res <- res[order(res$ID), ]
  rownames(res) <- NULL

  return(res)
}

#' @rdname cost_function
#' @export
cost_value.fits_agg_dm <- function(object, ...) {
  cost_value(object$drift_dm_obj)
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
#' @param prms_solve optional, vector of solver settings
#' @param solver optional, string controlling which component values are
#' evaluated
#' @param prms_matrix optional, matrix of parameters
#'
#' @returns
#' If solver "kfe", a named list with entries "mu_vals", "x_vals", "b_vals",
#' "dt_b_vals", "nt_vals".
#'
#' If solver "im_zero", the returned list will also contain "mu_int_vals".
#'
#' @details
#' arguments are optional, because they can be extracted from the model.
#' However, supplying these are faster than creating them.
#'
#' @keywords internal
comp_vals <- function(
  drift_dm_obj,
  x_vec = NULL,
  t_vec = NULL,
  nt = NULL,
  dt = NULL,
  nx = NULL,
  dx = NULL,
  prms_solve = NULL,
  solver = NULL,
  prms_matrix = NULL
) {
  # unpack values
  if (is.null(nt)) {
    nt <- drift_dm_obj$prms_solve[["nt"]]
  }
  if (is.null(dt)) {
    dt <- drift_dm_obj$prms_solve[["dt"]]
  }

  if (is.null(nx)) {
    nx <- drift_dm_obj$prms_solve[["nx"]]
  }
  if (is.null(dx)) {
    dx <- drift_dm_obj$prms_solve[["dx"]]
  }

  if (is.null(x_vec)) {
    x_vec <- seq(-1, 1, length.out = nx + 1)
  }
  if (is.null(t_vec)) {
    t_vec <- seq(0, drift_dm_obj$prms_solve[["t_max"]], length.out = nt + 1)
  }

  if (is.null(prms_solve)) {
    prms_solve <- drift_dm_obj$prms_solve
  }

  if (is.null(solver)) {
    solver <- drift_dm_obj$solver
  }

  if (is.null(prms_matrix)) {
    prms_matrix <- drift_dm_obj$flex_prms_obj$prms_matrix
  }

  # get the functions to call
  if (solver == "kfe") {
    comp_fun_names <- c("mu_fun", "x_fun", "b_fun", "dt_b_fun", "nt_fun")
  } else if (solver == "im_zero") {
    comp_fun_names <- c(
      "mu_fun",
      "mu_int_fun",
      "x_fun",
      "b_fun",
      "dt_b_fun",
      "nt_fun"
    )
  } else {
    stop("requested solver ", solver, " not implemented")
  }

  # get conds, ddm_opts, and comp_funs
  conds <- rownames(prms_matrix)
  ddm_opts <- drift_dm_obj$ddm_opts
  comp_funs <- drift_dm_obj$comp_funs

  # iterate over conds and get all model components
  all_comp_vecs <- sapply(
    conds,
    function(one_cond) {
      prms_model <- prms_matrix[one_cond, ]

      one_set_comp_vecs <- sapply(
        comp_fun_names,
        function(name_comp_fun) {
          if (name_comp_fun == "x_fun") {
            vals <- comp_funs[[name_comp_fun]](
              prms_model,
              prms_solve,
              x_vec,
              one_cond,
              ddm_opts
            )
          } else {
            vals <- comp_funs[[name_comp_fun]](
              prms_model,
              prms_solve,
              t_vec,
              one_cond,
              ddm_opts
            )
          }

          # checks
          # for all: numeric values and no nas or Infs
          if (!is.numeric(vals)) {
            stop(
              "function for ",
              name_comp_fun,
              " provided non-numeric values, condition ",
              one_cond
            )
          }

          if (any(is.infinite(vals)) | any(is.na(vals))) {
            stop(
              "function for ",
              name_comp_fun,
              " provided infinite values or NAs, condition ",
              one_cond
            )
          }

          # for boundary and densities, no negative values
          if (name_comp_fun %in% c("nt_fun", "x_fun", "b_fun")) {
            if (min(vals) < 0) {
              stop(
                "function for ",
                name_comp_fun,
                " provided negative values, ",
                "condition ",
                one_cond
              )
            }
          }

          # for densities, must roughly integrate to 1
          if (name_comp_fun %in% c("nt_fun", "x_fun")) {
            if (name_comp_fun == "nt_fun") {
              discr <- dt
            }
            if (name_comp_fun == "x_fun") {
              discr <- dx
            }

            if (abs(sum(vals) * discr - 1) > drift_dm_medium_approx_error()) {
              stop(
                "function for ",
                name_comp_fun,
                " doesn't integrate to 1, ",
                "condition ",
                one_cond
              )
            }
          }

          length_check <- ifelse(name_comp_fun == "x_fun", nx + 1, nt + 1)
          if (length(vals) != length_check) {
            stop(
              "function for ",
              name_comp_fun,
              " provided an unexpected ",
              "number of values, condition ",
              one_cond
            )
          }

          # for starting values, no zeros on the edges
          if (name_comp_fun == "x_fun") {
            if (vals[1] != 0 || vals[nx + 1] != 0) {
              stop(
                "function for ",
                name_comp_fun,
                " provides starting values that are at the decision boundary, ",
                "condition ",
                one_cond
              )
            }
          }
          return(vals)
        },
        USE.NAMES = TRUE,
        simplify = FALSE
      )
      names(one_set_comp_vecs) <- sub(
        pattern = "fun",
        replacement = c("vals"),
        x = names(one_set_comp_vecs)
      )
      return(one_set_comp_vecs)
    },
    USE.NAMES = TRUE,
    simplify = FALSE
  )

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
#' @keywords internal
prms_cond_combo <- function(drift_dm_obj) {
  # get all prm_conds that are not 0 or an expression
  linear_list <- drift_dm_obj$flex_prms_obj$linear_internal_list
  flatten_linear <- lapply(linear_list, function(x) {
    lapply(x, function(y) {
      if (check_digit_larger_0(y)) {
        return(y)
      } else {
        return(NULL)
      }
    })
  })

  # unlist and drop duplicate values
  flatten_linear <- unlist(flatten_linear)
  flatten_linear <- flatten_linear[!duplicated(flatten_linear)]
  stopifnot(!is.unsorted(flatten_linear))

  # get labels by splitting the colnames of the flattened list
  prm_conds <- sapply(names(flatten_linear), \(x) strsplit(x, "\\.")[[1]])
  prm_conds <- as.matrix(prm_conds)
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
#' @param object an object of type [dRiftDM::drift_dm], `fits_ids_dm`, or
#' `fits_agg_dm` (see [dRiftDM::estimate_dm()]).
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
#'  also [dRiftDM::unpack_obj] and the return value below).
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
#' `simulate_traces()` returns either an object of type `traces_dm_list`, or
#' directly a list of matrices across conditions, containing the traces
#' (if `unpack = TRUE`).
#' If the model has only one condition (and `unpack = TRUE`), then the matrix of
#' traces for this one condition is directly returned.
#'
#' The returned list has as many entries as conditions requested. For example,
#' if only one condition is requested via the `conds` argument, then the list is
#' of length 1 (if `unpack = FALSE`). If `conds` is set to `NULL` (default),
#' then the list will have as many entries as conditions specified in the
#' supplied `object` (see also [dRiftDM::conds]). If `unpack = FALSE`, the list
#' contains an additional attribute with the time space.
#'
#' Each matrix of traces has `k` rows and `nt + 1` columns, stored as an
#' array of size (`k`, `nt + 1`). Note that `nt` is the number of steps in the
#' discretization of time; see [dRiftDM::drift_dm]. If `unpack = FALSE`, the
#' array is of type `traces_dm`. It contains some additional attributes about
#' the time space, the drift rate, the boundary, the added starting values,
#' if starting values were added, the original model class and parameters, the
#' boundary coding, and the solver settings.
#'
#' The print methods `print.traces_dm_list()` and `print.traces_dm()` each
#' invisibly return the supplied object `x`.
#'
#' @examples
#' # get a pre-built model to demonstrate the function
#' my_model <- dmc_dm()
#' some_traces <- simulate_traces(my_model, k = 1, seed = 1)
#' print(some_traces)
#'
#' # a method is also available for fits_ids_dm objects
#' # (see estimate_model_ids)
#' # get an exemplary fits_ids_dm object
#' fits <- get_example_fits("fits_ids_dm")
#' some_traces <- simulate_traces(fits, k = 1, seed = 1)
#' print(some_traces)
#'
#' # we can also print only the traces of one condition
#' print(some_traces$comp)
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
#' [dRiftDM::unpack_obj()].
#'
#' @seealso [dRiftDM::unpack_obj()], [dRiftDM::plot.traces_dm_list()]
#'
#'
#' @export
simulate_traces <- function(object, k, ...) {
  UseMethod("simulate_traces")
}

#' @rdname simulate_traces
#' @export
simulate_traces.drift_dm <- function(
  object,
  k,
  ...,
  conds = NULL,
  add_x = FALSE,
  sigma = NULL,
  seed = NULL,
  unpack = FALSE
) {
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
  ks <- k
  if (length(ks) == 1) {
    ks <- rep(ks, length(conds))
  }

  if (length(ks) != length(conds)) {
    stop("number of values in k must match with the number of conditions")
  }

  if (is.null(names(ks))) {
    names(ks) <- conds
  }

  if (!all(conds %in% names(ks))) {
    stop("names in k don't match with the names for each condition")
  }

  # get and check the add_x (logical check done in simulate_one_traces)
  add_xs <- add_x
  if (length(add_xs) == 1) {
    add_xs <- rep(add_xs, length(conds))
  }

  if (length(add_xs) != length(conds)) {
    stop("number of values in add_x must match with the number of conditions")
  }

  if (is.null(names(add_xs))) {
    names(add_xs) <- conds
  }

  if (!all(conds %in% names(add_xs))) {
    stop("names in add_x don't match with the names for each condition")
  }

  # get and check the sigma (numeric check done in simulate_one_traces)
  if (is.null(sigma)) {
    sigma <- object$prms_solve[["sigma"]]
  }

  sigmas <- sigma
  if (length(sigmas) == 1) {
    sigmas <- rep(sigmas, length(conds))
  }

  if (length(sigmas) != length(conds)) {
    stop("number of values in sigma must match with the number of conditions")
  }

  if (is.null(names(sigmas))) {
    names(sigmas) <- conds
  }

  if (!all(conds %in% names(sigmas))) {
    stop("names in sigma don't match with the names for each condition")
  }

  # call internal function per conditions and pass back
  all_samples <- sapply(
    conds,
    function(one_cond) {
      simulate_traces_one_cond(
        drift_dm_obj = object,
        k = unname(ks[one_cond]),
        one_cond = one_cond,
        add_x = unname(add_xs[one_cond]),
        sigma = unname(sigmas[one_cond])
      )
    },
    USE.NAMES = TRUE,
    simplify = FALSE
  )

  # give it a class
  class(all_samples) <- c("traces_dm_list")

  # save the time vector as it is not condition dependent
  attr(all_samples, "t_vec") <- attr(all_samples[[1]], "t_vec")

  # unpack if desired
  if (unpack) {
    all_samples <- unpack_obj(all_samples, unpack_elements = unpack)
  }

  # and pass back
  return(all_samples)
}

#' @rdname simulate_traces
#' @export
simulate_traces.fits_ids_dm <- function(object, k, ...) {
  # get mean parameter values
  all_coefs <- coef(object, select_unique = TRUE)
  all_coefs <- all_coefs[, colnames(all_coefs) != "ID"]
  mean_coefs <- colMeans(all_coefs)

  # stick them into the model
  dm_obj <- object$drift_dm_fit_info$drift_dm_obj
  coef(dm_obj) <- mean_coefs

  # simulate and pass back
  traces_obj <- simulate_traces(dm_obj, k = k, ...)
  return(traces_obj)
}


#' @rdname simulate_traces
#' @export
simulate_traces.fits_agg_dm <- function(object, k, ...) {
  traces_obj <- simulate_traces(object$drift_dm_obj, k = k, ...)
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
#'   the traces in the array.
#'   * "add_x" -> boolean, indicating if the starting values were added or not
#'   * "orig_model_class" -> the class label of the original model
#'   * "orig_prms" -> the parameters with which the traces were simulated (for
#'      the respective condition)
#'   * "b_coding" -> the boundary coding
#'   * "prms_solve" -> the solver settings with which the traces were simulated
#'
simulate_traces_one_cond <- function(drift_dm_obj, k, one_cond, add_x, sigma) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }

  if (!is.logical(add_x) | length(add_x) != 1) {
    stop("add_x must be logical of length 1")
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

  if (!is_numeric(sigma) | sigma < 0) {
    stop("sigma must be a numeric >= 0")
  }

  # unpack arguments for easier usage
  dt <- drift_dm_obj$prms_solve[["dt"]]
  nt <- drift_dm_obj$prms_solve[["nt"]]
  nx <- drift_dm_obj$prms_solve[["nx"]]
  t_max <- drift_dm_obj$prms_solve[["t_max"]]

  # get component function's values
  all_vals <- comp_vals(drift_dm_obj)
  mu_vals <- all_vals[[one_cond]]$mu_vals
  b_vals <- all_vals[[one_cond]]$b_vals

  samp_x <- numeric(k) # storage for starting values
  if (add_x) {
    x_vec <- seq(-1, 1, length.out = nx + 1)
    x_vals <- all_vals[[one_cond]]$x_vals
    xx <- x_vec * b_vals[1]
    samp_x <- draw_from_pdf(a_pdf = x_vals, x_def = xx, k = k)
  }

  # check for variability in the drift rate
  var_drift <- FALSE
  prms_matrix <- drift_dm_obj$flex_prms_obj$prms_matrix
  prm_labels <- colnames(prms_matrix)
  if (any("sd_muc" == prm_labels)) {
    var_drift <- TRUE

    # and check if constant drift rate is used
    if (!identical(drift_dm_obj$comp_funs$mu_fun, mu_constant)) {
      stop(
        "Ratcliff DDM with variable drift rate requires dRiftDM's",
        " mu_constant function"
      )
    }

    # check for plausible value of sd
    sd_muc <- prms_matrix[one_cond, "sd_muc"]
    if (any(sd_muc < 0)) {
      stop("sd_muc values < 0 are not allowed")
    }
  }

  # now simulate values
  e_samples <-
    sapply(1:k, function(one_k) {
      if (var_drift) {
        mu_vals <- mu_vals + stats::rnorm(n = 1, mean = 0, sd = sd_muc)
      }
      steps <- mu_vals * dt + sigma * sqrt(dt) * stats::rnorm(nt + 1)
      acc_steps <- c(0, cumsum(steps)) + samp_x[one_k]
      acc_steps <- acc_steps[-length(acc_steps)] # discard last step
      idx_fP <- which(abs(acc_steps) >= b_vals)[1] # get first passage index
      if (is.na(idx_fP)) {
        warning("no boundary hit when simulating trial")
        return(acc_steps)
      }
      if (idx_fP > 0 & idx_fP < length(acc_steps)) {
        acc_steps[(idx_fP + 1):length(acc_steps)] <- NA_real_
      }
      return(acc_steps)
    })

  # make it a class and pass back (with several infos)
  e_samples <- t(e_samples)
  class(e_samples) <- "traces_dm"
  attr(e_samples, "t_vec") <- seq(0, t_max, length.out = nt + 1)
  attr(e_samples, "mu_vals") <- mu_vals
  attr(e_samples, "b_vals") <- b_vals
  attr(e_samples, "samp_x") <- samp_x
  attr(e_samples, "add_x") <- add_x
  attr(e_samples, "orig_model_class") <- class(drift_dm_obj)

  orig_prms <- flex_prms(drift_dm_obj)$prms_matrix[one_cond, ]
  attr(e_samples, "orig_prms") <- orig_prms
  attr(e_samples, "b_coding") <- b_coding(drift_dm_obj)
  temp_prms_solve <- drift_dm_obj$prms_solve
  temp_prms_solve["sigma"] <- sigma
  attr(e_samples, "prms_solve") <- temp_prms_solve

  return(e_samples)
}


#' Unpack/Destroy dRiftDM Objects
#'
#' @description
#'
#' When calling [dRiftDM::simulate_traces()], [dRiftDM::calc_stats], or
#' [dRiftDM::coef.fits_ids_dm] the returned objects will be custom objects
#' (e.g., subclasses of [list] or [data.frame]). The respective subclasses were
#' created to provide convenient plotting and printing, but they don't
#' really provide any additional functionality.
#'
#' The goal of `unpack_obj()` is to provide a convenient way to strip away
#' the attributes of the respective objects (revealing them as standard
#' [array]s, [data.frame]s, or [list]s).
#'
#' @param object an object of type `stats_dm`, `stats_dm_list`, `traces_dm`,
#'  `traces_dm_list`, or `coefs_dm`
#'
#' @param ... further arguments passed on to the respective method.
#'
#' @param unpack_elements logical, indicating if the `traces_dm`,
#'  `stats_dm`, or `coefs_dm` objects shall be unpacked. Default is `TRUE`.
#'
#' @param conds optional character vector, indicating specific condition(s). The
#' default `NULL` will lead to `conds = conds(object)`. Thus, per default all
#' conditions are addressed
#'
#' @param type optional character vector, indicating specific type(s) of
#' statistics. The default `NULL` will access all types of statics.
#'
#' @details
#' `unpack_obj()` is a generic function to strip away the custom information
#' and class labels of `stats_dm`, `stats_dm_list`, `traces_dm`,
#' `traces_dm_list`, and `coefs_dm` objects. These objects are created when
#' calling [dRiftDM::simulate_traces()], [dRiftDM::calc_stats], or
#' [dRiftDM::coef.fits_ids_dm].
#'
#' For `traces_dm_list`, `unpack_obj()` returns the
#' requested conditions (see the argument `conds`). The result contains
#' objects of type `traces_dm` if `unpack_elements = FALSE`. For
#' `unpack_elements = TRUE`, the result contains the plain [array]s with the
#' traces.
#'
#' For `stats_dm_list`, `unpack_obj()` returns the
#' requested statistics (see the argument `type`). The result contains
#' objects of type `stats_dm` if `unpack_elements = FALSE`. For
#' `unpack_elements = TRUE`, the result contains the plain [data.frame]s with
#' the statistics.
#'
#' @returns
#'
#' For `traces_dm_list`, the returned value is a list, if `conds` specifies more
#' than one condition. For example, if `conds = c("foo", "bar")`, then the
#' returned value is a list with the two (named) entries "foo" and "bar". If
#' the returned list would only have one entry (either because the
#' `traces_dm_list` has only one condition, see [dRiftDM::conds], or because a
#' user explicitly requested only one condition), then the underlying
#' [array] or `traces_dm` object is returned directly.
#'
#' For `stats_dm_list`, the returned value is a list, if `type` specifies more
#' than one condition. If the returned list would only have one entry, then
#' the underlying [data.frame] or `stats_dm` object is returned directly.
#'
#' For `traces_dm`, `unpack_obj()` returns an [array] with the traces, if
#' `unpack=TRUE`. If `unpack=FALSE`, the unmodified object is returned.
#'
#' For `stats_dm`, `unpack_obj()` returns a [data.frame] with the respective
#' statistic, if `unpack=TRUE`. If `unpack=FALSE`, the unmodified object is
#' returned.
#'
#' For `coefs_dm`, `unpack_obj()` returns a [data.frame] with the
#' parameters, if `unpack=TRUE`. If `unpack=FALSE`, the unmodified object is
#' returned.
#'
#' @examples
#' # get a pre-built model to demonstrate the function
#' my_model <- dmc_dm()
#'
#' # get some traces ...
#' some_traces <- simulate_traces(my_model, k = 2, seed = 1)
#' some_traces <- some_traces$comp
#' class(some_traces)
#' # ... unpack them to get the underlying arrays
#' class(unpack_obj(some_traces))
#'
#' # get some statistics ...
#' some_stats <- calc_stats(my_model, type = "cafs")
#' class(some_stats)
#' class(unpack_obj(some_stats))
#'
#' # get some parameters ...
#' some_coefs <- coef(get_example_fits("fits_ids_dm"))
#' class(some_coefs)
#' class(unpack_obj(some_coefs))
#'
#' @export
unpack_obj <- function(object, ...) {
  UseMethod("unpack_obj")
}


#' @rdname unpack_obj
#' @export
unpack_obj.traces_dm <- function(object, ..., unpack_elements = TRUE) {
  if (unpack_elements) {
    all_attr <- attributes(object)
    save_dims <- all_attr$dim
    attributes(object) <- NULL
    attr(object, "dim") <- save_dims
  }

  return(object)
}

#' @rdname unpack_obj
#' @export
unpack_obj.traces_dm_list <- function(
  object,
  ...,
  unpack_elements = TRUE,
  conds = NULL
) {
  # default is all conds
  if (is.null(conds)) {
    conds <- names(object)
  }
  conds <- match.arg(conds, names(object), several.ok = TRUE)

  # iterate across all
  traces <- sapply(
    conds,
    function(x) {
      unpack_obj(object[[x]], unpack_elements = unpack_elements)
    },
    simplify = FALSE,
    USE.NAMES = TRUE
  )

  if (length(conds) == 1) {
    return(traces[[1]])
  } else {
    return(traces)
  }
}


#' Unpack/Destroy Traces Objects
#'
#' @description
#'
#' `r lifecycle::badge("deprecated")`
#'
#'
#' `unpack_traces()` is deprecated. Please use the more general
#' [dRiftDM::unpack_obj()] function.
#'
#'
#' @param object an object of type `traces_dm` or `traces_dm_list` (see
#'  [dRiftDM::simulate_traces()])
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
#' `unpack_traces()` was a generic function to strip away the "unnecessary"
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


#' @rdname unpack_traces
#' @export
unpack_traces.traces_dm <- function(object, ..., unpack = TRUE) {
  lifecycle::deprecate_warn("0.2.2", "unpack_traces()", "unpack_obj()")
  unpack_obj(object, unpack_elements = unpack, ...)
}


#' @rdname unpack_traces
#' @export
unpack_traces.traces_dm_list <- function(
  object,
  ...,
  unpack = TRUE,
  conds = NULL
) {
  lifecycle::deprecate_warn("0.2.2", "unpack_traces()", "unpack_obj()")
  unpack_obj(object, unpack_elements = unpack, conds = conds, ...)
}


#' Simulate Synthetic Responses
#'
#' This function simulates data based on the provided model. To this end,
#' random samples from the predicted PDFs are drawn via approximate inverse CDF
#' sampling.
#'
#' @param object an object inheriting from [dRiftDM::drift_dm].
#' @param ... further arguments passed on to other functions, i.e.,
#' [dRiftDM::simulate_values()] and [dRiftDM::simulate_one_data_set()]. This
#' allows users to control the distribution from which original parameter
#' values are drawn (if `k` > 0) and the number of decimal places that the
#' simulated RTs should have. If users want to use a different distribution than
#' uniform for
#' [dRiftDM::simulate_values()], they must provide the additional arguments
#' (e.g., `means` and `sds`) in a format like `lower/upper`.
#'
#' @param n numeric, the number of trials per condition to draw. If a single
#' numeric, then each condition will have `n` trials. Can be a (named) numeric
#' vector with the same length as there are conditions to allow a different
#' number of trials per condition.
#' @param conds character vector, specifying the conditions to sample from.
#' Default `NULL` is equivalent to `conds(object)`.
#' @param k numeric larger than 0, indicating how many data sets shall
#' be simulated. If > 1, users must specify `lower/upper`.
#' @param lower,upper vectors or a list, specifying the simulation space for
#' each parameter of the model (see Details). Only relevant for `k > 1`
#' @param df_prms an optional data.frame providing the parameters
#' that should be used for simulating the data. `df_prms` must provide column
#' names matching with (`coef(object, select_unique = TRUE)`), plus a column
#' `ID` that will identify each simulated data set.
#' @param seed a single numeric, an optional seed for reproducible sampling
#' @param progress an integer, indicating if information about the progress
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
#' `Parameter Settings` output)
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
#' @examples
#' # Example 1 ----------------------------------------------------------------
#' # get a pre-built model for demonstration
#' a_model <- ratcliff_dm()
#'
#' # define a lower and upper simulation space
#' lower <- c(1, 0.4, 0.1)
#' upper <- c(6, 0.9, 0.5)
#'
#' # now simulate 5 data sets with each 100 trials
#' data_prms <- simulate_data(a_model,
#'   n = 100, k = 5, lower = lower,
#'   upper = upper, seed = 1, progress = 0
#' )
#' head(data_prms$synth_data)
#' head(data_prms$prms)
#'
#' # Example 2 ----------------------------------------------------------------
#' # more flexibility when defining lists for lower and upper
#' # get a pre-built model, and allow muc to vary across conditions
#' a_model <- dmc_dm(instr = "muc ~ ")
#'
#' # define a lower and upper simulation space
#' # let muc vary between 2 and 6, but in incomp conditions, let it vary
#' # between 1 and 4
#' lower <- list(
#'   default_values = c(
#'     muc = 2, b = 0.4, non_dec = 0.1,
#'     sd_non_dec = 0.01, tau = 0.02, A = 0.05,
#'     alpha = 3
#'   ),
#'   incomp = c(muc = 1)
#' )
#' upper <- list(
#'   default_values = c(
#'     muc = 6, b = 0.9, non_dec = 0.4,
#'     sd_non_dec = 0.15, tau = 0.15, A = 0.15,
#'     alpha = 7
#'   ),
#'   incomp = c(muc = 4)
#' )
#'
#' data_prms <- simulate_data(a_model,
#'   n = 100, k = 5, lower = lower,
#'   upper = upper, seed = 1, progress = 0
#' )
#' range(data_prms$prms$muc.comp)
#' range(data_prms$prms$muc.incomp)
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
simulate_data.drift_dm <- function(
  object,
  ...,
  n,
  conds = NULL,
  k = 1,
  lower = NULL,
  upper = NULL,
  df_prms = NULL,
  seed = NULL,
  progress = 1
) {
  dots <- list(...)

  # general input checks
  if (!is.null(seed)) {
    if (!is.numeric(seed) | length(seed) != 1) {
      stop("seed must be a single numeric")
    }
    withr::local_preserve_seed()
    set.seed(seed)
  }

  if (!(progress %in% c(0, 1))) {
    stop("progress must be 0 or 1")
  }

  if (!is.numeric(k) | k <= 0) {
    stop("k must be a numeric > 0")
  }

  # check what users specified ...
  case_sim <- !is.null(lower) | !is.null(upper)
  case_use <- !is.null(df_prms)

  # if only one data set is required and no lower/upper or df_prms,
  # then call simulate_one_data_set directly
  if (k == 1 & !case_sim & !case_use) {
    return(simulate_one_data_set(
      drift_dm_obj = object,
      n = n,
      conds = conds,
      round_to = dots$round_to
    ))
  }

  # otherwise conduct checks on what to do
  if (case_sim & case_use) {
    stop("Please specify only lower/upper OR df_prms, not both")
  }

  if (!case_sim & !case_use) {
    stop("Please specify lower/upper OR df_prms")
  }

  # get free_prms (needed further below for checks and actual simulation call)
  free_prms <- names(coef(object, select_unique = TRUE))

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

    sim_prms <- df_prms[c("ID", free_prms)]
  }

  # otherwise draw parameter values
  if (case_sim) {
    distr <- dots$distr
    means <- dots$means
    sds <- dots$sds

    if (!is.null(distr) && distr == "tnorm") {
      m_s <- get_parameters_smart(
        drift_dm_obj = object,
        input_a = means,
        input_b = sds,
        labels = TRUE,
        is_l_u = FALSE
      )
      means <- m_s$vec_a
      sds <- m_s$vec_b
    }

    l_u <- get_parameters_smart(
      drift_dm_obj = object,
      input_a = lower,
      input_b = upper,
      labels = TRUE
    )
    sim_prms <- simulate_values(
      lower = l_u$vec_a,
      upper = l_u$vec_b,
      k = k,
      cast_to_data_frame = TRUE,
      add_id_column = "numeric",
      distr = distr,
      means = means,
      sds = sds
    )
    sim_prms <- sim_prms[c("ID", free_prms)]
  }

  # check if prms data.frame has only numeric values for the parameters
  check_numeric <- sapply(sim_prms[names(sim_prms) != "ID"], is_numeric)
  if (!all(check_numeric)) {
    stop("Parameter values must be valid numbers")
  }

  # create a progress bar if desired
  if (progress == 1) {
    pb <- progress::progress_bar$new(
      format = "simulating [:bar] :percent; done in: :eta",
      total = nrow(sim_prms),
      clear = FALSE,
      width = 60
    )
    pb$tick(0)
  }

  # iterate through all requested data (i.e., iterate along k)
  all_sim_data <- lapply(1:nrow(sim_prms), function(one_k) {
    # set the new parameter values
    one_prm_set <- sim_prms[one_k, ]
    new_prm_values <- one_prm_set[names(one_prm_set) != "ID"]
    stopifnot(names(free_prms) == names(new_prm_values))
    coef(object, eval_model = TRUE) <- as.numeric(new_prm_values)

    # then simulate
    one_sim_dat <- simulate_one_data_set(
      drift_dm_obj = object,
      n = n,
      conds = conds,
      round_to = dots$round_to
    )
    one_sim_dat$ID <- one_prm_set$ID # use any cond to set ID
    if (progress == 1) {
      pb$tick()
    }
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
#' @param n integer, specifying the number of trials per condition. Can be a
#' single integer, or a (named) integer vector with the same length as
#' conds
#' @param conds character vector, specifying the conditions to sample from.
#' Default `NULL` is equivalent to conds(drift_dm_obj)
#' @param round_to integer, specifying the number of decimal places that the
#' simulated RTs should have. Default is `3L`.
#'
#' @returns A data.frame with the columns "RT", "<b_column>", and "Cond"; and
#' with n rows.
#'
#' @keywords internal
simulate_one_data_set <- function(
  drift_dm_obj,
  n,
  conds = NULL,
  round_to = NULL
) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }

  # get all conds an n
  model_conds <- conds(drift_dm_obj)
  if (is.null(conds)) {
    conds <- model_conds
  } else {
    conds <- match.arg(conds, model_conds, several.ok = TRUE)
  }

  if (length(n) == 1) {
    n <- rep(n, length(conds))
  }

  if (length(n) != length(conds)) {
    stop("n must have as many entries as there are requested conditions")
  }

  if (is.null(names(n))) {
    names(n) <- conds
  }

  if (!all(conds %in% names(n))) {
    stop("n doesn't specify names for each condition")
  }

  if (!is_numeric(n) || any(n <= 0)) {
    stop("n must be numeric > 0")
  }
  n <- sapply(n, as.integer)

  if (is.null(round_to)) {
    round_to <- 3L
  }

  # get the time space for draw_from_pdf
  t_max <- drift_dm_obj$prms_solve[["t_max"]]
  nt <- drift_dm_obj$prms_solve[["nt"]]
  t_vec <- seq(0, t_max, length.out = nt + 1)

  # re_evaluate if necessary
  if (is.null(drift_dm_obj$pdfs)) {
    drift_dm_obj <- re_evaluate_model(
      drift_dm_obj = drift_dm_obj,
      eval_model = TRUE
    )
  }

  # get b_coding to label the simulated data frame correctly
  b_coding <- attr(drift_dm_obj, "b_coding")

  # simulate the data across conditions
  sim_data <- lapply(conds, function(one_cond) {
    # get the n for cond
    one_n <- n[[one_cond]]

    # get pdf and n_u for cond
    pdf_u <- drift_dm_obj$pdfs[[one_cond]]$pdf_u
    pdf_l <- drift_dm_obj$pdfs[[one_cond]]$pdf_l

    p_u <- sum(pdf_u) / (sum(pdf_u) + sum(pdf_l))
    n_u <- stats::rbinom(1, one_n, p_u)

    # sample upper pdf and lower pdf
    samp_u <- draw_from_pdf(
      a_pdf = pdf_u,
      x_def = t_vec,
      k = n_u,
      method = "linear",
      round_to = round_to
    )
    samp_l <- draw_from_pdf(
      a_pdf = pdf_l,
      x_def = t_vec,
      k = one_n - n_u,
      method = "linear",
      round_to = round_to
    )

    cond_data <- data.frame(RT = c(samp_u, samp_l))
    cond_data[[b_coding$column]] <-
      rep(
        c(b_coding$u_name_value, b_coding$l_name_value),
        times = c(length(samp_u), length(samp_l))
      )
    cond_data$Cond <- one_cond
    return(cond_data)
  })

  # bind everything, check and pass back
  sim_data <- do.call(rbind, sim_data)
  sim_data <- check_reduce_raw_data(
    sim_data,
    b_coding_column = b_coding$column,
    u_value = b_coding$u_name_value,
    l_value = b_coding$l_name_value
  )
  return(sim_data)
}
