# ===== THE USER FUNCTION FOR CREATING A BASIC MODEL
#' @export
drift_dm <- function(prms_model, conds, free_prms = NULL, obs_data = NULL,
                     sigma = 1, t_max = 3, dt = .005, dx = .05) {
  # conduct input checks and set maybe defaults

  if (length(prms_model) == 0) {
    stop("prms_model has length 0")
  }
  check_if_named_numeric_vector(x = prms_model, var_name = "prms_model")
  if (!is.character(conds)) {
    stop("conds is not a character vector")
  }
  if (!is.numeric(sigma)) {
    stop("sigma is not numeric")
  }
  if (!is.numeric(t_max)) {
    stop("T is not numeric")
  }
  if (!is.numeric(dt)) {
    stop("dt is not numeric")
  }
  if (!is.numeric(dx)) {
    stop("dx is not numeric")
  }

  prms_solve <- c("sigma" = sigma, "t_max" = t_max, "dt" = dt, "dx" = dx)

  if (is.null(free_prms)) {
    free_prms <- names(prms_model)
  } else {
    if (!all(free_prms %in% names(prms_model))) {
      stop("free_prms do not match prms_model")
    }
    free_prms <- names(prms_model)[names(prms_model) %in% free_prms]
  }
  if (!is.character(free_prms)) {
    stop("free_prms is not a character vector")
  }


  # pass the arguments further down
  drift_dm_obj <- new_drift_dm(
    prms_model = prms_model, conds = conds,
    free_prms = free_prms, obs_data = obs_data,
    prms_solve = prms_solve
  )

  drift_dm_obj <- validate_drift_dm(drift_dm_obj)
  return(drift_dm_obj)
}


# ====== BACKEND FUNCTION FOR CREATING A DRIFT_DM OBJECT
new_drift_dm <- function(prms_model, conds, free_prms, obs_data = NULL,
                         prms_solve) {
  # calculate the number of discretization steps
  prms_solve["nT"] <- as.integer(
    prms_solve[["t_max"]] / prms_solve[["dt"]] + 1.e-8
  )
  prms_solve["nX"] <- as.integer(2 / prms_solve["dx"] + 1.e-8)


  # add everything
  drift_dm_obj <- list(
    prms_model = prms_model, conds = conds,
    prms_solve = prms_solve, free_prms = free_prms,
    solver = "kfe"
  )
  class(drift_dm_obj) <- "drift_dm"

  # convert and add data if necessary
  if (!is.null(obs_data)) {
    drift_dm_obj <- set_data(drift_dm_obj, obs_data, eval_model = T)
  }

  # return
  return(drift_dm_obj)
}

# ======== CHECKS ON EACH DRIFT_DM OBJECT
validate_drift_dm <- function(drift_dm_obj) {
  if (length(drift_dm_obj$prms_model) == 0) {
    stop("prms_model has length 0")
  }

  # conds is a character vector?
  if (!is.character(drift_dm_obj$conds)) {
    stop("conds in drift_dm_obj is not a character vector")
  }

  # ensure everything is fine with the model parameters and the parameters
  # for solving the model
  check_if_named_numeric_vector(
    x = drift_dm_obj$prms_model,
    var_name = "drift_dm_obj$prms_model"
  )

  check_if_named_numeric_vector(
    x = drift_dm_obj$prms_solve,
    var_name = "drift_dm_obj$prms_solve",
    labels = c(
      "sigma", "t_max", "dt", "dx", "nX",
      "nT"
    ),
    length = 6
  )
  if (!is.character(drift_dm_obj$free_prms)) {
    stop("free_prms in drift_dm_obj is not a character vector")
  }

  if (!all(drift_dm_obj$free_prms %in% names(drift_dm_obj$prms_model))) {
    stop("free_prms do not match prms_model in drift_dm_obj")
  }

  subs <- names(drift_dm_obj$prms_model)[names(drift_dm_obj$prms_model)
  %in% drift_dm_obj$free_prms]
  if (any(subs != drift_dm_obj$free_prms)) {
    stop("free_prms are not ordered according to prms_model")
  }

  prms_solve <- drift_dm_obj$prms_solve # for less intricate code
  if (prms_solve[["sigma"]] <= 0) stop("sigma in prms_model must be positive")
  if (prms_solve[["t_max"]] <= 0) stop("T in prms_model must be positive")
  if (prms_solve[["dt"]] <= 0) stop("dt in prms_model must be positive")
  if (prms_solve[["dx"]] <= 0) stop("dx in prms_model must be positive")
  if (prms_solve[["nT"]] <= 0) stop("nT in prms_model must be positive")
  if (prms_solve[["nX"]] <= 0) stop("nT in prms_model must be positive")
  if (abs(prms_solve[["nX"]] - as.integer(prms_solve[["nX"]])) != 0) {
    stop("nX must not have decimal places")
  }
  if (abs(prms_solve[["nT"]] - as.integer(prms_solve[["nT"]])) != 0) {
    stop("nT must not have decimal places")
  }
  if (abs(prms_solve[["dt"]] * prms_solve[["nT"]] - prms_solve[["t_max"]])
  >= drift_dm_approx_error()) {
    stop("Final timeline not nT times dt. Check the dt and T values!")
  }
  if (abs(prms_solve[["dx"]] * prms_solve[["nX"]] - 2) >=
    drift_dm_approx_error()) {
    stop("dX times nX is not 2. Check if 2 / dX provides an integer!")
  }

  if (prms_solve[["nX"]] <= 10) {
    warning("nX seems very small. Double check your model")
  }
  if (prms_solve[["nT"]] <= 10) {
    warning("nT seems very small. Double check your model")
  }

  # in case the max RT is larger than expected, adjust the prms_solve
  if (!is.null(drift_dm_obj$obs_data)) {
    max_rt <- max(unlist(drift_dm_obj$obs_data))
    if (max_rt > drift_dm_obj$prms_solve[["t_max"]]) {
      warning(
        "RTs in obs_data are larger than the maximum time in prms_solve ",
        "Trying to fix this by adjusting t_max and nT. Please double-check ",
        "your data and your model!"
      )

      prms_solve <- drift_dm_obj$prms_solve
      prms_solve[["nT"]] <- ceiling(max_rt / prms_solve[["dt"]])
      prms_solve[["t_max"]] <- prms_solve[["nT"]] * prms_solve[["dt"]]
      drift_dm_obj$prms_solve <- prms_solve
    }
  }

  invisible(drift_dm_obj)
}



# =======GENERIC FUNCTIONS FOR THE DIFFERENT COMPONENTS OF A DDM

mu <- function(drift_dm_obj, t_vec, one_cond) {
  UseMethod("mu")
}

mu_int <- function(drift_dm_obj, t_vec, one_cond) {
  UseMethod("mu_int")
}

x <- function(drift_dm_obj, one_cond) {
  UseMethod("x")
}

b <- function(drift_dm_obj, t_vec, one_cond) {
  UseMethod("b")
}

dt_b <- function(drift_dm_obj, t_vec, one_cond) {
  UseMethod("dt_b")
}

nt <- function(drift_dm_obj, t_vec, one_cond) {
  UseMethod("nt")
}

standard_drift <- function() {
  return(3)
}
standard_boundary <- function() {
  return(0.6)
}
standard_nt <- function() {
  return(0.3)
}


# ======= FALLBACK FUNCTIONS FOR THE DIFFERENT COMPONENTS OF A DDM

mu.drift_dm <- function(drift_dm_obj, t_vec, one_cond) {
  mu <- standard_drift()
  if (!is.numeric(mu) | length(mu) != 1) {
    stop("value for mu is not a single number")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }
  mu <- rep(mu, length(t_vec))
  return(mu)
}

mu_int.drift_dm <- function(drift_dm_obj, t_vec, one_cond) {
  mu <- standard_drift()
  if (!is.numeric(mu) | length(mu) != 1) {
    stop("value for mu is not a single number")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }
  return(mu * t_vec)
}

# @THOMAS: MUSS SICH DAS ZU 1 INTEGRIEREN?
x.drift_dm <- function(drift_dm_obj, one_cond) {
  nX <- drift_dm_obj$prms_solve[["nX"]]
  dx <- drift_dm_obj$prms_solve[["dx"]]
  x <- numeric(length = nX + 1)
  x[(length(x) + 1) %/% 2] <- 1 / dx
  return(x)
}

b.drift_dm <- function(drift_dm_obj, t_vec, one_cond) {
  b <- standard_boundary()
  if (!is.numeric(b) | length(b) != 1) {
    stop("b is not a single number")
  }
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }
  b <- rep(b, length(t_vec))
  return(b)
}

dt_b.drift_dm <- function(drift_dm_obj, t_vec, one_cond) {
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }
  dt_b <- rep(0, length(t_vec))

  return(dt_b)
}

nt.drift_dm <- function(drift_dm_obj, t_vec, one_cond) {
  non_dec_time <- standard_nt()
  if (!is.numeric(non_dec_time) | length(non_dec_time) != 1) {
    stop("non_dec_time is not a single number")
  }

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


# ===== FUNCTIONS FOR GETTING INFORMATION CRITERIA

get_ic <- function(drift_dm_obj) {
  if (is.null(drift_dm_obj$log_like_val)) {
    drift_dm_obj$log_like_val <- log_like(drift_dm_obj)
  }

  ll <- drift_dm_obj$log_like_val
  k <- length(drift_dm_obj$free_prms)
  n <- length(unlist(drift_dm_obj$obs_data))

  AIC <- 2 * k - 2 * ll
  BIC <- k * log(n) - 2 * ll

  return(c(AIC = AIC, BIC = BIC))
}

# ===== FUNCITONS FOR SETTING THINGS

#' @export
set_model_prms <- function(drift_dm_obj, new_model_prms, eval_model = T) {
  if (length(new_model_prms) != length(drift_dm_obj$free_prms)) {
    stop("new_prms don't match the number of freeprms")
  }
  if (!is.numeric(new_model_prms)) {
    stop("new_model_prms are not of type numeric")
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


#' @export
set_free_prms <- function(drift_dm_obj, new_free_prms) {
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

  if (length(matched_free_prms) > length(drift_dm_obj$prms_model)) {
    stop("more parameters listed in new_free_prms than listed in prms_model")
  }

  if (length(matched_free_prms) == 0) {
    stop("it is not allowed to have no free parameter")
  }

  # ensure ordering when setting the free parameters
  matched_free_prms <- name_prms_model[name_prms_model %in% matched_free_prms]
  drift_dm_obj$free_prms <- matched_free_prms

  # check model and return
  drift_dm_obj <- validate_drift_dm(drift_dm_obj)
  return(drift_dm_obj)
}


#' @export
set_solver_setting <- function(drift_dm_obj, name_prm_solve, value_prm_solve,
                               eval_model = T) {
  name_prm_solve <- match.arg(
    name_prm_solve,
    c("solver", "sigma", "t_max", "dt", "dx")
  )


  # if desired, set solver
  if (name_prm_solve == "solver") {
    if (!is.character(value_prm_solve) | length(value_prm_solve) != 1) {
      stop("solver argument must be a single character")
    }
    drift_dm_obj$solver <- value_prm_solve
  }

  # if desired, set sigma
  if (name_prm_solve == "sigma") {
    if (!is.numeric(value_prm_solve) | length(value_prm_solve) != 1) {
      stop("sigma argument must be a single numeric")
    }
    drift_dm_obj$prms_solve[["sigma"]] <- value_prm_solve
  }


  # if desired, set t_max or dt
  if (name_prm_solve == "t_max" | name_prm_solve == "dt") {
    if (!is.numeric(value_prm_solve) | length(value_prm_solve) != 1) {
      stop(name_prm_solve, "argument must be a single numeric")
    }

    prms_solve <- drift_dm_obj$prms_solve
    prms_solve[[name_prm_solve]] <- value_prm_solve
    prms_solve["nT"] <- as.integer(
      prms_solve[["t_max"]] / prms_solve[["dt"]] + 1.e-8
    )
    drift_dm_obj$prms_solve <- prms_solve
  }

  # if desired, set dx
  if (name_prm_solve == "dx") {
    if (!is.numeric(value_prm_solve) | length(value_prm_solve) != 1) {
      stop("dx argument must be a single numeric")
    }

    prms_solve <- drift_dm_obj$prms_solve
    prms_solve[["dx"]] <- value_prm_solve
    prms_solve["nX"] <- as.integer(2 / prms_solve["dx"] + 1.e-8)
    drift_dm_obj$prms_solve <- prms_solve
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


#' @export
set_data <- function(drift_dm_obj, obs_data, eval_model = T) {
  # input check
  check_raw_data(obs_data)

  # add rts to the model
  rts_corr <- list()
  rts_err <- list()
  for (one_cond in drift_dm_obj$conds) {
    subDat <- obs_data[obs_data$Cond == one_cond, ]
    rts_corr[[one_cond]] <- subDat$RT[subDat$Error == 0]
    rts_err[[one_cond]] <- subDat$RT[subDat$Error == 1]

    if (length(rts_corr[[one_cond]]) == 0 & length(rts_err[[one_cond]]) == 0) {
      stop(
        "Condition ", one_cond, " in the provided dataset did not provide ",
        "any RTs."
      )
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
  if (min(obs_data$RT) < 0) warning("RTs are not >= 0")
  if (!all(unique(obs_data$Error) %in% c(0, 1))) {
    stop("Error column should only contain 0s and 1s")
  }
}


# ===== FUNCTION FOR ENSURING EVERYTHING IS UP-TO-DATE

#' @export
re_evaluate_model <- function(drift_dm_obj) {
  drift_dm_obj$log_like_val <- log_like(drift_dm_obj)
  drift_dm_obj$ic_vals <- get_ic(drift_dm_obj)
  return(drift_dm_obj)
}



# ===== FUNCTIONS FOR SIMULATING DATA/TRIALS
#' @export
simulate_trace <- function(drift_dm_obj, k, one_cond, add_x = FALSE, seed = NULL) {
  if (!is.numeric(k) || k <= 0) {
    stop("k must be a numeric > 0")
  }
  if (!is.character(one_cond)) {
    stop("one_cond must be a character")
  }
  if (!(one_cond %in% drift_dm_obj$conds)) {
    stop("one_cond not in the models conds")
  }
  if (!is.logical(add_x)) {
    stop("add_x must be of type logical")
  }

  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1) {
      stop("seed must be a single numeric")
    }
    withr::local_seed(seed)
  }

  # unpack arguments for easier usage
  t_max <- drift_dm_obj$prms_solve[["t_max"]]
  dt <- drift_dm_obj$prms_solve[["dt"]]
  nT <- drift_dm_obj$prms_solve[["nT"]]
  nX <- drift_dm_obj$prms_solve[["nX"]]
  sigma <- drift_dm_obj$prms_solve[["sigma"]]


  e_samples <- matrix(0, nrow = k, ncol = nT + 1) # create matrix for storage
  t_vec <- seq(0, t_max, length.out = nT + 1) # all time steps
  mu_vec <- mu(drift_dm_obj = drift_dm_obj, t_vec = t_vec, one_cond = one_cond)
  b_vec <- b(drift_dm_obj = drift_dm_obj, t_vec = t_vec, one_cond = one_cond)
  samp_x <- numeric(k) # storage for starting values

  if (add_x) {
    pdf_x <- x(drift_dm_obj = drift_dm_obj, one_cond = one_cond)
    xx <- seq(-b_vec[1], b_vec[1], length.out = nX + 1)
    samp_x <- draw_from_pdf(pdf_x, xx, k)
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

  return(t(e_samples))
}

#' @export
simulate_data <- function(drift_dm_obj, n, seed = NULL) {
  if (!is.numeric(n) || n <= 0) {
    stop("n must be a numeric > 0")
  }
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1) {
      stop("seed must be a single numeric")
    }
    withr::local_seed(seed)
  }

  t_max <- drift_dm_obj$prms_solve[["t_max"]]
  nT <- drift_dm_obj$prms_solve[["nT"]]
  t_vec <- seq(0, t_max, length.out = nT + 1)

  sim_data <- data.frame(RT = numeric(), Error = numeric(), Cond = character())
  for (one_cond in drift_dm_obj$conds) {
    # get pdf and n_u for cond
    pdfs <- get_pdfs(
      drift_dm_obj = drift_dm_obj, one_cond = one_cond,
      solver = drift_dm_obj$solver
    )
    pdf_u <- pdfs[[1]]
    pdf_l <- pdfs[[2]]
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
        Error = rep(c(0, 1), times = c(n_u, n - n_u)),
        Cond = one_cond
      )
    )
  }
  check_raw_data(sim_data) # ensure we play by our own rules :)
  return(sim_data)
}
