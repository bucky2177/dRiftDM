# ===== FUNCTION FOR FITTING MULTIPLE SUBJECTS

#' Fit multiple individuals and save results
#'
#' @description
#' Provides a wrapper around [dRiftDM::estimate_model] to fit multiple
#' individuals. Each individual will be stored in a folder. This folder will
#' also contain a file `drift_dm_fit_info.rds`, containing the main arguments
#' of the function call.
#'
#' @param drift_dm_obj an object inheriting from [dRiftDM::drift_dm] that will
#'  be estimated for each individual in `obs_data_ids`.
#'
#' @param obs_data_ids data.frame, must be suitable for
#' [dRiftDM::set_obs_data]. An additional column `ID` necessary, to
#' identify a single individual
#' @param lower,upper numeric vectors, providing the parameter space, see
#' [dRiftDM::estimate_model].
#' @param fit_procedure_name character, providing a name of the fitting
#'  procedure. This name will be stored in `drift_dm_fit_info.rds` to identify
#'  the fitting procedure, see also [dRiftDM::load_fits_ids].
#' @param folder_name character, a folder name for storing all the individual
#'  model fits. This variable should just state the name, and should not be
#'  a path. Per default `folder_name` is identical to `fit_procedure_name`.
#' @param seed numeric, a seed to make the fitting procedure reproducable
#'  (only relevant for differntial evolution, see [dRiftDM::estimate_model]).
#'  Default is `NULL` which means no seed.
#' @param fit_dir character, a directory where (multiple) fitting procedures
#'  can be stored. If the directory does not exist yet, it will be created
#'  via \code{base::create.dir(fit_dir, recursive = TRUE)}. Default is
#'  `"drift_dm_fits"`.
#' @param force_refit logical, if `TRUE` each individual of a fitting routine will
#'  be fitted once more. Default is `FALSE` which indicates that saved files
#' @param progress numerical, indicating if and how progress shall be depicted.
#'  If 0, no progress is shown. If 1, the currently fitted individual is printed
#'  out. If 2, a progressbar is shown. Default is 2.
#' @param start_vals optional data.frame, providing values to be set
#'  before calling [dRiftDM::estimate_model]. Can be used to control the
#'  starting values for each individual when calling Nelder-Mead. Note that this
#'  will only have an effect if DEoptim is not used (i.e., when setting
#'  `use_de_optim = FALSE`; see [dRiftDM::estimate_model]). The data.frame
#'  must provide a column `ID` whose entries match the `ID` column
#'  in `obs_data_ids`, as well as a column for each parameter of the model.
#' @param ... additional arguments passed down to [dRiftDM::estimate_model].
#'
#' @details
#' Examples and more information can be found here
#' \code{vignette("use_ddm_models", "dRiftDM")}.
#'
#' When developing the fitting routine we had three levels of files/folders
#' in mind:
#'  - In a directory/folder named `fit_dir` multiple fitting routines can be
#'  stored
#'  - Each fitting routine has its own folder with name as given by
#'   `folder_name`
#'  - Within each folder, a file called
#'  `drift_dm_fit_info.rds` contains the main information about the function
#'  call. That is, the time when last modifying/calling a fitting routine, the
#'  `lower` and `upper` parameter boundaries, the `drift_dm_object` that was
#'  fitted to each individual, the original data set `obs_data_ids`, and
#'  the identifier `fit_procedure_name`. In the same folder each individual
#'  has its own `<individual>.rds` file containing the modified `drift_dm_object`.
#'
#'
#' @export
estimate_model_ids <- function(drift_dm_obj, obs_data_ids, lower,
                                    upper, fit_procedure_name,
                                    folder_name = fit_procedure_name,
                                    seed = NULL,
                                    fit_dir = "drift_dm_fits",
                                    force_refit = FALSE,
                                    progress = 2,
                                    start_vals = NULL, ...) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }
  if (!is.null(drift_dm_obj$obs_data)) {
    warning("obs_data in drift_dm_obj will be ignored and deleted")
    drift_dm_obj$obs_data <- NULL
  }
  drift_dm_obj <- validate_drift_dm(drift_dm_obj)


  if (!is.data.frame(obs_data_ids)) {
    stop("obs_data_ids is not a data.frame")
  }
  if (!("ID" %in% colnames(obs_data_ids))) {
    stop("no ID column found in obs_data_ids")
  }

  # check if data makes sense
  b_encoding = attr(drift_dm_obj, "b_encoding")
  obs_data_ids <- check_raw_data(obs_data_ids, b_encoding_column = b_encoding$column,
                                 u_name_value = b_encoding$u_name_value,
                                 b_encoding$l_name_value)
  if (drift_dm_obj$prms_solve[["t_max"]] < max(obs_data_ids$RT)) {
    stop(
      "t_max in drift_dm_obj is smaller than maximum RT. ",
      "Please adjust before calling estimate_model_ids"
    )
  }

  # check if lower or upper are named numeric vectors and unname them
  # (after sorting of course)
  if (!is.null(names(lower)) | !is.null(names(upper))) {
    if (is.null(names(lower)) & !is.null(names(upper))) {
      stop("upper is a named numeric vector, but lower isn't")
    }
    if (!is.null(names(lower)) & is.null(names(upper))) {
      stop("lower is a named numeric vector, but upper isn't")
    }

    check_if_named_numeric_vector(
      x = lower, var_name = "lower",
      labels = drift_dm_obj$free_prms
    )
    check_if_named_numeric_vector(
      x = upper, var_name = "upper",
      labels = drift_dm_obj$free_prms
    )

    lower <- lower[drift_dm_obj$free_prms]
    lower <- unname(lower)
    upper <- upper[drift_dm_obj$free_prms]
    upper <- unname(upper)
  }


  if (!is.null(seed)) {
    if (!is.numeric(seed) | length(seed) != 1) {
      stop("seed must be a single numeric")
    }
    withr::local_preserve_seed()
    set.seed(seed)
  }

  if (!is.character(fit_procedure_name) | length(fit_procedure_name) != 1) {
    stop("fit_procedure_name must be a character vector of length 1")
  }
  if (!is.character(folder_name) | length(folder_name) != 1) {
    stop("folder_name must be a character vector of length 1")
  }
  if (nchar(folder_name) == 0) {
    stop("empty name (i.e., '') for folder_name is not allowed")
  }

  if (!is.character(fit_dir) | length(fit_dir) != 1) {
    stop("fit_dir must be a character vector of length 1")
  }
  if (!is.logical(force_refit) | length(force_refit) != 1 | is.na(force_refit)) {
    stop("force_refit must be a single logical value")
  }
  if (!is.numeric(progress) | length(progress) != 1 | !(progress %in% c(0, 1, 2))) {
    stop("progress must be numeric of either 0, 1, or 2")
  }
  if (!is.null(start_vals)) {
    if (!is.data.frame(start_vals)) {
      stop("start_vals must be a data.frame")
    }
    if (!("ID" %in% colnames(start_vals))) {
      stop("no ID column found in start_vals")
    }
    ids_data <- unique(obs_data_ids$ID)
    ids_start <- unique(start_vals$ID)
    if (!all(ids_start %in% ids_data)) {
      stop("There are individuals in start_vals that are not in obs_data_ids")
    }
    if (length(ids_start) != length(ids_data)) {
      stop("different number of individuals in start_vals and obs_data_ids")
    }
    if (length(ids_start) != nrow(start_vals)) {
      stop("individual identifiers in the column ID of start_vals not unique")
    }
    if (!all(names(start_vals)[names(start_vals) != "ID"] %in%
      drift_dm_obj$free_prms)) {
      stop(
        "columns indicating parameters in start_vals don't match",
        " free_prms of the model object"
      )
    }
    if (!all(drift_dm_obj$free_prms %in%
      names(start_vals)[names(start_vals) != "ID"])) {
      stop(
        "columns indicating parameters in start_vals don't match",
        " free_prms of the model object"
      )
    }
  }

  # 0. Step: create directory/folder for saving the fitted objects
  if (!dir.exists(fit_dir)) {
    dir.create(fit_dir, recursive = T)
  }

  folder_name <- file.path(fit_dir, folder_name)
  if (!dir.exists(folder_name)) {
    dir.create(folder_name)
  }

  # 1. dump info about call
  fit_info <- list(
    time_call = Sys.time(), lower = lower,
    upper = upper, seed = seed,
    drift_dm_obj = drift_dm_obj,
    obs_data_ids = obs_data_ids,
    fit_procedure_name = fit_procedure_name,
    start_vals = start_vals
  )
  saveRDS(
    object = fit_info,
    file = file.path(folder_name, "drift_dm_fit_info.rds")
  )


  # 2. step: Split the data and find those to fit
  list_obs_data <- split(x = obs_data_ids, f = obs_data_ids$ID)
  if (any(names(list_obs_data) == "drift_dm_fit_info")) {
    stop("drift_dm_fit_info not allowed as individual number/identifier")
  }

  if (!force_refit) {
    files_exist <- sapply(names(list_obs_data), function(x, folder_name) {
      return(file.exists(file.path(folder_name, paste0(x, ".rds"))))
    }, folder_name = folder_name)

    if (any(files_exist)) {
      message(
        "There are already files saved in ", folder_name,
        ". Skipping individuals with a corresponding identifier...",
        "\nIf you want to re-fit all ",
        "individuals, specify the argument force_refit = T!"
      )
    }
    list_obs_data <- list_obs_data[!files_exist]
  }


  # prepare progress output
  if (progress == 1) {
    start_time <- Sys.time()
  } else if (progress == 2) {
    pb <- progress::progress_bar$new(
      format = "estimating [:bar] :percent; done in: :eta",
      total = length(list_obs_data), clear = FALSE, width = 60
    )
    pb$tick(0)
  }

  # 3. run a loop across the list
  for (name_one_ids in names(list_obs_data)) {
    # set the data to the model
    one_obs_data <- list_obs_data[[name_one_ids]]
    drift_dm_obj_id <- set_obs_data(
      drift_dm_obj = drift_dm_obj,
      obs_data = one_obs_data, eval_model = F
    )

    # set parameter values that might be used if DE is not run
    if (!is.null(start_vals)) {
      set_vals <- start_vals[start_vals$ID == name_one_ids, ]
      set_vals <- set_vals[names(set_vals) != "ID"]
      drift_dm_obj_id <- set_model_prms(
        drift_dm_obj = drift_dm_obj_id,
        new_prm_vals = unlist(set_vals)
      )
    }

    # estimate the model
    if (progress == 1) {
      num <- which(name_one_ids == names(list_obs_data))
      cat(
        "Estimating model for individual:", name_one_ids,
        sprintf("(%s/%s)", num, length(list_obs_data)), "\n"
      )
    }
    result <-
      tryCatch(expr = {
        # estimate the model
        estimate_model(
          drift_dm_obj = drift_dm_obj_id, lower = lower, upper = upper, ...
        )
      }, error = function(e) {
        warning(
          "\nan error occured in estimate_model:\n\n", e, "\n",
          "Happened when fitting individual ", name_one_ids, ".",
          " Skipping this individual."
        )
        return(NULL)
      })

    # dump file
    if (!is.null(result)) {
      saveRDS(
        object = result,
        file = file.path(folder_name, paste0(name_one_ids, ".rds"))
      )
    }

    # update progress
    if (progress == 1) {
      time_elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "min"))
      time_elapsed <- round(time_elapsed, 2)
      if (time_elapsed > 10) time_elapsed <- round(time_elapsed)
      cat("-> done. Time elapsed:", time_elapsed, "minutes", "\n")
    } else if (progress == 2) {
      pb$tick()
    }
  }
}


# ===== FUNCTION FOR LOADING MULTIPLE FITS


#' Load estimates of a fit procedure
#'
#' This function loads the results of a fit procedure where a model was fitted
#' to multiple individuals
#'
#'
#' @param path character, a path pointing to a folder or directory
#'  containing the individual model fits.
#'
#' @param fit_procedure_name character, an optional name that identifies the
#'  fit procedure that should be loaded
#' @param detailed_info logical, controls the amount of information displayed
#'  in case multiple fit procedures were found and the user is prompted to
#'  explicitly choose one
#' @param check_data logical, should the data be checked before passing them
#'  back? This checks the observed data and the properties of the model. This
#'  can take some time though, depending on the number of individuals and the
#'  size of the observed data. Default is `TRUE`
#' @param progress numerical, indicating if and how progress shall be depicted.
#'  If 0, no progress is shown. If 1, basic infos about the checking progress
#'  is shown. If 2, multiple progressbars are shown. Default is 2.
#'
#' @details
#' with respect to the logic outlined in the details of
#' [dRiftDM::estimate_model_ids] on the organization of fit procedures,
#' `path` could either point to a directory with (potentially) multiple fit
#' routines or to a specific folder with the invidiual fits. In either case
#' the intended location is recursively searched for files named
#' `drift_dm_fit_info.rds`.
#'
#' If the fit procedure was uniquely located, either because only one fit
#' routine was found in the intended location or because only one
#' `drift_dm_fit_info.rds` contains the optional identifier specified in
#' `fit_procedure_name`, then all individual model fits including the
#' information `fit_procedure_name` are loaded and returned
#'
#' In case multiple fit procedures are identified, the user is
#' prompted with a [utils::menu], listing information about the possible
#' candidates. The intended fit procedure can then interactively be chosen
#' by the user. The amount of displayed information is controlled via
#' `detailed_info`.
#'
#' @returns
#' An object of type `dm_fits_ids` which essentially is a list with two
#' entries:
#' - `drift_dm_fit_info`, containing a list of the main arguments when
#'  [dRiftDM::estimate_model_ids] was originally called, including
#'  a time-stamp
#' - `all_fits`, containing a list of all the modified/fitted `drift_dm`
#'  objects. The list's entry are named according to the individuals' identifier
#'
#'
#' @export
load_fits_ids <- function(path = "drift_dm_fits",
                          fit_procedure_name = "",
                          detailed_info = F,
                          check_data = T,
                          progress = 2) {


  if (!is.character(path) | length(path) != 1) {
    stop("path is not character vector of length 1")
  }

  if (!is.character(fit_procedure_name) | length(fit_procedure_name) != 1) {
    stop("fit_procedure_name is not character vector of length 1")
  }

  if (!is.logical(detailed_info) | length(detailed_info) != 1) {
    stop("fit_procedure_name is not character vector of length 1")
  }

  if (!is.logical(check_data) | length(check_data) != 1) {
    stop("fit_procedure_name is not character vector of length 1")
  }
  if (!(progress %in% c(0,1,2))) {
    stop("progress must be 0, 1, or 2")
  }


  if (!dir.exists(path)) {
    stop("no directory ", path, " found.")
  }

  # find folders with fit_info files
  search_files <- list.files(path, full.names = T, recursive = T)
  search_files <- search_files[grepl("[/\\]drift_dm_fit_info.rds$", search_files)]
  # ensure that there are no two files in one folder
  test <- dirname(search_files)
  stopifnot(length(unique(test)) == length(test))
  # ... find those with a matching fit_procedure_name
  if (fit_procedure_name != "") {
    info_proc_names <- sapply(search_files, function(x) {
      return(readRDS(file = x)$fit_procedure_name)
    })
    search_files <- search_files[which(info_proc_names == fit_procedure_name)]
  }

  # if no fit_info files were found
  if (length(search_files) == 0) {
    stop("no folder with a (suitable) file drift_dm_fit_info.rds found",
         " in path ", path, " with procedure name ", fit_procedure_name)
  }

  option <- 1
  # if multiple files were found, make the user select
  if (length(search_files) > 1) {
    strings_of_info <- sapply(search_files, function(x, detailed_info) {
      summarize_drift_dm_info(
        full_name_to_file = x,
        detailed_info = detailed_info
      )
    }, detailed_info = detailed_info)
    strings_of_info <- unname(strings_of_info)
    option <- utils::menu(
      strings_of_info,
      title = "multiple fit procedures found. Please select one"
    )
  }

  # if the selection was exited
  if (option == 0) {
    message("exiting selection of fit procedures. Returning NULL")
    return(NULL)
  }

  # take the selected file/folder or the first
  selected_file <- search_files[option]
  folder <- dirname(selected_file)
  files <- list.files(folder, pattern = ".rds$", full.names = T)


  # progress info ...
  if (progress == 1) {
    cat("Loading all individual fits\n")
  } else if (progress == 2) {
    pb <- progress::progress_bar$new(
      format = "loading [:bar] :percent; done in: :eta",
      total = length(files), clear = FALSE, width = 60
    )
    pb$tick(0)
  }

  # and then load the files
  list_of_all_fits <- lapply(files, function(x) {
    if (progress == 2) pb$tick()
    return(readRDS(x))
  })
  list_of_all_fits <- stats::setNames(list_of_all_fits,
                                      gsub(".rds$", "", basename(files)))
  fits_ids <- list(
    drift_dm_fit_info = list_of_all_fits$drift_dm_fit_info,
    all_fits = list_of_all_fits[names(list_of_all_fits) != "drift_dm_fit_info"]
  )

  class(fits_ids) <- "dm_fits_ids"
  if (check_data) {
    fits_ids <- validate_fits_ids(fits_ids = fits_ids, progress = progress)
  }
  return(fits_ids)
}


# ===== FUNCTION FOR HANDLING INFORMATION AND CHECKING FITS


# checks if all the information are in the fits_ids
# object and ensures that nothing obviously fishy is going on
# in the individual model fits, by validating each model, and checking
# if the data in info_file matches the data of each individual fit object
validate_fits_ids <- function(fits_ids, progress) {
  if (!inherits(fits_ids, "dm_fits_ids")) {
    stop("fits_ids is not of type dm_fits_ids")
  }

  # ensure that everything is there in the info file
  drift_dm_fit_info <- fits_ids$drift_dm_fit_info
  names_to_check <- c(
    "time_call", "lower", "upper", "drift_dm_obj",
    "obs_data_ids", "fit_procedure_name"
  )

  if (!all(names(drift_dm_fit_info)[!(names(drift_dm_fit_info) %in%
    c("seed", "start_vals"))]
  %in% names_to_check)) {
    stop("drift_dm_fit_info contains unexpected info entries")
  }
  if (!all(names_to_check %in% names(drift_dm_fit_info))) {
    stop("drift_dm_fit_info contains not all expected info entries")
  }

  if (!inherits(drift_dm_fit_info$time_call, "POSIXct")) {
    stop("time_call is not of type POSIXct")
  }

  if (!is.numeric(drift_dm_fit_info$lower)) {
    stop("lower is not of type numeric")
  }
  if (!is.numeric(drift_dm_fit_info$upper)) {
    stop("upper is not of type numeric")
  }
  if (length(drift_dm_fit_info$lower) != length(drift_dm_fit_info$upper)) {
    stop("length of upper and lower don't match")
  }
  if (length(drift_dm_fit_info$drift_dm_obj$free_prms) !=
    length(drift_dm_fit_info$upper)) {
    stop("length of upper/lower don't match the number of free parameters")
  }
  seed <- drift_dm_fit_info$seed
  if (!is.null(seed) & (!is.numeric(seed) | length(seed) != 1)) {
    stop("seed is not a single number or NULL")
  }
  validate_drift_dm(drift_dm_fit_info$drift_dm_obj)
  if (!is.data.frame(drift_dm_fit_info$obs_data_ids)) {
    stop("obs_data_ids is not a data.frame")
  }
  if (!("ID" %in% names(drift_dm_fit_info$obs_data_ids))) {
    stop("obs_data_ids has no column ID")
  }
  if (!is.character(drift_dm_fit_info$fit_procedure_name) |
    length(drift_dm_fit_info$fit_procedure_name) != 1) {
    stop("fit_procedure_name is not of type character with length 1")
  }

  # ensure that the fits are valid drift_dm objects....
  # first progress info
  if (progress == 1) {
    cat("Checking model of each fit\n")
  } else if (progress == 2) {
    pb <- progress::progress_bar$new(
      format = "checking models [:bar] :percent; done in: :eta",
      total = length(fits_ids$all_fits), clear = FALSE, width = 60
    )
    pb$tick(0)
  }

  # then check each model
  exp_model <- fits_ids$drift_dm_fit_info$drift_dm_obj
  exp_upper <- fits_ids$drift_dm_fit_info$upper
  exp_lower <- fits_ids$drift_dm_fit_info$lower
  for (one_vp in names(fits_ids$all_fits)) {
    one_fit <- fits_ids$all_fits[[one_vp]]
    if (any(names(exp_model$prms_model) != names(one_fit$prms_model))) {
      stop(
        "parameters in the model of the fit procedure info don't match",
        "the parameters of individual ", one_vp, "'s model "
      )
    }
    if (any(exp_model$conds != one_fit$conds)) {
      stop(
        "conditions in the model of the fit procedure info don't match",
        "the conditions of individual ", one_vp, "'s model "
      )
    }
    if (any(class(exp_model) != class(one_fit))) {
      stop(
        "class of the model of the fit procedure info doesn't match",
        "the class of individual ", one_vp, "'s model "
      )
    }
    if (any(exp_model$prms_solve != one_fit$prms_solve)) {
      stop(
        "prms_solve in the model of the fit procedure info doesn't match",
        "the prms_solve of individual ", one_vp, "'s model "
      )
    }
    if (any(exp_model$free_prms != one_fit$free_prms)) {
      stop(
        "free_prms in the model of the fit procedure info doesn't match",
        "the free_prms of individual ", one_vp, "'s model "
      )
    }
    # check if parameters are in the boundary range
    fitted_model_prms <- one_fit$prms_model[names(one_fit$prms_model) %in%
      one_fit$free_prms]
    if (any(fitted_model_prms < exp_lower)) {
      stop(
        "individual ", one_vp, " provided model parameters that are smaller ",
        "than the lower parameter boundaries found in the procedure info"
      )
    }

    if (any(fitted_model_prms > exp_upper)) {
      stop(
        "individual ", one_vp, " provided model parameters that are larger ",
        "than the upper parameter boundaries found in the procedure info"
      )
    }

    # just a warning for functions because comparing functions is tricky...
    if (!isTRUE(all.equal(exp_model$comp_funs, one_fit$comp_funs))) {
      warning(
        "comp_funs in the model of the fit procedure info doesn't match",
        "the comp_funs of individual ", one_vp, "'s model "
      )
    }

    # b_encoding
    b_encoding <- attr(one_fit, "b_encoding")

    if (!isTRUE(all.equal(attr(exp_model, "b_encoding"), b_encoding))) {
      stop(
        "individual ", one_vp, " provided model parameters that are larger ",
        "than the upper parameter boundaries found in the procedure info"
      )
    }
    validate_drift_dm(one_fit)
    if (progress == 2) pb$tick()
  }

  # the same or different number of individuals?
  exp_obs_data <- drift_dm_fit_info$obs_data_ids
  info_ids <- as.character(unique(exp_obs_data$ID))
  n_exp <- length(info_ids)
  n_real <- length(fits_ids$all_fits)

  if (n_exp != n_real) {
    warning(paste(
      "Based on the provided data in the info file,", n_exp,
      "individuals are expected but", n_real, "were found"
    ))
  }



  # from those participants that are found... Does the data match?
  conds <- unique(exp_obs_data$Cond)
  ids_fitted <- names(fits_ids$all_fits)
  ids_found <- ids_fitted[ids_fitted %in% info_ids]
  list_exp_obs_data <- split(x = exp_obs_data, f = exp_obs_data$ID)
  b_encoding <- attr(fits_ids$drift_dm_fit_info$drift_dm_obj, "b_encoding")


  # progress bar
  if (progress == 1) {
    cat("Checking data of each fit\n")
  } else if (progress == 2) {
    pb <- progress::progress_bar$new(
      format = "checking data [:bar] :percent; done in: :eta",
      total = length(ids_found), clear = FALSE, width = 60
    )
    pb$tick(0)
  }
  # loop
  for (one_vp in ids_found) {
    model_data <- fits_ids$all_fits[[one_vp]]$obs_data

    vp_data <- list_exp_obs_data[[one_vp]]
    for (one_cond in conds) {
      check_u = model_data$rts_u[[one_cond]] ==
        vp_data$RT[vp_data$Cond == one_cond & vp_data[[b_encoding$column]] == b_encoding$u_name_value]
      check_u = all(check_u)
      check_l = model_data$rts_l[[one_cond]] ==
        vp_data$RT[vp_data$Cond == one_cond & vp_data[[b_encoding$column]] == b_encoding$l_name_value]
      check_l = all(check_l)

      if (!check_u | !check_l) {
        warning(
          "data of individual ", one_vp, " in the fitted model ",
          "doesn't match with the expected data based on the info file. ",
          "Cond: ", one_cond
        )
      }
    }
    if (progress == 2) pb$tick()
  }
  return(fits_ids)
}

# reads the info file and turns its main information into a string
summarize_drift_dm_info <- function(full_name_to_file, detailed_info) {
  fit_info <- readRDS(file = full_name_to_file)
  string <- "\n"
  string <- paste0(string, "Fit Name: ", fit_info$fit_procedure_name, "\n")
  string <- paste0(
    string, "Last call: ",
    format(fit_info$time_call, "%Y-%m-%d %H:%M:%S"),
    "\n"
  )
  if (detailed_info) {
    string <- paste0(
      string, "Model: ",
      paste(class(fit_info$drift_dm_obj), collapse = ", "),
      "\n"
    )
    string <- paste0(
      string, "N Individuals: ",
      length(unique(fit_info$obs_data_ids$ID)),
      "\n"
    )
    string <- paste0(
      string, "Lower: ",
      paste(fit_info$lower, collapse = ", "),
      "\n"
    )
    string <- paste0(
      string, "Upper: ",
      paste(fit_info$upper, collapse = ", "),
      "\n"
    )
    string <- paste0(string, "Seed: ", fit_info$seed, "\n")
  }
  return(string)
}
