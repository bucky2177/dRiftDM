# ===== FUNCTION FOR FITTING MULTIPLE SUBJECTS

#' Fit multiple subjects and save results
#'
#' @description
#' Provides a wrapper around [dRiftDM::estimate_model] to fit multiple
#' subjects. Each subject will be stored in a folder. This folder will
#' also contain a file `drift_dm_fit_info.rds`, containing the main arguments
#' of the function call.
#'
#' @param drift_dm_obj an object inheriting from [dRiftDM::drift_dm] that will
#'  be estimated for each subject in `obs_data_subject`.
#'
#' @param obs_data_subject data.frame, providing an `RT`, `Error`, `Cond`
#' and `Subject` column. The first three columns must be suitable for
#' [dRiftDM::set_obs_data]. The column `Subject` identifies a single individual
#' @param lower,upper numeric vectors, providing the parameter space, see
#' [dRiftDM::estimate_model].
#' @param fit_procedure_name character, providing a name of the fitting
#'  procedure. This name will be stored in `drift_dm_fit_info.rds` to identify
#'  the fitting procedure, see also [dRiftDM::load_fits_subjects].
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
#' @param force_refit logical, if `TRUE` each subject of a fitting routine will
#'  be fitted once more. Default is `FALSE` which indicates that saved files
#' @param progress numerical, indicating if and how progress shall be depicted.
#'  If 0, no progress is shown. If 1, the currently fitted subject is printed
#'  out. If 2, a progressbar is shown.
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
#'  fitted to each individual, the original data set `obs_data_subject`, and
#'  the identifier `fit_procedure_name`. In the same folder each individual
#'  has its own `<subject>.rds` file containing the modified `drift_dm_object`.
#'
#'
#' @export
estimate_model_subjects <- function(drift_dm_obj, obs_data_subject, lower,
                                    upper, fit_procedure_name,
                                    folder_name = fit_procedure_name,
                                    seed = NULL,
                                    fit_dir = "drift_dm_fits",
                                    force_refit = FALSE,
                                    progress = 1, ...) {
  if (!inherits(drift_dm_obj, "drift_dm")) {
    stop("drift_dm_obj is not of type drift_dm")
  }
  if (!is.null(drift_dm_obj$obs_data)) {
    warning("obs_data in drift_dm_obj will be ignored and deleted")
    drift_dm_obj$obs_data <- NULL
  }
  if (!is.data.frame(obs_data_subject)) {
    stop("obs_data_subject is not a data.frame")
  }
  if (!("Subject" %in% colnames(obs_data_subject))) {
    stop("no Subject column found in obs_data_subject")
  }
  if (!is.null(seed)) {
    if (!is.numeric(seed) | length(seed) != 1) {
      stop("seed must be a single numeric")
    }
    withr::local_seed(seed)
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
    stop("verbose must be numeric of either 0, 1, or 2")
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
    obs_data_subject = obs_data_subject,
    fit_procedure_name = fit_procedure_name
  )
  saveRDS(
    object = fit_info,
    file = file.path(folder_name, "drift_dm_fit_info.rds")
  )


  # 2. step: Split the data
  list_obs_data <- split(x = obs_data_subject, f = obs_data_subject$Subject)

  # 3. run a loop across the list
  if (progress == 2) {
    pb <- utils::txtProgressBar(
      min = 0,
      max = length(list_obs_data),
      initial = 0
    )
  }

  for (name_one_subject in names(list_obs_data)) {
    if (name_one_subject == "drift_dm_fit_info") {
      stop("drift_dm_fit_info not allowed as subject number/identifier")
    }

    if (progress == 2) {
      utils::setTxtProgressBar(
        pb,
        which(name_one_subject == names(list_obs_data))
      )
    }

    one_obs_data <- list_obs_data[[name_one_subject]]
    drift_dm_obj_subj <- set_obs_data(
      drift_dm_obj = drift_dm_obj,
      obs_data = one_obs_data
    )

    file_name <- paste0(name_one_subject, ".rds")

    if (!force_refit & file.exists(file.path(folder_name, file_name))) {
      next
    }

    result <-
      tryCatch(expr = {
        if (progress == 1) {
          cat("Estimating model for subject:", name_one_subject, "\n")
        }
        estimate_model(
          drift_dm_obj = drift_dm_obj_subj, lower = lower,
          upper = upper, ...
        )
      }, error = function(e) {
        warning(
          "\nan error occured in estimate_model:\n\n", e, "\n",
          "Happened when fitting subject ", name_one_subject, ".",
          " Skipping this subject."
        )
        return(NULL)
      })

    if (!is.null(result)) {
      saveRDS(object = result, file = file.path(folder_name, file_name))
    }
  }

  if (progress == 2) {
    close(pb)
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
#'  can take some time though. Default is `TRUE`
#'
#' @details
#' with respect to the logic outlined in the details of
#' [dRiftDM::estimate_model_subjects] on the organization of fit procedures,
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
#' An object of type `dm_fits_subjects` which essentially is a list with two
#' entries:
#' - `drift_dm_fit_info`, containing a list of the main arguments when
#'  [dRiftDM::estimate_model_subjects] was originally called, including
#'  a time-stamp
#' - `all_fits`, containing a list of all the modified/fitted `drift_dm`
#'  objects. The list's entry are named according to the subjects' identifier
#'
#'
#' @export
load_fits_subjects <- function(path = "drift_dm_fits",
                               fit_procedure_name = "",
                               detailed_info = F,
                               check_data = T) {
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
    stop("no folder with a (suitable) file drift_dm_fit_info.rds found")
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
  list_of_all_fits <- stats::setNames(
    lapply(files, readRDS),
    gsub(".rds$", "", basename(files))
  )
  fits_subjects <- list(
    drift_dm_fit_info = list_of_all_fits$drift_dm_fit_info,
    all_fits = list_of_all_fits[names(list_of_all_fits) != "drift_dm_fit_info"]
  )

  class(fits_subjects) <- "dm_fits_subjects"
  if (check_data) {
    fits_subjects <- validate_fits_subjects(fits_subjects)
  }
  return(fits_subjects)
}


# ===== FUNCTION FOR HANDLING INFORMATION AND CHECKING FITS


# checks if all the information are in the fits_subjects
# object and ensures that nothing obviously fishy is going on
# in the individual model fits, by validating each model, and checking
# if the data in info_file matches the data of each individual fit object
validate_fits_subjects <- function(fits_subjects) {
  if (!inherits(fits_subjects, "dm_fits_subjects")) {
    stop("fits_subjects is not of type dm_fits_subjects")
  }

  # ensure that everything is there in the info file
  drift_dm_fit_info <- fits_subjects$drift_dm_fit_info
  names_to_check <- c(
    "time_call", "lower", "upper", "drift_dm_obj",
    "obs_data_subject", "fit_procedure_name"
  )
  if (!all(names(drift_dm_fit_info)[names(drift_dm_fit_info) != "seed"]
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
  if (!is.data.frame(drift_dm_fit_info$obs_data_subject)) {
    stop("obs_data_subject is not a data.frame")
  }
  if (!("Subject" %in% names(drift_dm_fit_info$obs_data_subject))) {
    stop("obs_data_subject has no column Subject")
  }
  if (!is.character(drift_dm_fit_info$fit_procedure_name) |
    length(drift_dm_fit_info$fit_procedure_name) != 1) {
    stop("fit_procedure_name is not of type character with length 1")
  }

  # ensure that the fits are valid drift_dm objects
  exp_model <- fits_subjects$drift_dm_fit_info$drift_dm_obj
  exp_upper <- fits_subjects$drift_dm_fit_info$upper
  exp_lower <- fits_subjects$drift_dm_fit_info$lower
  for (one_vp in names(fits_subjects$all_fits)) {
    one_fit <- fits_subjects$all_fits[[one_vp]]
    if (any(names(exp_model$prms_model) != names(one_fit$prms_model))) {
      stop(
        "parameters in the model of the fit procedure info don't match",
        "the parameters of subject ", one_vp, "'s model "
      )
    }
    if (any(exp_model$conds != one_fit$conds)) {
      stop(
        "conditions in the model of the fit procedure info don't match",
        "the conditions of subject ", one_vp, "'s model "
      )
    }
    if (any(class(exp_model) != class(one_fit))) {
      stop(
        "class of the model of the fit procedure info doesn't match",
        "the class of subject ", one_vp, "'s model "
      )
    }
    if (any(exp_model$prms_solve != one_fit$prms_solve)) {
      stop(
        "prms_solve in the model of the fit procedure info doesn't match",
        "the prms_solve of subject ", one_vp, "'s model "
      )
    }
    if (any(exp_model$free_prms != one_fit$free_prms)) {
      stop(
        "free_prms in the model of the fit procedure info doesn't match",
        "the free_prms of subject ", one_vp, "'s model "
      )
    }
    # check if parameters are in the boundary range
    fitted_model_prms <- one_fit$prms_model[names(one_fit$prms_model) %in%
      one_fit$free_prms]
    if (any(fitted_model_prms < exp_lower)) {
      stop(
        "subject ", one_vp, " provided model parameters that are smaller ",
        "than the lower parameter boundaries found in the procedure info"
      )
    }

    if (any(fitted_model_prms > exp_upper)) {
      stop(
        "subject ", one_vp, " provided model parameters that are larger ",
        "than the upper parameter boundaries found in the procedure info"
      )
    }

    # just a warning for functions because comparing functions is tricky...
    if (!isTRUE(all.equal(exp_model$comp_funs, one_fit$comp_funs))) {
      warning(
        "comp_funs in the model of the fit procedure info doesn't match",
        "the comp_funs of subject ", one_vp, "'s model "
      )
    }

    validate_drift_dm(one_fit)
  }

  # the same or different number of subjects?
  exp_obs_data <- drift_dm_fit_info$obs_data_subject
  all_subjects <- unique(as.character(exp_obs_data$Subject))
  n_exp <- length(all_subjects)
  n_real <- length(fits_subjects$all_fits)

  if (n_exp != n_real) {
    warning(paste(
      "Based on the provided data in the info file,", n_exp,
      "subjects are expected but", n_real, "were found"
    ))
  }


  # from those participants that are found... Does the data match?
  conds <- unique(exp_obs_data$Cond)
  for (one_vp in names(fits_subjects$all_fits)) {
    model_data <- summary(fits_subjects$all_fits[[one_vp]])$obs_data

    for (one_cond in conds) {
      vp_data <- exp_obs_data[exp_obs_data$Subject == one_vp, ]
      corr <- summary(vp_data$RT[vp_data$Cond == one_cond & vp_data$Error == 0])
      n_corr <- length(vp_data$RT[vp_data$Cond == one_cond & vp_data$Error == 0])
      err <- summary(vp_data$RT[vp_data$Cond == one_cond & vp_data$Error == 1])
      n_err <- length(vp_data$RT[vp_data$Cond == one_cond & vp_data$Error == 1])
      exp_corr <- model_data[rownames(model_data) == paste("correct", one_cond)]
      exp_err <- model_data[rownames(model_data) == paste("error", one_cond)]

      if (n_err > 0) {
        if (any(exp_err != c(err, n_err))) {
          warning(paste(
            "data of subject", one_vp, "in the fitted model",
            "doesn't match with the expected data based on the info file.",
            "Cond:", one_cond
          ))
        }
      }

      if (n_corr > 0) {
        if (any(exp_corr != c(corr, n_corr))) {
          warning(paste(
            "data of subject", one_vp, "in the fitted model",
            "doesn't match with the expected data based on the info file.",
            "Cond:", one_cond
          ))
        }
      }
    }
  }
  return(fits_subjects)
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
      string, "N Subjects: ",
      length(unique(fit_info$obs_data_subject$Subject)),
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
