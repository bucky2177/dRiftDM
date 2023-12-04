test_that("estimate_model_subject and load_fits_subjects works as expected", {
  unlink(test_path("temp_fits"), recursive = TRUE)

  a_model <- ratcliff_dm(t_max = 1, dt = 0.01, dx = 0.1)
  subject_1 <- simulate_data(drift_dm_obj = a_model, n = 300, seed = 1)
  subject_1$Subject <- 1
  subject_2 <- simulate_data(drift_dm_obj = a_model, n = 300, seed = 2)
  subject_2$Subject <- 2
  data_both <- rbind(subject_1, subject_2)
  a_model <- set_obs_data(a_model, subject_1)
  a_model <- set_free_prms(a_model, c("muc"))
  expect_snapshot(
    expect_warning(
      estimate_model_subjects(
        drift_dm_obj = a_model,
        obs_data_subject = data_both,
        lower = c(1), upper = c(5),
        fit_procedure_name = "test_case_1",
        seed = 1,
        fit_dir = test_path("temp_fits"),
        force_refit = TRUE,
        verbose = 1,
      ), "obs_data in drift_dm_obj will be ignored"
    )
  )

  a_model$obs_data <- NULL

  expect_snapshot(
    estimate_model_subjects(
      drift_dm_obj = a_model,
      obs_data_subject = data_both,
      lower = c(1), upper = c(5),
      fit_procedure_name = "test_case_2",
      folder_name = "test_case_no_2",
      seed = 2,
      force_refit = TRUE,
      fit_dir = test_path("temp_fits"),
      verbose = 0
    )
  )

  # input checks
  expect_error(
    estimate_model_subjects("wrong_input",
      obs_data_subject = data_both,
      lower = c(1), upper = c(5),
      fit_dir = test_path("temp_fits"),
      fit_procedure_name = "test"
    ),
    "drift_dm_obj is not of type drift_dm"
  )

  expect_error(
    estimate_model_subjects(a_model,
      obs_data_subject = as.matrix(data_both),
      lower = c(1), upper = c(5),
      fit_dir = test_path("temp_fits"),
      fit_procedure_name = "test"
    ),
    "obs_data_subject is not a data.frame"
  )

  expect_error(
    estimate_model_subjects(a_model,
      obs_data_subject = data_both[, c(1, 2, 3)],
      lower = c(1), upper = c(5),
      fit_dir = test_path("temp_fits"),
      fit_procedure_name = "test"
    ),
    "no Subject column"
  )

  expect_error(
    estimate_model_subjects(a_model,
      obs_data_subject = data_both,
      seed = NA,
      lower = c(1), upper = c(5),
      fit_dir = test_path("temp_fits"),
      fit_procedure_name = "test"
    ),
    "seed must be a single numeric"
  )
  expect_error(
    estimate_model_subjects(a_model,
      obs_data_subject = data_both,
      lower = c(1), upper = c(5),
      fit_dir = test_path("temp_fits"),
      fit_procedure_name = NA
    ),
    "fit_procedure_name must be a character"
  )

  expect_error(
    estimate_model_subjects(a_model,
      obs_data_subject = data_both,
      lower = c(1), upper = c(5),
      fit_dir = test_path("temp_fits"),
      fit_procedure_name = ""
    ),
    "empty name"
  )
  expect_error(
    estimate_model_subjects(a_model,
      obs_data_subject = data_both,
      lower = c(1), upper = c(5),
      fit_dir = test_path("temp_fits"),
      fit_procedure_name = "",
      folder = NA
    ),
    "folder_name must be a character"
  )

  expect_error(
    estimate_model_subjects(a_model,
      obs_data_subject = data_both,
      lower = c(1), upper = c(5),
      fit_procedure_name = "test",
      fit_dir = NA
    ),
    "fit_dir must be a character"
  )

  expect_error(
    estimate_model_subjects(a_model,
      obs_data_subject = data_both,
      lower = c(1), upper = c(5),
      fit_dir = test_path("temp_fits"),
      fit_procedure_name = "test",
      force_refit = NA
    ),
    "force_refit must be a single logical"
  )


  # vp is called like the info file
  temp_data <- data_both
  temp_data$Subject <- ifelse(data_both$Subject == 1, "drift_dm_fit_info",
    data_both$Subject
  )
  expect_error(
    estimate_model_subjects(a_model,
      obs_data_subject = temp_data,
      lower = c(1), upper = c(5),
      fit_dir = test_path("temp_fits"),
      fit_procedure_name = "subject_fail",
      force_refit = FALSE,
      progress = 0
    ),
    "drift_dm_fit_info not allowed as subject number/identifier"
  )
  unlink(test_path("temp_fits", "subject_fail"), recursive = T)


  # lower-level error by estimate_model
  temp_data <- data_both[data_both$Subject == 1, ]
  expect_warning(
    estimate_model_subjects(a_model,
      obs_data_subject = temp_data,
      lower = c(1), upper = c(5, 2),
      fit_dir = test_path("temp_fits"),
      fit_procedure_name = "lower_fail",
      force_refit = FALSE,
      progress = 0
    ),
    "Happened when fitting subject 1"
  )
  unlink(test_path("temp_fits", "lower_fail"), recursive = T)


  # ensure that everything is skipped, tested below when loading data
  subject_1 <- simulate_data(drift_dm_obj = a_model, n = 300, seed = 1)
  subject_1$Subject <- 1
  subject_2 <- simulate_data(drift_dm_obj = a_model, n = 300, seed = 4)
  subject_2$Subject <- 2
  subject_3 <- simulate_data(drift_dm_obj = a_model, n = 300, seed = 5)
  subject_3$Subject <- 3
  data_all <- rbind(subject_1, subject_2, subject_3)

  expect_snapshot(
    estimate_model_subjects(
      drift_dm_obj = a_model,
      obs_data_subject = data_all,
      lower = c(1), upper = c(5),
      fit_procedure_name = "test_case_2",
      folder_name = "test_case_no_2",
      seed = 2,
      force_refit = FALSE,
      fit_dir = test_path("temp_fits"),
      verbose = 0
    )
  )

  #### NOW THE LOADING

  # wrong path
  expect_error(load_fits_subjects(), "no directory")

  # wrong identifier
  expect_error(
    load_fits_subjects(test_path("temp_fits"), "test_cas_e1"),
    "no folder with a \\(suitable\\) file drift_dm_fit_info.rds found"
  )

  # load one case
  expect_warning(
    load_fits_subjects(
      path = test_path("temp_fits"),
      fit_procedure_name = "test_case_2"
    ), "data of subject 2"
  )

  # load one case
  loaded_data <- suppressWarnings(
    load_fits_subjects(
      path = test_path("temp_fits"),
      fit_procedure_name = "test_case_2"
    )
  )

  expect_equal(data_all, loaded_data$drift_dm_fit_info$obs_data_subject)
  expect_equal(a_model, loaded_data$drift_dm_fit_info$drift_dm_obj)
  expect_equal(1, loaded_data$drift_dm_fit_info$lower)
  expect_equal(5, loaded_data$drift_dm_fit_info$upper)
  expect_equal(2, loaded_data$drift_dm_fit_info$seed)
  expect_equal("test_case_2", loaded_data$drift_dm_fit_info$fit_procedure_name)
})


test_that("load_fits_subjects menu and erros work as expected", {
  local({
    # Here, we override the menu function to force expected choice
    suppressWarnings(
      local_mock(menu = function(choices, title = NULL) 1)
    )

    case_1 <- load_fits_subjects(
      path = test_path("temp_fits"),
      fit_procedure_name = "test_case_1",
      check_data = F
    )
    case_1_menu <- load_fits_subjects(
      path = test_path("temp_fits"),
      fit_procedure_name = "",
      check_data = F,
      detailed_info = T
    )

    expect_equal(
      case_1_menu,
      case_1
    )
  })
})


test_that("validate_models errs as expected", {
  case_1 <- load_fits_subjects(
    path = test_path("temp_fits"),
    fit_procedure_name = "test_case_1",
    check_data = F
  )

  temp <- case_1
  class(temp) <- "foo"
  expect_error(
    validate_fits_subjects(temp), "not of type dm_fits_subjects"
  )

  # wrong entries
  temp <- case_1
  temp$drift_dm_fit_info$bar <- 5
  expect_error(
    validate_fits_subjects(temp), "unexpected info entries"
  )

  temp <- case_1
  temp$drift_dm_fit_info$time_call <- NULL
  expect_error(
    validate_fits_subjects(temp), "contains not all expected info entries"
  )

  # no time object
  temp <- case_1
  temp$drift_dm_fit_info$time_call <- "20"
  expect_error(
    validate_fits_subjects(temp), "time_call"
  )

  # lower an upper
  temp <- case_1
  temp$drift_dm_fit_info$upper <- NA
  expect_error(
    validate_fits_subjects(temp), "upper is not of type numeric"
  )

  temp <- case_1
  temp$drift_dm_fit_info$lower <- NA
  expect_error(
    validate_fits_subjects(temp), "lower is not of type numeric"
  )

  temp <- case_1
  temp$drift_dm_fit_info$lower <- c(1, 2)
  temp$drift_dm_fit_info$upper <- c(1, 2, 3)
  expect_error(
    validate_fits_subjects(temp), "length of upper and lower don't match"
  )

  # free_prms
  temp <- case_1
  temp$drift_dm_fit_info$lower <- c(1, 2, 3)
  temp$drift_dm_fit_info$upper <- c(1, 2, 3)
  expect_error(
    validate_fits_subjects(temp),
    "length of upper/lower don't match the number of free parameters"
  )

  # seed
  temp <- case_1
  temp$drift_dm_fit_info$seed <- NA
  expect_error(
    validate_fits_subjects(temp), "seed is not a single number or NULL"
  )

  # data frame
  temp <- case_1
  temp$drift_dm_fit_info$obs_data_subject <- "foo"
  expect_error(
    validate_fits_subjects(temp), "obs_data_subject is not a data.frame"
  )

  temp <- case_1
  temp$drift_dm_fit_info$obs_data_subject$Subject <- NULL
  expect_error(
    validate_fits_subjects(temp), "no column Subject"
  )

  # fit procedure name
  temp <- case_1
  temp$drift_dm_fit_info$fit_procedure_name <- c("a", "b")
  expect_error(
    validate_fits_subjects(temp), "fit_procedure_name is not of type character"
  )

  # modify one subject
  temp <- case_1
  names(temp$all_fits$`2`$prms_model) <- c("muc", "b", "foo")
  expect_error(
    validate_fits_subjects(temp), "parameters of subject 2"
  )

  temp <- case_1
  temp$all_fits$`2`$conds <- c("comp", "incomp")
  expect_error(
    validate_fits_subjects(temp), "conditions of subject 2"
  )

  temp <- case_1
  class(temp$all_fits$`2`) <- c("dmc_dm", "drift_dm")
  expect_error(
    validate_fits_subjects(temp), "class of subject 2"
  )


  temp <- case_1
  temp$all_fits$`2` <- set_solver_settings(temp$all_fits$`2`, "sigma", 2)
  expect_error(
    validate_fits_subjects(temp), "prms_solve of subject 2"
  )

  temp <- case_1
  temp$all_fits$`2` <- set_free_prms(temp$all_fits$`2`, c("b"))
  expect_error(
    validate_fits_subjects(temp), "free_prms of subject 2"
  )

  temp <- case_1
  temp$all_fits$`1`$prms_model[1] <- 0.5
  expect_error(
    validate_fits_subjects(temp), "that are smaller than"
  )


  temp <- case_1
  temp$all_fits$`1`$prms_model[1] <- Inf
  expect_error(
    validate_fits_subjects(temp), "that are larger than"
  )


  temp <- case_1
  temp$all_fits$`2`$comp_funs$x_fun <- function(drift_dm_obj, x_vec, one_cond) {
    return(stats::dbeta(x_vec, 1, 1))
  }
  expect_warning(
    validate_fits_subjects(temp), "comp_funs"
  )

  # modify the data
  temp <- case_1
  obs_data <- temp$drift_dm_fit_info$obs_data_subject
  obs_data <- obs_data[obs_data$Subject == 1, ]
  temp$drift_dm_fit_info$obs_data_subject <- obs_data
  expect_warning(
    validate_fits_subjects(temp), "1 subjects are expected but 2 were found"
  )

  temp <- case_1
  temp$drift_dm_fit_info$obs_data_subject$RT[1] <- 0.1
  expect_warning(
    validate_fits_subjects(temp), "doesn't match with the expected data"
  )

  unlink(test_path("temp_fits"), recursive = TRUE)
})
