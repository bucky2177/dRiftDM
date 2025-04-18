test_that("estimate_model_ids and load_fits_ids works as expected", {
  # remove previous folders (might be present if the tests crashed)
  unlink(file.path(tempdir(), "temp"), recursive = T)
  unlink(file.path(tempdir(), "temp_fits"), recursive = T)

  a_model <- ratcliff_dm(t_max = 1, dt = 0.01, dx = 0.1)
  id_1 <- simulate_data(a_model, n = 300, seed = 1)
  id_1$ID <- 1
  id_2 <- simulate_data(a_model, n = 300, seed = 2)
  id_2$ID <- 2
  data_both <- rbind(id_1, id_2)
  obs_data(a_model) <- id_1
  a_model <- modify_flex_prms(a_model, instr = "b + non_dec <!>")

  expect_warning(
    estimate_model_ids(
      drift_dm_obj = a_model,
      obs_data_ids = data_both,
      lower = c(1), upper = c(5),
      fit_path = tempdir(),
      fit_procedure_name = "test_case_1",
      seed = 1,
      fit_dir = "temp_fits",
      force_refit = TRUE,
      verbose = 0,
      progress = 0,
    ), "obs_data in drift_dm_obj will be ignored"
  )


  a_model$obs_data <- NULL


  estimate_model_ids(
    drift_dm_obj = a_model,
    obs_data_ids = data_both,
    lower = c(1), upper = c(5),
    fit_path = tempdir(),
    fit_procedure_name = "test_case_2",
    folder_name = "test_case_no_2",
    seed = 2,
    force_refit = TRUE,
    fit_dir = "temp_fits",
    verbose = 0,
    progress = 0
  )

  # input checks
  expect_error(
    estimate_model_ids("wrong_input",
      obs_data_ids = data_both,
      lower = c(1), upper = c(5),
      fit_path = tempdir(),
      fit_dir = "temp_fits",
      fit_procedure_name = "test"
    ),
    "drift_dm_obj is not of type drift_dm"
  )

  expect_error(
    estimate_model_ids(a_model,
      obs_data_ids = as.matrix(data_both),
      lower = c(1), upper = c(5),
      fit_path = tempdir(),
      fit_dir = "temp_fits",
      fit_procedure_name = "test"
    ),
    "obs_data_ids is not a data.frame"
  )

  expect_error(
    estimate_model_ids(a_model,
      obs_data_ids = data_both[, c(1, 2, 3)],
      lower = c(1), upper = c(5),
      fit_path = tempdir(),
      fit_dir = "temp_fits",
      fit_procedure_name = "test"
    ),
    "no ID column"
  )

  temp_model <- a_model
  prms_solve(temp_model)["t_max"] <- 0.5
  expect_error(
    estimate_model_ids(
      temp_model,
      obs_data_ids = data_both,
      lower = c(1),
      upper = c(5),
      fit_path = tempdir(),
      fit_dir = "temp_fits",
      fit_procedure_name = "test"
    ),
    "smaller than maximum RT"
  )

  temp_data <- data_both
  temp_data <- rbind(
    temp_data,
    data.frame(RT = 0.5, Error = 0, Cond = "foo", ID = 2)
  )
  temp_data <- temp_data[temp_data$ID == 2, ]
  expect_warning(
    estimate_model_ids(
      a_model,
      obs_data_ids = temp_data,
      lower = c(1),
      upper = c(5),
      fit_path = tempdir(),
      fit_dir = "temp_fits",
      fit_procedure_name = "test", progress = 0
    ),
    "This condition will be dropped"
  )
  unlink(file.path(tempdir(), "temp_fits", "test"), recursive = T)


  expect_error(
    estimate_model_ids(a_model,
      obs_data_ids = data_both,
      seed = NA,
      lower = c(1), upper = c(5),
      fit_path = tempdir(),
      fit_dir = "temp_fits",
      fit_procedure_name = "test"
    ),
    "seed must be a single numeric"
  )
  expect_error(
    estimate_model_ids(a_model,
      obs_data_ids = data_both,
      lower = c(1), upper = c(5),
      fit_path = tempdir(),
      fit_dir = "temp_fits",
      fit_procedure_name = NA
    ),
    "fit_procedure_name must be a character"
  )

  expect_error(
    estimate_model_ids(a_model,
      obs_data_ids = data_both,
      lower = c(1), upper = c(5),
      fit_path = tempdir(),
      fit_dir = "temp_fits",
      fit_procedure_name = ""
    ),
    "empty name"
  )
  expect_error(
    estimate_model_ids(a_model,
      obs_data_ids = data_both,
      lower = c(1), upper = c(5),
      fit_path = tempdir(),
      fit_dir = "temp_fits",
      fit_procedure_name = "",
      folder_name = NA
    ),
    "folder_name must be a character"
  )

  expect_error(
    estimate_model_ids(
      a_model,
      obs_data_ids = data_both,
      lower = c(1),
      upper = c(5),
      fit_path = tempdir(),
      fit_dir = "temp_fits",
      fit_procedure_name = "",
      folder_name = "foo/bar"
    ),
    "is a plain folder name"
  )

  expect_error(
    estimate_model_ids(a_model,
      obs_data_ids = data_both,
      lower = c(1), upper = c(5),
      fit_path = tempdir(),
      fit_procedure_name = "test",
      fit_dir = NA
    ),
    "fit_dir must be a character"
  )

  expect_error(
    estimate_model_ids(
      a_model,
      obs_data_ids = data_both,
      lower = c(1),
      upper = c(5),
      fit_path = c(tempdir(), "foo"),
      fit_procedure_name = "test",
      fit_dir = "temp_fits"
    ),
    "fit_path must be a character vector of length 1"
  )

  expect_error(
    estimate_model_ids(
      a_model,
      obs_data_ids = data_both,
      lower = c(1),
      upper = c(5),
      fit_path = "foo",
      fit_procedure_name = "test",
      fit_dir = "temp_fits"
    ),
    "fit_path must provide a valid path to an existing directory"
  )

  expect_error(
    estimate_model_ids(a_model,
      obs_data_ids = data_both,
      lower = c(1), upper = c(5),
      fit_path = tempdir(),
      fit_dir = "temp_fits",
      fit_procedure_name = "test",
      force_refit = NA
    ),
    "force_refit must be a single logical"
  )

  expect_error(
    estimate_model_ids(
      a_model,
      obs_data_ids = data_both,
      lower = c(1),
      upper = c(5),
      fit_path = tempdir(),
      fit_dir = "temp_fits",
      fit_procedure_name = "test",
      progress = NA
    ),
    "progress must be numeric"
  )



  # vp is called like the info file
  temp_data <- data_both
  temp_data$ID <- ifelse(data_both$ID == 1, "drift_dm_fit_info",
    data_both$ID
  )
  expect_error(
    estimate_model_ids(a_model,
      obs_data_ids = temp_data,
      lower = c(1), upper = c(5),
      fit_path = tempdir(),
      fit_dir = "temp_fits",
      fit_procedure_name = "id_fail",
      force_refit = FALSE,
      progress = 0
    ),
    "drift_dm_fit_info not allowed as individual number/identifier"
  )
  unlink(file.path(tempdir(), "temp_fits", "id_fail"), recursive = T)


  # lower-level error by estimate_model
  temp_data <- data_both[data_both$ID == 1, ]
  expect_warning(
    estimate_model_ids(a_model,
      obs_data_ids = temp_data,
      lower = c(1), upper = c(5, 2),
      fit_path = tempdir(),
      fit_dir = "temp_fits",
      fit_procedure_name = "lower_fail",
      force_refit = FALSE,
      progress = 0
    ),
    "Happened when fitting individual 1"
  )
  unlink(file.path(tempdir(), "temp_fits", "lower_fail"), recursive = T)


  # ensure that everything is skipped, tested below when loading data
  id_1 <- simulate_data(a_model, n = 300, seed = 1)
  id_1$ID <- 1
  id_2 <- simulate_data(a_model, n = 300, seed = 4)
  id_2$ID <- 2
  id_3 <- simulate_data(a_model, n = 300, seed = 5)
  id_3$ID <- 3
  data_all <- rbind(id_1, id_2, id_3)

  expect_message(
    estimate_model_ids(
      drift_dm_obj = a_model,
      obs_data_ids = data_all,
      lower = c(1), upper = c(5),
      fit_path = tempdir(),
      fit_procedure_name = "test_case_2",
      folder_name = "test_case_no_2",
      seed = 2,
      force_refit = FALSE,
      fit_dir = "temp_fits",
      verbose = 0,
      progress = 0,
    ), "Skipping individuals"
  )

  #### NOW THE LOADING

  # wrong path
  expect_error(load_fits_ids("falkenhorst"), "no directory")

  # wrong identifier
  expect_error(
    load_fits_ids(
      path = file.path(tempdir(), "temp_fits"),
      fit_procedure_name = "test_cas_e1"
    ),
    "no folder with a \\(suitable\\) file drift_dm_fit_info.rds found"
  )

  # load one case
  expect_warning(
    load_fits_ids(
      path = file.path(tempdir(), "temp_fits"),
      fit_procedure_name = "test_case_2"
    ), "data of individual 2"
  )

  # load one case
  loaded_data <- suppressWarnings(
    load_fits_ids(
      path = file.path(tempdir(), "temp_fits"),
      fit_procedure_name = "test_case_2"
    )
  )

  expect_equal(data_all, loaded_data$drift_dm_fit_info$obs_data_ids)
  expect_equal(a_model, loaded_data$drift_dm_fit_info$drift_dm_obj)
  expect_equal(1, loaded_data$drift_dm_fit_info$lower)
  expect_equal(5, loaded_data$drift_dm_fit_info$upper)
  expect_equal(2, loaded_data$drift_dm_fit_info$seed)
  expect_equal("test_case_2", loaded_data$drift_dm_fit_info$fit_procedure_name)

  ## additional input checks for load_fits_ids
  expect_error(
    load_fits_ids(
      path = NA,
      fit_procedure_name = "test_case_2"
    ), "not a character vector"
  )

  expect_error(
    load_fits_ids(
      path = file.path(tempdir(), "temp_fits"),
      fit_procedure_name = NA
    ), "not a character vector"
  )

  expect_error(
    load_fits_ids(
      path = file.path(tempdir(), "temp_fits"),
      fit_procedure_name = "test_case_1", detailed_info = "NA"
    ), "not logical"
  )

  expect_error(
    load_fits_ids(
      path = file.path(tempdir(), "temp_fits"),
      fit_procedure_name = "test_case_1", check_data = "NA"
    ), "not logical"
  )

  expect_error(
    load_fits_ids(
      path = file.path(tempdir(), "temp_fits"),
      fit_procedure_name = "test_case_1", progress = "doo"
    ), "must be 0, 1, or 2"
  )
})


test_that("load_fits_ids menu and errors work as expected", {
  local({
    # Here, we override the menu function to force expected choice
    suppressWarnings(
      local_mocked_bindings(
        menu = function(choices, graphics = FALSE, title = NULL) 1,
        .package = "utils"
      )
    )

    case_1 <- load_fits_ids(
      path = file.path(tempdir(), "temp_fits"),
      fit_procedure_name = "test_case_1"
    )
    case_1_menu <- load_fits_ids(
      path = file.path(tempdir(), "temp_fits"),
      fit_procedure_name = "",
      check_data = F,
      detailed_info = T,
      progress = 0
    )

    expect_equal(
      case_1_menu,
      case_1
    )
  })


  # option 0
  local({
    # Here, we override the menu function to force expected choice
    suppressWarnings(
      local_mocked_bindings(
        menu = function(choices, graphics = FALSE, title = NULL) 0,
        .package = "utils"
      )
    )

    expect_message(expect_equal(
      load_fits_ids(
        path = file.path(tempdir(), "temp_fits"),
        check_data = F, progress = 0
      ),
      NULL
    ), "exiting selection")
  })
})


test_that("validate_models errs as expected", {
  # requires estimate_model_id and load_fits_ids works as expected
  case_1 <- load_fits_ids(
    path = file.path(tempdir(), "temp_fits"),
    fit_procedure_name = "test_case_1",
    check_data = F, progress = 1
  )

  temp <- case_1
  class(temp) <- "foo"
  expect_error(
    validate_fits_ids(temp, progress = 0), "not of type fits_ids_dm"
  )

  # wrong entries
  temp <- case_1
  temp$drift_dm_fit_info$bar <- 5
  expect_error(
    validate_fits_ids(temp, progress = 0), "unexpected info entries"
  )

  temp <- case_1
  temp$drift_dm_fit_info$time_call <- NULL
  expect_error(
    validate_fits_ids(temp, progress = 0),
    "contains not all expected info entries"
  )

  # no time object
  temp <- case_1
  temp$drift_dm_fit_info$time_call <- 20
  expect_error(
    validate_fits_ids(temp, progress = 0), "time_call"
  )

  # lower an upper
  temp <- case_1
  temp$drift_dm_fit_info$upper <- NA
  expect_error(
    validate_fits_ids(temp, progress = 0), "valid numeric values"
  )

  temp <- case_1
  temp$drift_dm_fit_info$lower <- NA
  expect_error(
    validate_fits_ids(temp, progress = 0), "valid numeric values"
  )



  # seed
  temp <- case_1
  temp$drift_dm_fit_info$seed <- NA
  expect_error(
    validate_fits_ids(temp, progress = 0),
    "seed is not a single number or NULL"
  )

  # data frame
  temp <- case_1
  temp$drift_dm_fit_info$obs_data_ids <- "foo"
  expect_error(
    validate_fits_ids(temp, progress = 0), "obs_data_ids is not a data.frame"
  )

  temp <- case_1
  temp$drift_dm_fit_info$obs_data_ids$ID <- NULL
  expect_error(
    validate_fits_ids(temp, progress = 0), "no column ID"
  )

  # fit procedure name
  temp <- case_1
  temp$drift_dm_fit_info$fit_procedure_name <- c("a", "b")
  expect_error(
    validate_fits_ids(temp, progress = 0),
    "fit_procedure_name is not of type character"
  )

  # modify one individual
  temp <- case_1
  temp$all_fits$`2`$flex_prms_obj$linear_internal_list$muc$null <- 2
  expect_error(
    validate_fits_ids(temp, progress = 0), "linear_list of individual 2"
  )

  temp <- case_1
  temp$all_fits$`1`$flex_prms_obj$internal_list$muc$null <- 2
  expect_error(
    validate_fits_ids(temp, progress = 0), "internal_list of individual 1"
  )

  temp <- case_1
  rownames(temp$all_fits$`1`$flex_prms_obj$prms_matrix) <- c("foo")
  expect_error(
    validate_fits_ids(temp, progress = 0), "row_names of individual 1"
  )

  temp <- case_1
  colnames(temp$all_fits$`1`$flex_prms_obj$prms_matrix) <- c("muc", "b", "foo")
  expect_error(
    validate_fits_ids(temp, progress = 0), "col_names of individual 1"
  )

  temp <- case_1
  class(temp$all_fits$`2`) <- c("dmc_dm", "drift_dm")
  expect_error(
    validate_fits_ids(temp, progress = 0), "class of individual 2"
  )


  temp <- case_1
  prms_solve(temp$all_fits$`2`)["sigma"] <- 2
  expect_error(
    validate_fits_ids(temp, progress = 0), "prms_solve of individual 2"
  )

  temp <- case_1
  solver(temp$all_fits$`2`) <- "im_zero"
  expect_error(
    validate_fits_ids(temp, progress = 0), "solver of individual 2"
  )

  temp <- case_1
  temp$all_fits$`1`$flex_prms_obj$prms_matrix[1, 1] <- 0.5
  expect_error(
    validate_fits_ids(temp, progress = 0), "that are smaller than"
  )


  temp <- case_1
  temp$all_fits$`1`$flex_prms_obj$prms_matrix[1, 1] <- Inf
  expect_error(
    validate_fits_ids(temp, progress = 0), "that are larger than"
  )


  temp <- case_1
  temp$all_fits$`2`$comp_funs$x_fun <- function(prms_model, prms_solve,
                                                x_vec, one_cond, ddm_opts) {
    return(stats::dbeta(x_vec, 1, 1))
  }
  expect_warning(
    validate_fits_ids(temp, progress = 0), "comp_funs"
  )

  temp <- case_1
  b_coding(temp$all_fits$`1`)["column"] <- "Correct"
  expect_error(
    validate_fits_ids(temp, progress = 0), "individual 1 provided a b_coding"
  )


  # modify the data
  temp <- case_1
  obs_data <- temp$drift_dm_fit_info$obs_data_ids
  obs_data <- obs_data[obs_data$ID == 1, ]
  temp$drift_dm_fit_info$obs_data_ids <- obs_data
  expect_warning(
    validate_fits_ids(temp, progress = 0),
    "1 individuals are expected but 2 were found"
  )

  temp <- case_1
  temp$drift_dm_fit_info$obs_data_ids$RT[1] <- 0.1
  expect_warning(
    validate_fits_ids(temp, progress = 0),
    "doesn't match with the expected data"
  )


  # modify the ddm_opts
  temp <- case_1
  ddm_opts(temp$drift_dm_fit_info$drift_dm_obj) <- "foo"
  expect_error(
    validate_fits_ids(temp, progress = 0),
    "doesn't match the ddm_opts"
  )

  unlink(file.path(tempdir(), "temp_fits"), recursive = TRUE)
})


test_that("start_vals work as expected", {
  a_model <- ratcliff_dm(t_max = 1, dt = 0.01, dx = 0.1)
  flex_prms(a_model) <- flex_prms(c(muc = 2, b = 0.5, non_dec = 0.3),
    conds = c("comp", "incomp"),
    instr = "b ~"
  )
  id_1 <- simulate_data(a_model, n = 100, seed = 1)
  id_1$ID <- 1
  start_vals <- data.frame(
    ID = 1, muc = 5, b.comp = 0.4, b.incomp = 0.5,
    non_dec = 0.2
  )
  expect_warning(
    estimate_model_ids(a_model, id_1,
      lower = c(2, 0.2, 0.1),
      upper = c(6, 0.8, 0.4),
      fit_path = tempdir(),
      fit_procedure_name = "foo",
      fit_dir = "temp",
      use_de_optim = F,
      start_vals = start_vals
    ),
    "passing back unmodified object"
  )
  unmodifed <- readRDS(file.path(tempdir(), "temp", "foo", "1.rds"))
  expect_true(
    all(unmodifed$flex_prms_obj$prms_matrix[1, ] ==
      start_vals[c("muc", "b.comp", "non_dec")])
  )
  expect_true(
    all(unmodifed$flex_prms_obj$prms_matrix[2, ] ==
      start_vals[c("muc", "b.incomp", "non_dec")])
  )
  unlink(file.path(tempdir(), "temp"), recursive = T)

  # default
  expect_warning(
    estimate_model_ids(a_model, id_1,
      lower = c(2, 0.2, 0.1),
      upper = c(6, 0.8, 0.4),
      fit_path = tempdir(),
      fit_procedure_name = "foo",
      fit_dir = "temp",
      use_de_optim = F,
      start_vals = NULL
    ),
    "passing back unmodified object"
  )
  unmodifed <- readRDS(file.path(tempdir(), "temp", "foo", "1.rds"))
  expect_equal(a_model$flex_prms_obj, unmodifed$flex_prms_obj)
  unlink(file.path(tempdir(), "temp"), recursive = T)

  # wrong input
  expect_error(
    estimate_model_ids(a_model, id_1,
      lower = c(2, 0.2, 0.1),
      upper = c(6, 0.8, 0.4),
      fit_path = tempdir(),
      fit_procedure_name = "foo",
      fit_dir = "temp",
      use_de_optim = F,
      start_vals = as.matrix(start_vals)
    ),
    "must be a data.frame"
  )


  temp_start <- start_vals[, names(start_vals) != "ID"]
  expect_error(
    estimate_model_ids(a_model, id_1,
      lower = c(2, 0.2, 0.1),
      upper = c(6, 0.8, 0.4),
      fit_path = tempdir(),
      fit_procedure_name = "foo",
      fit_dir = "temp",
      use_de_optim = F,
      start_vals = temp_start,
    ),
    "no ID column found"
  )

  expect_error(
    estimate_model_ids(a_model, id_1,
      lower = c(2, 0.2, 0.1),
      upper = c(6, 0.8, 0.4),
      fit_path = tempdir(),
      fit_procedure_name = "foo",
      fit_dir = "temp",
      use_de_optim = F,
      start_vals = start_vals[c("ID", "muc")]
    ),
    "don't match the unique free parameters"
  )

  test_starts <- cbind(start_vals, bla = 3)
  expect_error(
    estimate_model_ids(a_model, id_1,
      lower = c(2, 0.2, 0.1),
      upper = c(6, 0.8, 0.4),
      fit_path = tempdir(),
      fit_procedure_name = "foo",
      fit_dir = "temp",
      use_de_optim = F,
      start_vals = test_starts
    ),
    "don't match the unique free parameters"
  )

  test_starts <- rbind(start_vals, start_vals)
  test_starts$ID <- c(1, 2)
  expect_error(
    estimate_model_ids(a_model, id_1,
      lower = c(2, 0.2, 0.1),
      upper = c(6, 0.8, 0.4),
      fit_path = tempdir(),
      fit_procedure_name = "foo",
      fit_dir = "temp",
      use_de_optim = F,
      start_vals = test_starts
    ),
    "individuals in start_vals that are not in obs_data_ids"
  )

  temp_data <- rbind(id_1, id_1)
  temp_data$ID[(nrow(temp_data) / 2 + 1):nrow(temp_data)] <- 2
  expect_error(
    estimate_model_ids(
      a_model,
      temp_data,
      lower = c(2, 0.2, 0.1),
      upper = c(6, 0.8, 0.4),
      fit_path = tempdir(),
      fit_procedure_name = "foo",
      fit_dir = "temp",
      use_de_optim = F,
      start_vals = start_vals
    ),
    "different number of individuals "
  )







  test_starts$ID <- c(1, 1)
  expect_error(
    estimate_model_ids(a_model, id_1,
      lower = c(2, 0.2, 0.1),
      upper = c(6, 0.8, 0.4),
      fit_path = tempdir(),
      fit_procedure_name = "foo",
      fit_dir = "temp",
      use_de_optim = F,
      start_vals = test_starts
    ),
    "not unique"
  )

  # wrong order relative to free_prms should not matter
  start_vals <- data.frame(
    ID = 1, b.incomp = 1, b.comp = 0.5, muc = 2,
    non_dec = 3
  )
  expect_warning(
    estimate_model_ids(a_model, id_1,
      lower = c(2, 0.2, 0.1),
      upper = c(6, 0.8, 0.4),
      fit_path = tempdir(),
      fit_procedure_name = "foo",
      fit_dir = "temp",
      use_de_optim = F,
      start_vals = start_vals
    ),
    "passing back unmodified object"
  )
  unmodifed <- readRDS(file.path(tempdir(), "temp", "foo", "1.rds"))
  expect_true(all(unmodifed$flex_prms_obj$prms_matrix[1, ] == c(2, 0.5, 3)))
  expect_true(all(unmodifed$flex_prms_obj$prms_matrix[2, ] == c(2, 1, 3)))
  unlink(file.path(tempdir(), "temp"), recursive = T)
})
