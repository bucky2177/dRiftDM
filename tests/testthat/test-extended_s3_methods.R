# DRIFT_DM ----------------------------------------------------------------

test_that("nobs.drift_dm works as expected", {
  a_model <- ssp_dummy
  expect_identical(nobs(a_model), nrow(dRiftDM::ssp_synth_data))
})


test_that("logLik is formatted as expected", {
  a_model <- dmc_dummy
  log_like_obj <- logLik(a_model)

  expect_identical(class(log_like_obj), "logLik")
  expect_identical(attr(log_like_obj, "nobs"), 600L)
  expect_identical(attr(log_like_obj, "df"), 7)
  expect_identical(length(attributes(log_like_obj)), 3L)
})

test_that("logLik returns NULL for missing data", {
  a_model <- dmc_dummy
  obs_data(a_model) <- NULL

  expect_identical(logLik(a_model), NULL)
})


test_that("coef.drift_dm returns values as expected", {
  a_model <- dmc_dummy

  coefs_unique <- coef(a_model)
  expect_equal(
    coefs_unique,
    c(
      muc = 4,
      b = 0.6,
      non_dec = 0.3,
      sd_non_dec = 0.02,
      tau = 0.04,
      A = 0.1,
      alpha = 4
    )
  )

  prms_matrix <- a_model$flex_prms_obj$prms_matrix
  expect_identical(
    coef(a_model, select_unique = FALSE, select_custom_prms = FALSE),
    prms_matrix
  )
  prms_matrix <- a_model$flex_prms_obj$prms_matrix
  prms_matrix <- cbind(prms_matrix, peak_l = prms_matrix[, "tau"])
  expect_identical(
    coef(a_model, select_unique = FALSE, select_custom_prms = TRUE),
    prms_matrix
  )
})


# FITS_IDS_DM -------------------------------------------------------------

test_that("coef.fits_ids_dm returns values as expected", {
  all_fits <- get_example_fits(class = "fits_ids")

  coefs_unique <- coef(all_fits)
  expect_true(is.data.frame(coefs_unique))
  u_model <- all_fits$drift_dm_fit_info$drift_dm_obj
  expect_equal(names(coefs_unique), c("ID", names(coef(u_model))))

  coefs_all <- coef(all_fits, select_unique = FALSE)
  expect_true(is.data.frame(coefs_all))
  exp_coefs <- colnames(coef(u_model, select_unique = FALSE))
  expect_equal(names(coefs_all), c("ID", "Cond", exp_coefs))
})


test_that("logLik|AIC|BIC.fits_ids_dm return values as expected", {
  all_fits <- get_example_fits(class = "fits_ids")

  logs <- logLik(all_fits)
  aics <- AIC(all_fits)
  bics <- BIC(all_fits)

  expect_true(is.data.frame(logs))
  expect_equal(names(logs), c("ID", "Log_Like"))

  expect_true(is.data.frame(aics))
  expect_equal(names(aics), c("ID", "AIC"))

  expect_true(is.data.frame(bics))
  expect_equal(names(bics), c("ID", "BIC"))
})


# MCMC_DM -----------------------------------------------------------------

test_that("coef.mcmc_dm returns named vector of means by default", {
  mcmc_obj <- get_example_fits("mcmc_dm")
  exp_coefs <- names(coef(attr(mcmc_obj, "data_model")))

  # result under test
  coef_vec <- coef(mcmc_obj) # default .f = mean

  # expectations: a named numeric vector
  expect_type(coef_vec, "double")
  expect_equal(names(coef_vec), exp_coefs)

  # compare to direct computation on chains
  chains <- get_subset_chains(chains_obj = mcmc_obj, id = NULL)
  manual <- apply(chains, 1, mean)
  expect_equal(coef_vec, manual)
})

test_that("coef.mcmc_dm respects id subsetting (single ID)", {
  mcmc_obj <- get_example_fits("mcmc_dm", hierarchical = TRUE)
  id <- 1L

  coef_id <- coef(mcmc_obj, id = id)
  chains_id <- get_subset_chains(chains_obj = mcmc_obj, id = id)
  manual_id <- apply(chains_id, 1, mean)
  expect_equal(coef_id, manual_id)
})

test_that("coef.mcmc_dm with .f returning multiple stats produces a matrix", {
  mcmc_obj <- get_example_fits("mcmc_dm")

  f_stats <- function(x) c(mean = mean(x), sd = stats::sd(x))
  coef_mat <- coef(mcmc_obj, .f = f_stats)

  # matrix with rows = stats, cols = parameters
  expect_true(is.matrix(coef_mat))
  expect_equal(rownames(coef_mat), c("mean", "sd"))
})

test_that("coef.mcmc_dm with multiple ids returns a sorted data.frame", {
  mcmc_obj <- get_example_fits("mcmc_dm", hierarchical = TRUE)
  ids <- c(2L, 1L) # out of order to test sorting

  df <- coef(mcmc_obj, id = ids) # default .f = mean

  # structure
  expect_true(is.data.frame(df))
  expect_true("ID" %in% names(df))
  expect_identical(df$ID, sort(df$ID))

  # values equal to per-ID computations
  chains_2 <- get_subset_chains(chains_obj = mcmc_obj, id = 2L)
  manual_2 <- apply(chains_2, 1, mean)

  # columns for parameters must exist and be named
  param_cols <- setdiff(names(df), "ID")
  row2 <- df[df$ID == 2L, param_cols]

  # ensure same order of parameters
  expect_equal(as.numeric(row2), unname(manual_2))

  # try out with .f returning multiple stats
  f_stats <- function(x) c(mean = mean(x), sd = stats::sd(x))
  coef_mat <- coef(mcmc_obj, id = ids, .f = f_stats)
  expect_identical(names(coef_mat)[2], ".f_out")

  # try out NA as input to get data of all individuals
  df <- coef(mcmc_obj, id = NA)
  exp_ids <- dimnames(mcmc_obj$theta)[[3]]
  expect_identical(as.integer(exp_ids), df$ID)
})

test_that("coef.mcmc_dm errors as expected for wrong input", {
  mcmc_obj <- get_example_fits("mcmc_dm")

  # not a function
  expect_error(coef(mcmc_obj, .f = "test"), "must be a function")

  # id doesn't make sense in the non-hierarchical case
  expect_error(coef(mcmc_obj, id = 1), "doesn't make sense")

  # applied function can not be coerced to a vector or matrix
  weird_fun <- function(...) rnorm(sample(1:10, 1))
  expect_error(coef(mcmc_obj, .f = weird_fun))
})


# HELPER ------------------------------------------------------------------

test_that("try_cast_integer casts from character to integer", {
  expect_identical(try_cast_integer(c("1", "2", "5")), c(1L, 2L, 5L))
})

test_that("try_cast_integer returns input if not character", {
  expect_identical(try_cast_integer(15), 15)
})


test_that("try_cast_integer returns input if not only digits", {
  expect_identical(try_cast_integer(c("1.NA", "1.3")), c("1.NA", "1.3"))
})
