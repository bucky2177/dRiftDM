test_that("input checks estimate_model", {
  w_estimate_model <- function(...) {
    suppress_lifecycle_deprecated(estimate_model(...))
  }

  # first tests without data
  a_model <- ratcliff_dummy
  a_model$obs_data <- NULL

  lifecycle::expect_deprecated(
    expect_error(estimate_model("hallo"), "drift_dm")
  )
  expect_warning(
    w_estimate_model(a_model, lower = c(1, 0.2, 0.1), upper = c(7, 0.8, 0.6)),
    "No data set"
  )
  return_val <- suppressWarnings(w_estimate_model(
    a_model,
    lower = c(1, 0.2, 0.1),
    upper = c(7, 0.8, 0.6)
  ))
  expect_identical(return_val, a_model)

  # first tests with data
  a_model <- ratcliff_dummy
  expect_error(
    w_estimate_model(a_model, lower = c(0.2, 0.1), upper = c(7, 0.8, 0.6)),
    "Double-check your arguments"
  )

  expect_error(
    w_estimate_model(a_model, lower = c("1", 0.2, 0.1), upper = c(7, 0.8, 0.6)),
    "illegal data type"
  )

  expect_error(
    w_estimate_model(a_model, lower = c(0.2, 0.2, 0.1), upper = c(1, Inf, 0.6)),
    "illegal data type"
  )

  expect_error(
    w_estimate_model(a_model, lower = c(0.2, 0.1), upper = c(7, 0.8)),
    "Double-check your arguments"
  )

  expect_error(
    w_estimate_model(
      a_model,
      lower = c(1, 0.2, 0.1),
      upper = c(7, 0.8, 0.8),
      verbose = "a"
    ),
    "must be numeric"
  )

  expect_error(
    w_estimate_model(a_model, lower = c(a = 1, 2, 3), upper = c(5, 3, 4)),
    "Parameter labels"
  )

  expect_error(
    w_estimate_model(
      a_model,
      lower = c(muc = 1, b = 0.3, foo = 0.4),
      upper = c(muc = 3, b = 5, non_dec = 0.4)
    ),
    "missing input values"
  )

  expect_error(
    w_estimate_model(
      a_model,
      lower = c(muc = 1, b = 0.3, non_dec = 0.4),
      upper = c(muc = 3, foo = 5, non_dec = 0.4)
    ),
    "missing input values"
  )

  expect_warning(
    w_estimate_model(
      a_model,
      lower = c(1, 0.2, 0.1),
      upper = c(7, 0.8, 0.8),
      use_nmkb = F,
      use_de_optim = F
    ),
    "No estimation done"
  )

  return_val <- suppressWarnings(w_estimate_model(
    a_model,
    lower = c(1, 0.2, 0.1),
    upper = c(7, 0.8, 0.8),
    use_nmkb = F,
    use_de_optim = F
  ))
  expect_identical(return_val, a_model)

  expect_error(
    w_estimate_model(
      a_model,
      lower = c(1, 0.2, 0.1),
      upper = c(7, 0.8, 0.6),
      seed = "a"
    ),
    "seed"
  )
  expect_error(
    w_estimate_model(
      a_model,
      lower = c(1, 0.2, 0.1),
      upper = c(7, 0.8, 0.6),
      de_n_cores = "a"
    ),
    "de_n_cores"
  )

  expect_error(
    w_estimate_model(
      a_model,
      lower = c(1, 0.2, 0.1),
      upper = c(7, 0.8, 0.6),
      de_control = "a"
    ),
    "control"
  )
  expect_error(
    w_estimate_model(
      a_model,
      lower = c(1, 0.2, 0.1),
      upper = c(7, 0.8, 0.6),
      use_de_optim = FALSE,
      use_nmkb = TRUE,
      nmkb_control = "a"
    ),
    "control"
  )

  # some check with list
  expect_error(
    w_estimate_model(
      a_model,
      lower = list(
        default_values = c(muc = 1, b = 0.3, non_dec = 0.4),
        null = c(foo = 3)
      ),
      upper = c(muc = 3, b = 5, non_dec = 0.4)
    ),
    "not part of the default values"
  )
})

test_that("snapshot of the model running through nmkb", {
  w_estimate_model <- function(...) {
    suppress_lifecycle_deprecated(estimate_model(...))
  }

  a_model <- ratcliff_dummy
  prms_solve(a_model) <- c(dt = .005, dx = .05)

  new_data <- simulate_data(a_model, n = 2000, seed = 1)
  obs_data(a_model) <- new_data
  coef(a_model) <- c(muc = 2, b = 0.3, non_dec = 0.2)

  # with model parameters
  expect_snapshot(
    w_estimate_model(
      a_model,
      lower = c(1, 0.2, 0.1),
      upper = c(7, 0.8, 0.6),
      use_de_optim = F,
      use_nmkb = T,
      verbose = 2
    )
  )
})

test_that("behavior of DE and nmkb toggles", {
  w_estimate_model <- function(...) {
    suppress_lifecycle_deprecated(estimate_model(...))
  }

  # get a dummy model
  a_model <- suppressWarnings(dmc_dm(
    t_max = 1,
    dt = 0.01,
    dx = 0.1,
    instr = "b + sd_non_dec + tau + a + A + alpha <!>"
  ))
  subject <- simulate_data(a_model, n = 300, seed = 1)
  obs_data(a_model) <- subject

  # both toggles FALSE
  expect_warning(
    w_estimate_model(
      drift_dm_obj = a_model,
      lower = c(1, 0.1),
      upper = c(5, 0.5),
      seed = 1,
      use_de_optim = FALSE
    ),
    "No estimation done"
  )

  # DE TRUE nmkb FALSE
  expect_snapshot(
    w_estimate_model(
      drift_dm_obj = a_model,
      lower = c(1, 0.1),
      upper = c(5, 0.5),
      seed = 1,
      verbose = 1,
      use_de_optim = TRUE,
      de_control = list(
        reltol = 1e-5,
        steptol = 10,
        itermax = 200,
        trace = TRUE
      )
    )
  )

  # DE FALSE and nmkb TRUE -> see above

  # nmkb TRUE but univariate case
  a_model <- modify_flex_prms(a_model, instr = "non_dec <!>")
  expect_warning(
    w_estimate_model(
      drift_dm_obj = a_model,
      lower = c(1),
      upper = c(5),
      seed = 1,
      use_nmkb = TRUE,
      use_de_optim = FALSE,
      verbose = 0
    ),
    "univariate optimization"
  )

  # DE TRUE nmkb FALSE - parallel
  a_model <- modify_flex_prms(a_model, instr = "non_dec ~!")
  expect_snapshot(
    w_estimate_model(
      drift_dm_obj = a_model,
      lower = c(1, 0.1),
      upper = c(5, 0.5),
      seed = 1,
      verbose = 1,
      use_de_optim = TRUE,
      de_control = list(
        reltol = 1e-5,
        steptol = 10,
        itermax = 200,
        trace = TRUE
      ),
      de_n_cores = 2
    )
  )
})
