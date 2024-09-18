test_that("input checks estimate_model", {
  # first tests without data
  a_model <- ratcliff_dm()

  expect_error(estimate_model("hallo"), "drift_dm")
  expect_warning(estimate_model(a_model,
    lower = c(1, 0.2, 0.1),
    upper = c(7, 0.8, 0.6)
  ), "No data set")
  return_val <- suppressWarnings(estimate_model(a_model,
    lower = c(1, 0.2, 0.1),
    upper = c(7, 0.8, 0.6)
  ))
  expect_identical(return_val, a_model)


  # first tests with data
  a_model <- ratcliff_dm(obs_data = ratcliff_synth_data)
  expect_error(estimate_model(a_model,
    lower = c(0.2, 0.1),
    upper = c(7, 0.8, 0.6)
  ), "length")

  expect_error(estimate_model(a_model,
    lower = c("1", 0.2, 0.1),
    upper = c(7, 0.8, 0.6)
  ), "numeric")

  expect_error(estimate_model(a_model,
    lower = c(0.2, 0.1),
    upper = c(7, 0.8)
  ), "number of free_prms")

  expect_error(
    estimate_model(a_model,
      lower = c(1, 0.2, 0.1),
      upper = c(7, 0.8, 0.8), verbose = "a"
    ),
    "must be numeric of either 0, 1, or 2"
  )
  expect_error(
    estimate_model(a_model,
      lower = c(1, 0.2, 0.1),
      upper = c(7, 0.8, 0.8), use_de_optim = "a"
    ),
    "logical"
  )
  expect_error(
    estimate_model(a_model,
      lower = c(1, 0.2, 0.1),
      upper = c(7, 0.8, 0.8), use_nmkb = "a"
    ),
    "logical"
  )

  expect_error(
    estimate_model(a_model,
                   lower = c(a = 1, 2, 3), upper = c(5, 3, 4)
    ),
    "lower is a named numeric vector, but upper"
  )

  expect_error(
    estimate_model(a_model,
                       lower = c(1, 2, 3), upper = c(a = 5, 3, 4)
    ),
    "upper is a named numeric vector, but lower"
  )


  expect_error(
    estimate_model(a_model,
                   lower = c(muc = 1, b = 0.3, foo = 0.4),
                   upper = c(muc = 3, b = 5, non_dec = 0.4)
    ),
    "can not be adressed"
  )

  expect_error(
    estimate_model(a_model,
                       lower = c(muc = 1, b = 0.3, non_dec = 0.4),
                       upper = c(muc = 3, foo = 5, non_dec = 0.4)
    ),
    "can not be adressed"
  )


  expect_warning(
    estimate_model(a_model,
      lower = c(1, 0.2, 0.1),
      upper = c(7, 0.8, 0.8),
      use_nmkb = F, use_de_optim = F
    ),
    "No estimation done"
  )

  return_val <- suppressWarnings(estimate_model(a_model,
    lower = c(1, 0.2, 0.1),
    upper = c(7, 0.8, 0.8),
    use_nmkb = F, use_de_optim = F
  ))
  expect_identical(return_val, a_model)

  expect_error(estimate_model(a_model,
    lower = c(1, 0.2, 0.1),
    upper = c(7, 0.8, 0.6), seed = "a"
  ), "seed")
  expect_error(
    estimate_model(a_model,
      lower = c(1, 0.2, 0.1),
      upper = c(7, 0.8, 0.6), de_n_cores = "a"
    ),
    "de_n_cores"
  )

  expect_error(
    estimate_model(a_model,
      lower = c(1, 0.2, 0.1),
      upper = c(7, 0.8, 0.6), de_control = "a"
    ),
    "de_control"
  )
  expect_error(
    estimate_model(a_model,
      lower = c(1, 0.2, 0.1),
      upper = c(7, 0.8, 0.6), nmkb_control = "a"
    ),
    "nmkb_control"
  )
})

test_that("snapshot of the model running through nmkb", {
  a_model <- ratcliff_dm(dt = .005, dx = .05)

  new_data <- simulate_data(a_model, n = 3000, seed = 1)
  a_model <- set_obs_data(a_model, new_data)
  a_model <- set_model_prms(a_model, c(muc = 2, b = 0.5, non_dec = 0.2))
  expect_snapshot(
    estimate_model(a_model,
      lower = c(1, 0.2, 0.1), upper = c(7, 0.8, 0.6),
      use_de_optim = F, use_nmkb = T, verbose = 2
    )
  )
})



test_that("behavior of DE and nmkb toggles", {
  a_model <- dmc_dm(t_max = 1, dt = 0.01, dx = 0.1)
  subject <- simulate_data(drift_dm_obj = a_model, n = 300, seed = 1)
  a_model <- set_obs_data(a_model, subject)
  a_model <- set_free_prms(a_model, c("muc", "non_dec"))

  # both toggles FALSE
  expect_warning(
    estimate_model(
      drift_dm_obj = a_model,
      lower = c(1, 0.1),
      upper = c(5, 0.5), seed = 1,
      use_de_optim = FALSE
    ), "No estimation done"
  )

  # DE TRUE nmkb FALSE
  expect_snapshot(
    estimate_model(
      drift_dm_obj = a_model,
      lower = c(1, 0.1),
      upper = c(5, 0.5),
      seed = 1,
      verbose = 1,
      use_de_optim = TRUE,
      de_control = list(
        reltol = 1e-9, steptol = 50,
        itermax = 200, trace = TRUE
      )
    )
  )

  # DE FALSE and nmkb TRUE -> see above

  # nmkb TRUE but univariate case
  a_model <- set_free_prms(a_model, c("muc"))
  expect_warning(
    estimate_model(
      drift_dm_obj = a_model,
      lower = c(1),
      upper = c(5), seed = 1,
      use_nmkb = TRUE, use_de_optim = FALSE,
      verbose = 0
    ),
    "univariate optimization"
  )

  # skip on cran: multiple cores use
  # DE TRUE nmkb FALSE
  skip_on_cran()
  a_model <- set_free_prms(a_model, c("muc", "non_dec"))
  expect_snapshot(
    estimate_model(
      drift_dm_obj = a_model,
      lower = c(1, 0.1),
      upper = c(5, 0.5),
      seed = 1,
      verbose = 1,
      use_de_optim = TRUE,
      de_control = list(
        reltol = 1e-9, steptol = 50,
        itermax = 200, trace = TRUE
      ),
      de_n_cores = 2
    )
  )
})
