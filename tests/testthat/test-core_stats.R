test_that("calc_caf works as expected", {
  ### OBS
  params <- c("a" = 1)
  conds <- c("null", "foo")
  dummy_model <- drift_dm(params, conds)

  # create sample data
  rts_1 <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6) / 10
  errs_1 <- c(1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0)
  exp_1 <- c(0, 1, 0.5, 0, 1, 1)
  rts_2 <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6) / 10 + 0.2
  errs_2 <- c(0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0)
  exp_2 <- c(0.5, 0, 0.5, 0.5, 0.5, 1)


  dat <- data.frame(
    RT = c(rts_1, rts_2),
    Error = c(errs_1, errs_2),
    Cond = rep(c("null", "foo"), each = 12)
  )

  # add to model
  dummy_model <- set_obs_data(
    drift_dm_obj = dummy_model,
    obs_data = dat
  )

  # test
  cafs <- calc_stats(
    drift_dm_obj = dummy_model, type = "cafs",
    source = "obs", n_bins = 6
  )
  expect_identical(cafs$P_Corr[cafs$Cond == "null"], exp_1)
  expect_identical(cafs$P_Corr[cafs$Cond == "foo"], exp_2)


  # input checks
  expect_error(
    calc_stats(
      calc_stats(drift_dm_obj = dummy_model, type = "cafs", n_bins = 1),
      "larger than 1"
    )
  )
  expect_error(
    calc_stats(drift_dm_obj = dummy_model, type = "cafs", n_bins = NA),
    "single numeric"
  )
  expect_error(
    calc_stats(drift_dm_obj = dummy_model, type = "cafs", source = "foo"),
    "should be one of"
  )


  ### PRED
  # get some pdfs/cafs
  a_model <- ratcliff_dm(dx = .005, dt = .005)
  pdfs <- re_evaluate_model(a_model)$pdfs[["null"]]
  pred_cafs <- calc_stats(drift_dm_obj = a_model, type = "cafs", source = "pred")

  # calculate cafs by hand
  pdfs_u <- pdfs[[1]]
  pdfs_l <- pdfs[[2]]
  caf_value_const <- sum(pdfs_u) / (sum(pdfs_l) + sum(pdfs_u))
  caf <- rep(caf_value_const, 5)
  expect_true(all(abs(caf - pred_cafs$P_Corr) < .001))

  # another example with non-constant cafs
  a_model <- dmc_dm(dt = 0.001, dx = 0.005, t_max = 1)
  pdfs_comp <- re_evaluate_model(a_model)$pdfs[["comp"]]
  pdfs_incomp <- re_evaluate_model(a_model)$pdfs[["incomp"]]
  pred_cafs <- calc_stats(
    drift_dm_obj = a_model,
    type = "cafs", source = "pred"
  )

  # reference obtained by my former package
  expect_true(all(
    abs(pred_cafs$P_Corr[pred_cafs$Cond == "comp"] -
      c(0.9825608, 0.9824196, 0.9804658, 0.9837679, 0.9892333)) < 0.01
  ))
  expect_true(all(
    abs(pred_cafs$P_Corr[pred_cafs$Cond == "incomp"] -
      c(0.8391671, 0.9725687, 0.9871877, 0.9908459, 0.9918593)) < 0.01
  ))

  ### Both
  dat$Cond <- rep(c("comp", "incomp"), each = 12)
  a_model <- set_obs_data(a_model, dat)
  caf_final <- calc_stats(a_model, type = "cafs", source = "both", n_bins = 6)
  pred_cafs <- calc_stats(
    drift_dm_obj = a_model, type = "cafs",
    source = "pred", n_bins = 6
  )

  expect_true(nrow(caf_final) == 6 * 4)
  expect_identical(
    caf_final$P_Corr[caf_final$Cond == "comp" & caf_final$Source == "obs"], exp_1
  )
  expect_identical(
    caf_final$P_Corr[caf_final$Cond == "incomp" & caf_final$Source == "obs"], exp_2
  )
  expect_identical(
    caf_final$P_Corr[caf_final$Source == "pred"], pred_cafs$P_Corr
  )
})

test_that("calc_quantiles works as expected", {
  dummy_model <- dmc_dm(dx = .01, t_max = 1.5)


  ## OBS
  withr::local_seed(1)
  rts_1 <- rnorm(100, mean = 0.5, sd = 0.02)
  rts_2 <- rnorm(100, mean = 0.9, sd = 0.03)
  errs_1 <- sample(c(1, 0), 100, replace = T, prob = c(0.2, 0.8))
  errs_2 <- sample(c(1, 0), 100, replace = T, prob = c(0.2, 0.8))

  dat <- data.frame(
    RT = c(rts_1, rts_2),
    Error = c(errs_1, errs_2),
    Cond = rep(c("comp", "incomp"), each = 100)
  )

  dummy_model <- set_obs_data(drift_dm_obj = dummy_model, obs_data = dat)
  quants_obs <- calc_stats(
    drift_dm_obj = dummy_model, source = "obs", type = "quantiles",
    probs = seq(0.2, 0.8, 0.1)
  )
  expect_identical(
    quants_obs$Quant_Corr[quants_obs$Cond == "comp"],
    unname(quantile(dat$RT[dat$Cond == "comp" & dat$Error == 0],
      probs = seq(0.2, 0.8, 0.1)
    ))
  )

  expect_identical(
    quants_obs$Quant_Err[quants_obs$Cond == "comp"],
    unname(quantile(dat$RT[dat$Cond == "comp" & dat$Error == 1],
      probs = seq(0.2, 0.8, 0.1)
    ))
  )

  expect_identical(
    quants_obs$Quant_Corr[quants_obs$Cond == "incomp"],
    unname(quantile(dat$RT[dat$Cond == "incomp" & dat$Error == 0],
      probs = seq(0.2, 0.8, 0.1)
    ))
  )

  expect_identical(
    quants_obs$Quant_Err[quants_obs$Cond == "incomp"],
    unname(quantile(dat$RT[dat$Cond == "incomp" & dat$Error == 1],
      probs = seq(0.2, 0.8, 0.1)
    ))
  )


  ## preds
  quants_pred <- calc_stats(
    dummy_model,
    source = "pred", type = "quantiles",
    probs = seq(0.2, 0.8, 0.1)
  )


  expect_true(all(
    abs(quants_pred$Quant_Corr[quants_pred$Cond == "comp"] -
      c(0.346, 0.365, 0.385, 0.408, 0.435, 0.467, 0.510)) < 0.001
  )) # values derived by former package

  expect_true(all(
    abs(quants_pred$Quant_Err[quants_pred$Cond == "comp"] -
      c(0.343, 0.362, 0.380, 0.398, 0.418, 0.443, 0.475)) < 0.001
  ))

  expect_true(all(
    abs(quants_pred$Quant_Corr[quants_pred$Cond == "incomp"] -
      c(0.376, 0.395, 0.413, 0.432, 0.454, 0.480, 0.516)) < 0.001
  ))

  expect_true(all(
    abs(quants_pred$Quant_Err[quants_pred$Cond == "incomp"] -
      c(0.314, 0.323, 0.332, 0.341, 0.352, 0.365, 0.386)) < 0.001
  ))

  # both
  quants_both <- calc_stats(
    dummy_model,
    source = "both", type = "quantiles",
    probs = seq(0.2, 0.8, 0.1)
  )
  both <- rbind(quants_obs, quants_pred)
  colnames(both)[1] <- "Source"
  rownames(both) <- 1:nrow(both)
  expect_identical(quants_both, both)

  # input checks
  expect_error(
    calc_stats(dummy_model, type = "quantiles", source = "foo"),
    "should be one of"
  )
  expect_error(
    calc_stats(dummy_model, type = "quantiles", probs = NA),
    "numeric vector"
  )
  expect_error(
    calc_stats(dummy_model, type = "quantiles", probs = numeric()),
    "numeric vector"
  )
  expect_error(
    calc_stats(dummy_model, type = "quantiles", probs = c(0, 0.1)),
    "must be in the range"
  )
  expect_error(
    calc_stats(dummy_model, type = "quantiles", probs = c(0.1, 1)),
    "must be in the range"
  )
})



test_that("calc_delta_fun works as expected", {
  a_model = dmc_dm(dt = .005, dx = .005)
  a_model$solver = "kfe"
  a_model$conds = c("comp", "incomp", "neutral")

  a_model$comp_funs$mu_fun = function(prms_model, prms_solve, t_vec, one_cond,
                                      ddm_opts) {

    # unpack values and conduct checks
    muc <- prms_model[["muc"]]
    tau <- prms_model[["tau"]]
    A <- prms_model[["A"]]

    mua <- A / tau * exp(1 - t_vec / tau) * (1 - t_vec / tau)

    # get drift rate, depending on the condition
    if (one_cond == "comp") {
      return(muc + mua)
    }
    if (one_cond == "incomp") {
      return(muc - mua)
    }
    if (one_cond == "neutral") {
      return(muc + mua * 0)
    }
  }

  a_model$comp_funs$mu_int_fun = function(prms_model, prms_solve, t_vec, one_cond,
                                      ddm_opts) {
    return(t_vec)
  }

  data = simulate_data(a_model, 1000)
  a_model = set_obs_data(a_model, data, eval_model = T)
  delta_dat = calc_stats(drift_dm_obj = a_model, type = "delta_fun",
             minuend = "incomp", subtrahend = "comp")
  expect_equal(delta_dat$Delta_incomp_comp,
               delta_dat$Quant_Corr_incomp - delta_dat$Quant_Corr_comp)
  expect_equal(delta_dat$Avg_incomp_comp,
               0.5*delta_dat$Quant_Corr_incomp + 0.5*delta_dat$Quant_Corr_comp)

  # incomp vs comp Corr
  delta_dat = calc_stats(drift_dm_obj = a_model, type = "delta_fun",
                         minuend = "incomp", subtrahend = "comp")
  expect_equal(delta_dat$Delta_incomp_comp,
               delta_dat$Quant_Corr_incomp - delta_dat$Quant_Corr_comp)
  expect_equal(delta_dat$Avg_incomp_comp,
               0.5*delta_dat$Quant_Corr_incomp + 0.5*delta_dat$Quant_Corr_comp)


  # incomp vs comp Corr and Err
  delta_dat = calc_stats(drift_dm_obj = a_model, type = "delta_fun",
                         minuend = "incomp", subtrahend = "comp",
                         dv = c("Quant_Corr", "Quant_Err"))
  expect_equal(delta_dat$Delta_Corr_incomp_comp,
               delta_dat$Quant_Corr_incomp - delta_dat$Quant_Corr_comp)
  expect_equal(delta_dat$Avg_Corr_incomp_comp,
               0.5*delta_dat$Quant_Corr_incomp + 0.5*delta_dat$Quant_Corr_comp)

  expect_equal(delta_dat$Delta_Err_incomp_comp,
               delta_dat$Quant_Err_incomp - delta_dat$Quant_Err_comp)
  expect_equal(delta_dat$Avg_Err_incomp_comp,
               0.5*delta_dat$Quant_Err_incomp + 0.5*delta_dat$Quant_Err_comp)


  # incomp vs neutral Corr and neutral vs. comp  Err
  delta_dat = calc_stats(drift_dm_obj = a_model, type = "delta_fun",
                         minuend = c("incomp", "neutral"),
                         subtrahend = c("comp", "comp"),
                         dv = c("Quant_Corr", "Quant_Err"))
  expect_equal(delta_dat$Delta_Corr_incomp_comp,
               delta_dat$Quant_Corr_incomp - delta_dat$Quant_Corr_comp)
  expect_equal(delta_dat$Avg_Corr_incomp_comp,
               0.5*delta_dat$Quant_Corr_incomp + 0.5*delta_dat$Quant_Corr_comp)

  expect_equal(delta_dat$Delta_Err_neutral_comp,
               delta_dat$Quant_Err_neutral - delta_dat$Quant_Err_comp)
  expect_equal(delta_dat$Avg_Err_neutral_comp,
               0.5*delta_dat$Quant_Err_neutral + 0.5*delta_dat$Quant_Err_comp)


  # compare with quantiles
  quantiles = calc_stats(drift_dm_obj = a_model, type = "quantiles")
  expect_equal(quantiles$Quant_Corr[quantiles$Cond == "comp"],
               delta_dat$Quant_Corr_comp)
  expect_equal(quantiles$Quant_Corr[quantiles$Cond == "neutral"],
               delta_dat$Quant_Corr_neutral)


  # input checks
  expect_error(
    calc_stats(drift_dm_obj = a_model, type = "delta_fun",
               minuend = c("incomp", "neutral", "foo"),
               subtrahend = c("comp", "comp"),
               dv = c("Quant_Corr", "Quant_Err")), "length of minuend and subtrahend"
  )

  expect_error(
    calc_stats(drift_dm_obj = a_model, type = "delta_fun",
               minuend = c("incomp", "neutral"),
               subtrahend = c("comp", "comp", "foo"),
               dv = c("Quant_Corr", "Quant_Err")), "length of minuend and subtrahend"
  )

  expect_error(
    calc_stats(drift_dm_obj = "foo", type = "delta_fun",
               minuend = c("incomp", "neutral"),
               subtrahend = c("comp", "comp"),
               dv = c("Quant_Corr", "Quant_Err")), "drift_dm"
  )

  expect_error(
    calc_stats(drift_dm_obj = a_model, type = "delta_fun",
               minuend = c("incomp", "neutral", "bla"),
               subtrahend = c("comp", "comp", "uff"),
               dv = c("Quant_Corr", "Quant_Err")), "Conds specified in minuend"
  )

  expect_error(
    calc_stats(drift_dm_obj = a_model, type = "delta_fun",
               minuend = c("incomp", "neutral"),
               subtrahend = c("comp", "uff"),
               dv = c("Quant_Corr", "Quant_Err")), "Conds specified in subtrahend"
  )

  expect_error(
    calc_stats(drift_dm_obj = a_model, type = "delta_fun",
               minuend = character(),
               subtrahend = c("comp", "comp", "comp"),
               dv = c("Quant_Corr", "Quant_Err")), "minuend"
  )

  expect_error(
    calc_stats(drift_dm_obj = a_model, type = "delta_fun",
               minuend = c("incomp"),
               subtrahend = character(),
               dv = c("Quant_Corr", "Quant_Err")), "subtrahend"
  )
})

test_that("no data warnings", {
  # no data warning
  a_model <- ratcliff_dm()
  expect_warning(calc_stats(a_model, type = "quantiles", source = "obs"), "no data")
  expect_warning(calc_stats(a_model, type = "cafs", source = "obs"), "no data")
})
