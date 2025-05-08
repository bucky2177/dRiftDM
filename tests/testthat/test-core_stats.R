
# Basic Stats -------------------------------------------------------------

test_that("calc_basic_stats_obs works as expected", {

  rts_u = c(1,2,2,3,3,4)
  rts_l = c(1,2,6, 3)

  returned_dat = calc_basic_stats_obs(rts_u, rts_l, one_cond = "foo")

  # expectation
  exp = data.frame(Cond = "foo", Mean_U = mean(rts_u), Mean_L = mean(rts_l),
                   SD_U = sd(rts_u), SD_L = sd(rts_l), P_U = 0.6)
  expect_identical(exp, returned_dat)

})


test_that("calc_basic_stats_pred works as expected", {

  dt = .005
  t_vec = seq(0, 2, dt)

  pdf_u = dgamma(t_vec, shape = 2, scale = .125) * 0.65
  pdf_l = dnorm(t_vec, mean = 0.3, sd = 0.05) * 0.35


  returned_dat = calc_basic_stats_pred(pdf_u = pdf_u, pdf_l = pdf_l,
                                       one_cond = "bar", t_vec = t_vec,
                                       dt = dt)

  # expectation
  exp = data.frame(Cond = "bar", Mean_U = 0.25, Mean_L = 0.3,
                   SD_U = 0.17678, SD_L = 0.05, P_U = 0.65)
  expect_s3_class(returned_dat, "data.frame")
  expect_equal(exp$Cond, returned_dat$Cond)
  expect_true(abs(exp$Mean_U - returned_dat$Mean_U) < .0001)
  expect_equal(exp$Mean_L, returned_dat$Mean_L)
  expect_true(abs(exp$SD_U - returned_dat$SD_U) < .0001)
  expect_true(abs(exp$SD_L - returned_dat$SD_L) < .0001)
  expect_true(abs(exp$P_U - returned_dat$P_U) < .0001)

})


test_that("calc_basic_stats -> works as expected", {

  # get a model to test
  model <- dmc_dm(dt = .005, dx = .005, obs_data = dmc_synth_data,
                  t_max = 2)

  model <- re_evaluate_model(model)


  # expectations based on the pdfs
  t_vec = seq(0, 2, 0.005)
  pdfs_pred = pdfs(model)$pdfs
  exps_pred_comp = calc_basic_stats_pred(pdf_u = pdfs_pred$comp$pdf_u,
                                         pdf_l = pdfs_pred$comp$pdf_l,
                                         one_cond = "comp", t_vec = t_vec,
                                         dt = .005, skip_if_contr_low = NULL)
  exps_pred_comp = cbind(Source = "pred", exps_pred_comp)

  exps_pred_incomp = calc_basic_stats_pred(pdf_u = pdfs_pred$incomp$pdf_u,
                                           pdf_l = pdfs_pred$incomp$pdf_l,
                                           one_cond = "incomp", t_vec = t_vec,
                                           dt = .005, skip_if_contr_low = NULL)
  exps_pred_incomp = cbind(Source = "pred", exps_pred_incomp)


  # expectations based on the data
  exps_obs_comp = calc_basic_stats_obs(rts_u = model$obs_data$rts_u$comp,
                                       rts_l = model$obs_data$rts_l$comp,
                                       one_cond = "comp")
  exps_obs_comp = cbind(Source = "obs", exps_obs_comp)
  exps_obs_incomp = calc_basic_stats_obs(rts_u = model$obs_data$rts_u$incomp,
                                         rts_l = model$obs_data$rts_l$incomp,
                                         one_cond = "incomp")
  exps_obs_incomp = cbind(Source = "obs", exps_obs_incomp)

  exps = rbind(exps_obs_comp, exps_obs_incomp, exps_pred_comp, exps_pred_incomp)


  # calculate via calc_stats.drift_dm
  basics <- calc_stats(model, type = "basic_stats")

  # test equivalence (workaround because row names are different)
  expect_identical(basics$Mean_corr, exps$Mean_U)
  expect_identical(basics$Mean_err, exps$Mean_L)
  expect_identical(basics$SD_corr, exps$SD_U)
  expect_identical(basics$SD_err, exps$SD_L)
  expect_identical(basics$P_corr, exps$P_U)
  expect_identical(basics$Cond, exps$Cond)
  expect_identical(basics$Source, exps$Source)


  ## validate object attributes/aspects
  expect_equal(class(basics), c("basic_stats", "sum_dist", "stats_dm",
                                "data.frame"))
  expect_equal(colnames(basics),
               c("Source", "Cond", "Mean_corr", "Mean_err", "SD_corr",
                 "SD_err", "P_corr"))
  expect_true(!is.null(attr(basics, "b_coding")))
})


test_that("basic_stats -> validate work as expected", {
  data_id <- dRiftDM::ulrich_flanker_data
  obs_basic_stats <- calc_stats(data_id, type = "basic_stats")

  test <- aggregate(cbind(Mean_corr, Mean_err, SD_corr, SD_err, P_corr) ~
                    Source + Cond, obs_basic_stats, mean,
                    na.action = na.pass, na.rm = T)

  # test what is returned
  basic_stats_agg <- calc_stats(data_id, type = "basic_stats", average = T)
  expect_equal(unpack_obj(basic_stats_agg), test)
  expect_equal(class(basic_stats_agg),
               c("basic_stats", "sum_dist", "stats_dm", "data.frame"))
  expect_equal(attr(basic_stats_agg, "b_coding"), drift_dm_default_b_coding())

  # input checks of validate
  temp <- basic_stats_agg
  attr(temp, "b_coding") <- NULL
  expect_error(validate_stats_dm(temp), "b_coding")

  temp <- basic_stats_agg
  colnames(temp)[1] <- "foo"
  expect_error(validate_stats_dm(temp), "Source")

  temp <- basic_stats_agg
  colnames(temp)[2] <- "foo"
  expect_error(validate_stats_dm(temp), "Cond")

  temp <- basic_stats_agg
  colnames(temp)[3] <- "foo"
  expect_error(validate_stats_dm(temp), "Mean_")

  temp <- basic_stats_agg
  colnames(temp)[4] <- "foo"
  expect_error(validate_stats_dm(temp), "Mean_")

  temp <- basic_stats_agg
  colnames(temp)[5] <- "foo"
  expect_error(validate_stats_dm(temp), "SD_")

  temp <- basic_stats_agg
  colnames(temp)[6] <- "foo"
  expect_error(validate_stats_dm(temp), "SD_")

  temp <- basic_stats_agg
  colnames(temp)[7] <- "foo"
  expect_error(validate_stats_dm(temp), "P_")

  temp <- basic_stats_agg
  temp$P_foo <- temp$P_corr
  expect_error(validate_stats_dm(temp), "P_")
})



test_that("basic_stats -> input checks", {
  b_coding <- drift_dm_default_b_coding()

  # input checks
  expect_error(
    calc_cafs(
      pdf_u = c(0, 1, 0), pdf_l = NULL, rts_u = NULL, rts_l = NULL,
      one_cond = "foo", b_coding = b_coding
    ),
    "both NULL or not"
  )

  expect_error(
    calc_cafs(
      pdf_u = NULL, pdf_l = NULL, rts_u = c(0, 1, 0), rts_l = NULL,
      one_cond = "foo", b_coding = b_coding
    ),
    "both NULL or not"
  )
})


# CAFS --------------------------------------------------------------------

test_that("calc_caf -> observed works as expected", {

  params <- c(a = 1)
  conds <- c("null", "foo")
  dummy_model <- drift_dm(params, conds, subclass = "test")

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
  obs_data(dummy_model) <- dat

  # cafs via calc_stats.drift_dm ####
  cafs <- calc_stats(dummy_model, type = "cafs", n_bins = 6)
  cafs <- cafs[cafs$Source == "obs", ]
  expect_identical(cafs$P_corr[cafs$Cond == "null"], exp_1)
  expect_identical(cafs$P_corr[cafs$Cond == "foo"], exp_2)

  # cafs via calc_stats.data.frame ####
  cafs <- calc_stats(dat, type = "cafs", n_bins = 6)
  expect_identical(cafs$P_corr[cafs$Cond == "null"], exp_1)
  expect_identical(cafs$P_corr[cafs$Cond == "foo"], exp_2)


  ### try with different b_coding for calc_stats.data.frame
  dat <- data.frame(
    RT = c(rts_1, rts_2),
    Response = c(errs_1, errs_2),
    Cond = rep(c("null", "foo"), each = 12)
  )
  dat$Response <- ifelse(dat$Response == 0, 1, -1)
  b_coding <- list(
    column = "Response",
    u_name_value = c("left" = 1),
    l_name_value = c("right" = -1)
  )
  cafs <- calc_stats(dat, type = "cafs", n_bins = 6, b_coding = b_coding)
  expect_identical(cafs$P_left[cafs$Cond == "null"], exp_1)

  # just one cond
  cafs <- calc_stats(dat,
    type = "cafs", n_bins = 6, b_coding = b_coding,
    conds = "null"
  )
  expect_identical(cafs$P_left, exp_1)


  ## validate object attributes/aspects
  expect_equal(class(cafs), c("cafs", "sum_dist", "stats_dm", "data.frame"))
  expect_equal(colnames(cafs), c("Source", "Cond", "Bin", "P_left"))
  expect_true(!is.null(attr(cafs, "b_coding")))
})

test_that("cafs -> validate and aggregate work as expected", {
  data_id <- dRiftDM::ulrich_flanker_data
  obs_cafs <- calc_stats(data_id, type = "cafs")

  test <- aggregate(P_corr ~ Bin + Cond + Source, obs_cafs, mean)
  test <- test[c("Source", "Cond", "Bin", "P_corr")]

  # test what is returned
  obs_cafs <- calc_stats(data_id, type = "cafs", average = T)
  expect_equal(obs_cafs$P_corr, test$P_corr)
  expect_equal(class(obs_cafs), c("cafs", "sum_dist", "stats_dm", "data.frame"))
  expect_equal(attr(obs_cafs, "b_coding"), drift_dm_default_b_coding())

  # input checks of validate
  temp <- obs_cafs
  attr(temp, "b_coding") <- NULL
  expect_error(validate_stats_dm(temp), "b_coding")

  temp <- obs_cafs
  colnames(temp)[1] <- "foo"
  expect_error(validate_stats_dm(temp), "Source")

  temp <- obs_cafs
  colnames(temp)[2] <- "foo"
  expect_error(validate_stats_dm(temp), "Cond")

  temp <- obs_cafs
  colnames(temp)[3] <- "foo"
  expect_error(validate_stats_dm(temp), "Bin")

  temp <- obs_cafs
  colnames(temp)[4] <- "foo"
  expect_error(validate_stats_dm(temp), "P_")

  temp <- obs_cafs
  temp$P_foo <- temp$P_corr
  expect_error(validate_stats_dm(temp), "P_")
})

test_that("calc_cafs -> input checks", {
  b_coding <- drift_dm_default_b_coding()

  # input checks
  expect_error(
    calc_cafs(
      pdf_u = NULL, pdf_l = NULL, rts_u = NULL, rts_l = NULL,
      one_cond = "foo", n_bins = 0, b_coding = b_coding
    ),
    "larger than 1"
  )
  expect_error(
    calc_cafs(
      pdf_u = NULL, pdf_l = NULL, rts_u = NULL, rts_l = NULL,
      one_cond = "foo", n_bins = NA, b_coding = b_coding
    ),
    "a valid numeric"
  )
  expect_error(
    calc_cafs(
      pdf_u = c(0, 1, 0), pdf_l = NULL, rts_u = NULL, rts_l = NULL,
      one_cond = "foo", b_coding = b_coding
    ),
    "both NULL or not"
  )

  expect_error(
    calc_cafs(
      pdf_u = NULL, pdf_l = NULL, rts_u = c(0, 1, 0), rts_l = NULL,
      one_cond = "foo", b_coding = b_coding
    ),
    "both NULL or not"
  )
})

test_that("calc_cafs -> predicted works as expected", {
  a_model <- dmc_dm(dt = 0.001, dx = 0.005, t_max = 1)
  pdfs_comp <- re_evaluate_model(a_model)$pdfs[["comp"]]
  pdfs_incomp <- re_evaluate_model(a_model)$pdfs[["incomp"]]
  pred_cafs <- calc_stats(a_model, type = "cafs")

  # reference obtained by my former package
  expect_true(all(
    abs(pred_cafs$P_corr[pred_cafs$Cond == "comp"] -
      c(0.9825608, 0.9824196, 0.9804658, 0.9837679, 0.9892333)) < 0.01
  ))
  expect_true(all(
    abs(pred_cafs$P_corr[pred_cafs$Cond == "incomp"] -
      c(0.8391671, 0.9725687, 0.9871877, 0.9908459, 0.9918593)) < 0.01
  ))
})


# QUANTILES ---------------------------------------------------------------


test_that("calc_quantiles -> observed and predicted works as expected", {
  # get a dummy model
  dummy_model <- dmc_dm()

  # and some data
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

  obs_data(dummy_model) <- dat
  quants <- calc_stats(dummy_model,
    type = "quantiles",
    probs = seq(0.2, 0.8, 0.1)
  )
  quants_obs <- quants[quants$Source == "obs", ]

  expect_identical(
    quants_obs$Quant_corr[quants_obs$Cond == "comp"],
    unname(quantile(dat$RT[dat$Cond == "comp" & dat$Error == 0],
      probs = seq(0.2, 0.8, 0.1)
    ))
  )

  expect_identical(
    quants_obs$Quant_err[quants_obs$Cond == "comp"],
    unname(quantile(dat$RT[dat$Cond == "comp" & dat$Error == 1],
      probs = seq(0.2, 0.8, 0.1)
    ))
  )

  expect_identical(
    quants_obs$Quant_corr[quants_obs$Cond == "incomp"],
    unname(quantile(dat$RT[dat$Cond == "incomp" & dat$Error == 0],
      probs = seq(0.2, 0.8, 0.1)
    ))
  )

  expect_identical(
    quants_obs$Quant_err[quants_obs$Cond == "incomp"],
    unname(quantile(dat$RT[dat$Cond == "incomp" & dat$Error == 1],
      probs = seq(0.2, 0.8, 0.1)
    ))
  )


  ## preds
  quants_pred <- quants[quants$Source == "pred", ]


  expect_true(all(
    abs(quants_pred$Quant_corr[quants_pred$Cond == "comp"] -
      c(0.346, 0.365, 0.385, 0.408, 0.435, 0.467, 0.510)) < 0.002
  )) # values derived by former package

  expect_true(all(
    abs(quants_pred$Quant_err[quants_pred$Cond == "comp"] -
      c(0.343, 0.362, 0.380, 0.398, 0.418, 0.443, 0.475)) < 0.002
  ))

  expect_true(all(
    abs(quants_pred$Quant_corr[quants_pred$Cond == "incomp"] -
      c(0.376, 0.395, 0.413, 0.432, 0.454, 0.480, 0.516)) < 0.002
  ))

  expect_true(all(
    abs(quants_pred$Quant_err[quants_pred$Cond == "incomp"] -
      c(0.314, 0.323, 0.332, 0.341, 0.352, 0.365, 0.386)) < 0.002
  ))


  ### try with different b_coding and via calc_stats.data_frame
  colnames(dat)[2] <- "Foo"
  dat$Foo <- ifelse(dat$Foo == 0, 2, -1)
  quants_obs_new_b <- calc_stats(dat,
    type = "quantiles",
    probs = seq(0.2, 0.8, 0.1),
    conds = "incomp",
    b_coding = list(
      column = "Foo",
      u_name_value = c("foo" = 2),
      l_name_value = c("bar" = -1)
    )
  )
  expect_equal(
    colnames(quants_obs_new_b),
    c("Source", "Cond", "Prob", "Quant_foo", "Quant_bar")
  )
  expect_equal(
    quants_obs_new_b$Quant_foo,
    quants_obs$Quant_corr[quants_obs$Cond == "incomp"]
  )
  expect_equal(
    quants_obs_new_b$Quant_bar,
    quants_obs$Quant_err[quants_obs$Cond == "incomp"]
  )
})

test_that("quantiles -> validate and aggregate work as expected", {
  data_id <- dRiftDM::ulrich_flanker_data
  obs_quants <- calc_stats(data_id, type = "quantiles")



  test <- aggregate(cbind(Quant_corr, Quant_err) ~ Prob + Cond + Source,
    obs_quants, mean,
    na.rm = T, na.action = na.pass
  )
  test <- test[c("Source", "Cond", "Prob", "Quant_corr", "Quant_err")]

  # test what is returned
  obs_quants <- calc_stats(data_id, type = "quantiles", average = T)
  expect_equal(obs_quants$Quant_corr, test$Quant_corr)
  expect_equal(obs_quants$Quant_err, test$Quant_err)

  expect_equal(
    class(obs_quants),
    c("quantiles", "sum_dist", "stats_dm", "data.frame")
  )
  expect_equal(attr(obs_quants, "b_coding"), drift_dm_default_b_coding())

  # input checks of validate
  temp <- obs_quants
  attr(temp, "b_coding") <- NULL
  expect_error(validate_stats_dm(temp), "b_coding")

  temp <- obs_quants
  colnames(temp)[1] <- "foo"
  expect_error(validate_stats_dm(temp), "Source")

  temp <- obs_quants
  colnames(temp)[2] <- "foo"
  expect_error(validate_stats_dm(temp), "Cond")

  temp <- obs_quants
  colnames(temp)[3] <- "foo"
  expect_error(validate_stats_dm(temp), "Prob")

  temp <- obs_quants
  colnames(temp)[4] <- "foo"
  expect_error(validate_stats_dm(temp), "Quant_")

  temp <- obs_quants
  temp$Quant_foo <- temp$Quant_corr
  expect_error(validate_stats_dm(temp), "Quant_")
})

test_that("calc_quantiles -> input checks", {
  b_coding <- drift_dm_default_b_coding()

  t <- seq(0, 1, 0.1)

  # input checks
  expect_error(
    calc_quantiles(
      pdf_u = NULL, pdf_l = NULL, t_vec = t,
      rts_u = NULL, rts_l = NULL, one_cond = "foo",
      probs = 0, b_coding = b_coding
    ),
    "length > 1"
  )
  expect_error(
    calc_quantiles(
      pdf_u = NULL, pdf_l = NULL, t_vec = t,
      rts_u = NULL, rts_l = NULL, one_cond = "foo",
      probs = 0, b_coding = b_coding
    ),
    "a valid numeric"
  )
  expect_error(
    calc_quantiles(
      pdf_u = c(0, 1, 2), pdf_l = NULL, t_vec = t,
      rts_u = NULL, rts_l = NULL, one_cond = "foo",
      b_coding = b_coding
    ),
    "both NULL or not"
  )

  expect_error(
    calc_quantiles(
      pdf_u = NULL, pdf_l = NULL, t_vec = t,
      rts_u = NULL, rts_l = c(0, 1, 2), one_cond = "foo",
      b_coding = b_coding
    ),
    "both NULL or not"
  )
})


# DELTA FUNS --------------------------------------------------------------

test_that("calc_delta_funs -> observed and predicted works as expected", {
  a_model <- dmc_dm(dt = .005, dx = .005)

  prms <- coef(a_model, select_unique = F)[1, ]
  new_flex_prms <- flex_prms(
    object = prms,
    conds = c("comp", "neutral", "incomp"),
    instr = "
                            A ~ incomp == -(A ~ comp)
                            A <!> neutral
                            A ~ neutral => 0
                            "
  )

  flex_prms(a_model) <- new_flex_prms

  data <- simulate_data(a_model, n = 1000)
  obs_data(a_model) <- data

  ### derive statistics via calc_stats.drift_dm
  delta_dat <- calc_stats(
    a_model,
    type = "delta_fun", minuends = "incomp", subtrahends = "comp"
  )
  expect_equal(
    delta_dat$Delta_incomp_comp,
    delta_dat$Quant_corr_incomp - delta_dat$Quant_corr_comp
  )
  expect_equal(
    delta_dat$Avg_incomp_comp,
    0.5 * delta_dat$Quant_corr_incomp + 0.5 * delta_dat$Quant_corr_comp
  )

  # incomp vs comp Corr and Err
  delta_dat <- calc_stats(
    a_model,
    type = "delta_fun", minuends = "incomp", subtrahends = "comp",
    dvs = c("Quant_corr", "Quant_err")
  )
  expect_equal(
    delta_dat$Delta_corr_incomp_comp,
    delta_dat$Quant_corr_incomp - delta_dat$Quant_corr_comp
  )
  expect_equal(
    delta_dat$Avg_corr_incomp_comp,
    0.5 * delta_dat$Quant_corr_incomp + 0.5 * delta_dat$Quant_corr_comp
  )

  expect_equal(
    delta_dat$Delta_err_incomp_comp,
    delta_dat$Quant_err_incomp - delta_dat$Quant_err_comp
  )
  expect_equal(
    delta_dat$Avg_err_incomp_comp,
    0.5 * delta_dat$Quant_err_incomp + 0.5 * delta_dat$Quant_err_comp
  )


  # incomp vs neutral Corr and neutral vs. comp  Err
  delta_dat <- calc_stats(
    a_model,
    type = "delta_fun",
    minuends = c("incomp", "neutral"),
    subtrahends = c("comp", "comp"),
    dvs = c("Quant_corr", "Quant_err")
  )
  expect_equal(
    delta_dat$Delta_corr_incomp_comp,
    delta_dat$Quant_corr_incomp - delta_dat$Quant_corr_comp
  )
  expect_equal(
    delta_dat$Avg_corr_incomp_comp,
    0.5 * delta_dat$Quant_corr_incomp + 0.5 * delta_dat$Quant_corr_comp
  )

  expect_equal(
    delta_dat$Delta_err_neutral_comp,
    delta_dat$Quant_err_neutral - delta_dat$Quant_err_comp
  )
  expect_equal(
    delta_dat$Avg_err_neutral_comp,
    0.5 * delta_dat$Quant_err_neutral + 0.5 * delta_dat$Quant_err_comp
  )


  # compare with quantiles
  quantiles <- calc_stats(a_model, type = "quantiles")
  expect_equal(
    quantiles$Quant_corr[quantiles$Cond == "comp"],
    delta_dat$Quant_corr_comp
  )
  expect_equal(
    quantiles$Quant_corr[quantiles$Cond == "neutral"],
    delta_dat$Quant_corr_neutral
  )


  ### derive statistics via calc_stats.obs_data
  delta_dat <- calc_stats(
    a_model,
    type = "delta_fun", minuends = "incomp", subtrahends = "comp"
  )
  delta_dat <- delta_dat[delta_dat$Source == "obs", ]
  obs_delta_dat <- calc_stats(
    data,
    type = "delta_fun", minuends = "incomp", subtrahends = "comp"
  )
  expect_equal(delta_dat, obs_delta_dat)
})

test_that("delta_funs -> validate and aggregate work as expected", {
  data_id <- dRiftDM::ulrich_flanker_data
  obs_delta <- calc_stats(data_id,
    type = "delta_funs", minuends = "incomp",
    subtrahends = "comp"
  )


  test <- aggregate(cbind(Quant_corr_comp, Delta_incomp_comp) ~ Prob + Source,
    obs_delta, mean,
    na.rm = T, na.action = na.pass
  )
  test <- test[c("Source", "Prob", "Quant_corr_comp", "Delta_incomp_comp")]

  # test what is returned
  avg_delta <- calc_stats(data_id,
    type = "delta_funs", minuends = "incomp",
    subtrahends = "comp", average = T
  )
  expect_equal(avg_delta$Delta_incomp_comp, test$Delta_incomp_comp)
  expect_equal(avg_delta$Quant_corr_comp, test$Quant_corr_comp)

  expect_equal(
    class(avg_delta),
    c("delta_funs", "sum_dist", "stats_dm", "data.frame")
  )
  expect_equal(attr(avg_delta, "b_coding"), drift_dm_default_b_coding())


  # test of one data
  delta_1 <- calc_stats(data_id[data_id$ID == 1, ],
    type = "delta_funs",
    minuends = "incomp", subtrahends = "comp"
  )
  obs_delta_1 <- obs_delta[obs_delta$ID == 1, ]
  expect_equal(delta_1, obs_delta_1)


  # input checks of validate
  temp <- obs_delta
  attr(temp, "b_coding") <- NULL
  expect_error(validate_stats_dm(temp), "b_coding")

  temp <- obs_delta
  colnames(temp)[2] <- "foo"
  expect_error(validate_stats_dm(temp), "Source")

  temp <- obs_delta
  colnames(temp)[3] <- "foo"
  expect_error(validate_stats_dm(temp), "Prob")

  temp <- obs_delta
  temp$Quant_corr_incomp <- NULL
  expect_error(validate_stats_dm(temp), "Quant_")

  temp <- obs_delta
  temp$Avg_incomp_comp <- NULL
  expect_error(validate_stats_dm(temp), "Avg_")

  temp <- obs_delta
  temp$Delta_incomp_comp <- NULL
  expect_error(validate_stats_dm(temp), "Delta_")
})

test_that("calc_delta_funs -> input checks", {
  some_data <- dRiftDM::dmc_synth_data
  quantiles <- calc_stats(some_data, "quantiles")
  b_coding <- drift_dm_default_b_coding()

  # input checks
  expect_error(
    calc_delta_funs(
      quantiles_dat = quantiles,
      minuends = c("incomp", "neutral", "foo"),
      subtrahends = c("comp", "comp"),
      dvs = c("Quant_corr", "Quant_err"), b_coding = b_coding
    ), "length of minuends and subtrahends"
  )

  expect_error(
    calc_delta_funs(
      quantiles_dat = quantiles,
      minuends = c("incomp", "neutral", "bla"),
      subtrahends = c("comp", "comp", "uff"),
      dvs = c("Quant_corr", "Quant_err"), b_coding = b_coding
    ), "Conds specified in minuends"
  )
  expect_error(
    calc_delta_funs(
      quantiles_dat = quantiles,
      minuends = c("incomp", "incomp"),
      subtrahends = c("comp", "uff"),
      dvs = c("Quant_corr", "Quant_err"), b_coding = b_coding
    ), "Conds specified in subtrahends"
  )

  expect_error(
    calc_delta_funs(
      quantiles_dat = quantiles,
      minuends = character(),
      subtrahends = c("comp", "uff"),
      dvs = c("Quant_corr", "Quant_err"), b_coding = b_coding
    ), "must be a character"
  )

  expect_error(
    calc_delta_funs(
      quantiles_dat = quantiles,
      minuends = c("incomp"),
      subtrahends = character(),
      dvs = c("Quant_corr", "Quant_err"), b_coding = b_coding
    ), "must be a character"
  )

  expect_error(
    calc_delta_funs(
      quantiles_dat = quantiles,
      minuends = c("incomp"),
      subtrahends = NULL,
      dvs = c("Quant_corr", "Quant_err"), b_coding = b_coding
    ), "subtrahends not provided"
  )

  expect_error(
    calc_delta_funs(
      quantiles_dat = quantiles,
      minuends = NULL,
      subtrahends = c("incomp"),
      dvs = c("Quant_corr", "Quant_err"), b_coding = b_coding
    ), "minuends not provided"
  )

  expect_error(
    calc_delta_funs(
      quantiles_dat = quantiles,
      minuends = "comp",
      subtrahends = "incomp",
      dvs = c("Quant_corr", "Quant_foo"), b_coding = b_coding
    )
  )

  temp <- quantiles
  colnames(temp)[5] <- "Quant_foo"
  expect_error(
    calc_delta_funs(
      quantiles_dat = temp,
      minuends = "comp",
      subtrahends = "incomp",
      dvs = c("Quant_corr"), b_coding = b_coding
    ), "unexpected column names"
  )
})


# FIT STATS ---------------------------------------------------------------


test_that("calc_fit_stats -> works as expected", {
  data <- data.frame(
    RT = c(0.431, 0.402, 0.344, 0.3322, 0.435),
    Error = c(0, 0, 1, 0, 1),
    Cond = c("null", "null", "null", "foo", "foo")
  )

  a_model <- drift_dm(c("a" = 4, "b" = 4),
    conds = c("null", "foo"), dx = 0.005,
    dt = 0.005, t_max = 1, subclass = "test"
  )
  obs_data(a_model) <- data
  a_model <- re_evaluate_model(a_model)
  pdfs_null <- a_model$pdfs$null
  pdfs_foo <- a_model$pdfs$foo

  # log_like_value calculated by hand
  t_vec <- seq(0, 1, 0.005)
  d_1 <- (pdfs_null$pdf_u[88] - pdfs_null$pdf_u[87]) * 0.2 + pdfs_null$pdf_u[87]
  d_2 <- (pdfs_null$pdf_u[82] - pdfs_null$pdf_u[81]) * 0.4 + pdfs_null$pdf_u[81]
  d_3 <- (pdfs_null$pdf_l[70] - pdfs_null$pdf_l[69]) * 0.8 + pdfs_null$pdf_l[69]
  d_4 <- (pdfs_foo$pdf_u[68] - pdfs_foo$pdf_u[67]) * 0.44 + pdfs_foo$pdf_u[67]
  d_5 <- pdfs_foo$pdf_l[88]
  for_test_log_like <- log(d_1) + log(d_2) + log(d_3) + log(d_4) + log(d_5)
  # calc_log_like
  expect_equal(a_model$log_like, for_test_log_like)

  # AIC, BIC
  for_test_AIC <- -2 * for_test_log_like + 2 * 2
  for_test_BIC <- -2 * for_test_log_like + 2 * log(5)


  # get from function
  fit_stats <- calc_stats(a_model, "fit_stats")
  exp_stats <- data.frame(
    Log_Like = for_test_log_like,
    AIC = for_test_AIC,
    BIC = for_test_BIC
  )
  class(exp_stats) <- c("fit_stats", "stats_dm", "data.frame")
  expect_equal(fit_stats, exp_stats)


  # use AIC k = 3
  expect_equal(AIC(a_model, k = 3), -2 * for_test_log_like + 2 * 3)
})

test_that("fit_stats -> validate and aggregate work as expected", {
  data_id <- data.frame(
    ID = c(1, 2),
    Log_Like = c(1, 2),
    AIC = c(2, 3),
    BIC = c(4, 5)
  )
  data_id <- new_stats_dm(data_id, "fit_stats")



  test <- colMeans(data_id)[-1]

  # test what is returned
  avg <- aggregate_stats(data_id)
  expect_equal(as.numeric(avg), as.numeric(test))
  expect_equal(colnames(avg), names(test))


  # input checks of validate
  temp <- data_id
  temp <- as.matrix(temp)
  class(temp) <- c("fit_stats", "stats_dm", "matrix", "array")
  expect_error(validate_stats_dm(temp), "not of type data.frame")

  temp <- data_id
  colnames(temp)[2] <- "foo"
  expect_error(validate_stats_dm(temp), "Log_Like")

  temp <- data_id
  colnames(temp)[3] <- "foo"
  expect_error(validate_stats_dm(temp), "AIC")

  temp <- data_id
  colnames(temp)[4] <- "foo"
  expect_error(validate_stats_dm(temp), "BIC")
})

test_that("fit_stats -> input checks", {
  expect_error(calc_ic(NULL))

  a_model <- drift_dm(c("a" = 4, "b" = 4),
    conds = c("null", "foo"), dx = 0.005,
    dt = 0.005, t_max = 1, subclass = "test"
  )

  expect_warning(expect_error(calc_ic(a_model)), "No data")
})


# FITS_IDS_DM -----------------------------------------------------------

test_that("fits_ids_dm calc_stats works as expected", {
  all_fits <- load_fits_ids(test_path("fixtures"),
    fit_procedure_name = "test_case_saved"
  )

  all_stats <- calc_stats(all_fits, c("fit_stats", "cafs"))

  # test against one single subject
  all_2 <- all_stats$fit_stats[all_stats$fit_stats$ID == 2, ]
  rownames(all_2) <- 1L
  sep_2 <- calc_stats(all_fits$all_fits$`2`, type = "fit_stats")
  expect_equal(class(all_2), class(sep_2))
  expect_equal(
    all_2[c("Log_Like", "AIC", "BIC")],
    sep_2[c("Log_Like", "AIC", "BIC")]
  )


  # test against one single subject
  all_2 <- all_stats$cafs[all_stats$cafs$ID == 2, ]
  rownames(all_2) <- 1:nrow(all_2)
  sep_2 <- calc_stats(all_fits$all_fits$`2`, type = "cafs")
  expect_equal(class(all_2), class(sep_2))
  expect_equal(
    all_2[c("Source", "Cond", "Bin", "P_corr")],
    sep_2[c("Source", "Cond", "Bin", "P_corr")]
  )


  # test the direct aggregation (aggregation function was tested above)
  agg_stats <- calc_stats(all_fits, c("fit_stats", "cafs"), average = T)
  expect_equal(agg_stats$fit_stats, aggregate_stats(all_stats$fit_stats))
  expect_equal(agg_stats$cafs, aggregate_stats(all_stats$cafs))
})
