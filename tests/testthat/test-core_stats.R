# Basic Stats -------------------------------------------------------------

test_that("calc_basic_stats_obs works as expected", {
  rts_u = c(1, 2, 2, 3, 3, 4)
  rts_l = c(1, 2, 6, 3)

  returned_dat = calc_basic_stats_obs(rts_u, rts_l, one_cond = "foo")

  # expectation
  exp = data.frame(
    Cond = "foo",
    Mean_U = mean(rts_u),
    Mean_L = mean(rts_l),
    SD_U = sd(rts_u),
    SD_L = sd(rts_l),
    P_U = 0.6
  )
  expect_identical(exp, returned_dat)
})


test_that("calc_basic_stats_pred works as expected", {
  dt = .005
  t_vec = seq(0, 2, dt)

  pdf_u = dgamma(t_vec, shape = 2, scale = .125) * 0.65
  pdf_l = dnorm(t_vec, mean = 0.3, sd = 0.05) * 0.35

  returned_dat = calc_basic_stats_pred(
    pdf_u = pdf_u,
    pdf_l = pdf_l,
    one_cond = "bar",
    t_vec = t_vec,
    dt = dt
  )

  # expectation
  exp = data.frame(
    Cond = "bar",
    Mean_U = 0.25,
    Mean_L = 0.3,
    SD_U = 0.17678,
    SD_L = 0.05,
    P_U = 0.65
  )
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
  model <- dmc_dummy
  prms_solve(model)[c("dx", "dt", "t_max")] <- c(.005, .005, 2)

  model <- re_evaluate_model(model)

  # expectations based on the pdfs
  t_vec = seq(0, 2, 0.005)
  pdfs_pred = pdfs(model)$pdfs
  exps_pred_comp = calc_basic_stats_pred(
    pdf_u = pdfs_pred$comp$pdf_u,
    pdf_l = pdfs_pred$comp$pdf_l,
    one_cond = "comp",
    t_vec = t_vec,
    dt = .005,
    skip_if_contr_low = NULL
  )
  exps_pred_comp = cbind(Source = "pred", exps_pred_comp)

  exps_pred_incomp = calc_basic_stats_pred(
    pdf_u = pdfs_pred$incomp$pdf_u,
    pdf_l = pdfs_pred$incomp$pdf_l,
    one_cond = "incomp",
    t_vec = t_vec,
    dt = .005,
    skip_if_contr_low = NULL
  )
  exps_pred_incomp = cbind(Source = "pred", exps_pred_incomp)

  # expectations based on the data
  exps_obs_comp = calc_basic_stats_obs(
    rts_u = model$obs_data$rts_u$comp,
    rts_l = model$obs_data$rts_l$comp,
    one_cond = "comp"
  )
  exps_obs_comp = cbind(Source = "obs", exps_obs_comp)
  exps_obs_incomp = calc_basic_stats_obs(
    rts_u = model$obs_data$rts_u$incomp,
    rts_l = model$obs_data$rts_l$incomp,
    one_cond = "incomp"
  )
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
  expect_equal(
    class(basics),
    c("basic_stats", "sum_dist", "stats_dm", "data.frame")
  )
  expect_equal(
    colnames(basics),
    c("Source", "Cond", "Mean_corr", "Mean_err", "SD_corr", "SD_err", "P_corr")
  )
  expect_true(!is.null(attr(basics, "b_coding")))
})


test_that("basic_stats -> validate work as expected", {
  data_id <- dRiftDM::ulrich_flanker_data
  obs_basic_stats <- calc_stats(data_id, type = "basic_stats")

  test <- aggregate(
    cbind(Mean_corr, Mean_err, SD_corr, SD_err, P_corr) ~ Source + Cond,
    obs_basic_stats,
    mean,
    na.action = na.pass,
    na.rm = T
  )

  # test what is returned
  basic_stats_agg <- calc_stats(data_id, type = "basic_stats", level = "group")
  expect_equal(unpack_obj(basic_stats_agg), test)
  expect_equal(
    class(basic_stats_agg),
    c("basic_stats", "sum_dist", "stats_dm", "data.frame")
  )
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
    calc_basic_stats(
      pdf_u = c(0, 1, 0),
      pdf_l = NULL,
      rts_u = NULL,
      rts_l = NULL,
      one_cond = "foo",
      b_coding = b_coding
    ),
    "both NULL or not"
  )

  expect_error(
    calc_basic_stats(
      pdf_u = NULL,
      pdf_l = NULL,
      rts_u = c(0, 1, 0),
      rts_l = NULL,
      one_cond = "foo",
      b_coding = b_coding
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
  cafs <- calc_stats(
    dat,
    type = "cafs",
    n_bins = 6,
    b_coding = b_coding,
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
  obs_cafs <- calc_stats(data_id, type = "cafs", level = "group")
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
      pdf_u = NULL,
      pdf_l = NULL,
      rts_u = NULL,
      rts_l = NULL,
      one_cond = "foo",
      n_bins = 0,
      b_coding = b_coding
    ),
    "larger than 1"
  )
  expect_error(
    calc_cafs(
      pdf_u = NULL,
      pdf_l = NULL,
      rts_u = NULL,
      rts_l = NULL,
      one_cond = "foo",
      n_bins = NA,
      b_coding = b_coding
    ),
    "a valid numeric"
  )
  expect_error(
    calc_cafs(
      pdf_u = c(0, 1, 0),
      pdf_l = NULL,
      rts_u = NULL,
      rts_l = NULL,
      one_cond = "foo",
      b_coding = b_coding
    ),
    "both NULL or not"
  )

  expect_error(
    calc_cafs(
      pdf_u = NULL,
      pdf_l = NULL,
      rts_u = c(0, 1, 0),
      rts_l = NULL,
      one_cond = "foo",
      b_coding = b_coding
    ),
    "both NULL or not"
  )
})

test_that("calc_cafs -> predicted works as expected", {
  a_model <- dmc_dummy
  pdfs_comp <- a_model$pdfs[["comp"]]
  pdfs_incomp <- a_model$pdfs[["incomp"]]
  pred_cafs <- calc_stats(a_model, type = "cafs")
  pred_cafs <- pred_cafs[pred_cafs$Source == "pred", ]

  # reference obtained by my former package
  expect_true(all(
    abs(
      pred_cafs$P_corr[pred_cafs$Cond == "comp"] -
        c(0.9825608, 0.9824196, 0.9804658, 0.9837679, 0.9892333)
    ) <
      0.01
  ))
  expect_true(all(
    abs(
      pred_cafs$P_corr[pred_cafs$Cond == "incomp"] -
        c(0.8391671, 0.9725687, 0.9871877, 0.9908459, 0.9918593)
    ) <
      0.01
  ))
})


# QUANTILES ---------------------------------------------------------------

test_that("calc_quantiles -> observed and predicted works as expected", {
  # get a dummy model
  dummy_model <- dmc_dummy

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
  quants <- calc_stats(
    dummy_model,
    type = "quantiles",
    probs = seq(0.2, 0.8, 0.1)
  )
  quants_obs <- quants[quants$Source == "obs", ]

  expect_identical(
    quants_obs$Quant_corr[quants_obs$Cond == "comp"],
    unname(quantile(
      dat$RT[dat$Cond == "comp" & dat$Error == 0],
      probs = seq(0.2, 0.8, 0.1)
    ))
  )

  expect_identical(
    quants_obs$Quant_err[quants_obs$Cond == "comp"],
    unname(quantile(
      dat$RT[dat$Cond == "comp" & dat$Error == 1],
      probs = seq(0.2, 0.8, 0.1)
    ))
  )

  expect_identical(
    quants_obs$Quant_corr[quants_obs$Cond == "incomp"],
    unname(quantile(
      dat$RT[dat$Cond == "incomp" & dat$Error == 0],
      probs = seq(0.2, 0.8, 0.1)
    ))
  )

  expect_identical(
    quants_obs$Quant_err[quants_obs$Cond == "incomp"],
    unname(quantile(
      dat$RT[dat$Cond == "incomp" & dat$Error == 1],
      probs = seq(0.2, 0.8, 0.1)
    ))
  )

  ## preds
  quants_pred <- quants[quants$Source == "pred", ]

  expect_true(all(
    abs(
      quants_pred$Quant_corr[quants_pred$Cond == "comp"] -
        c(0.346, 0.365, 0.385, 0.408, 0.435, 0.467, 0.510)
    ) <
      0.002
  )) # values derived by former package

  expect_true(all(
    abs(
      quants_pred$Quant_err[quants_pred$Cond == "comp"] -
        c(0.343, 0.362, 0.380, 0.398, 0.418, 0.443, 0.475)
    ) <
      0.002
  ))

  expect_true(all(
    abs(
      quants_pred$Quant_corr[quants_pred$Cond == "incomp"] -
        c(0.376, 0.395, 0.413, 0.432, 0.454, 0.480, 0.516)
    ) <
      0.002
  ))

  expect_true(all(
    abs(
      quants_pred$Quant_err[quants_pred$Cond == "incomp"] -
        c(0.314, 0.323, 0.332, 0.341, 0.352, 0.365, 0.386)
    ) <
      0.002
  ))

  ### try with different b_coding and via calc_stats.data_frame
  colnames(dat)[2] <- "Foo"
  dat$Foo <- ifelse(dat$Foo == 0, 2, -1)
  quants_obs_new_b <- calc_stats(
    dat,
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

  test <- aggregate(
    cbind(Quant_corr, Quant_err) ~ Prob + Cond + Source,
    obs_quants,
    mean,
    na.rm = T,
    na.action = na.pass
  )
  test <- test[c("Source", "Cond", "Prob", "Quant_corr", "Quant_err")]

  # test what is returned
  obs_quants <- calc_stats(data_id, type = "quantiles", level = "group")
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
      pdf_u = NULL,
      pdf_l = NULL,
      t_vec = t,
      rts_u = NULL,
      rts_l = NULL,
      one_cond = "foo",
      probs = 0,
      b_coding = b_coding
    ),
    "length > 1"
  )
  expect_error(
    calc_quantiles(
      pdf_u = NULL,
      pdf_l = NULL,
      t_vec = t,
      rts_u = NULL,
      rts_l = NULL,
      one_cond = "foo",
      probs = 0,
      b_coding = b_coding
    ),
    "a valid numeric"
  )
  expect_error(
    calc_quantiles(
      pdf_u = c(0, 1, 2),
      pdf_l = NULL,
      t_vec = t,
      rts_u = NULL,
      rts_l = NULL,
      one_cond = "foo",
      b_coding = b_coding
    ),
    "both NULL or not"
  )

  expect_error(
    calc_quantiles(
      pdf_u = NULL,
      pdf_l = NULL,
      t_vec = t,
      rts_u = NULL,
      rts_l = c(0, 1, 2),
      one_cond = "foo",
      b_coding = b_coding
    ),
    "both NULL or not"
  )

  expect_error(
    calc_quantiles(
      pdf_u = c(1, 1, 1),
      pdf_l = c(1, 1, 1),
      one_cond = "foo",
      b_coding = b_coding,
      probs = c(0, 2)
    ),
    "probs"
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
    type = "delta_fun",
    minuends = "incomp",
    subtrahends = "comp"
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
    type = "delta_fun",
    minuends = "incomp",
    subtrahends = "comp",
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
    type = "delta_fun",
    minuends = "incomp",
    subtrahends = "comp"
  )
  delta_dat <- delta_dat[delta_dat$Source == "obs", ]
  obs_delta_dat <- calc_stats(
    data,
    type = "delta_fun",
    minuends = "incomp",
    subtrahends = "comp"
  )
  expect_equal(delta_dat, obs_delta_dat)
})

test_that("delta_funs -> validate and aggregate work as expected", {
  data_id <- dRiftDM::ulrich_flanker_data
  obs_delta <- calc_stats(
    data_id,
    type = "delta_funs",
    minuends = "incomp",
    subtrahends = "comp"
  )

  test <- aggregate(
    cbind(Quant_corr_comp, Delta_incomp_comp) ~ Prob + Source,
    obs_delta,
    mean,
    na.rm = T,
    na.action = na.pass
  )
  test <- test[c("Source", "Prob", "Quant_corr_comp", "Delta_incomp_comp")]

  # test what is returned
  avg_delta <- calc_stats(
    data_id,
    type = "delta_funs",
    minuends = "incomp",
    subtrahends = "comp",
    level = "group"
  )
  expect_equal(avg_delta$Delta_incomp_comp, test$Delta_incomp_comp)
  expect_equal(avg_delta$Quant_corr_comp, test$Quant_corr_comp)

  expect_equal(
    class(avg_delta),
    c("delta_funs", "sum_dist", "stats_dm", "data.frame")
  )
  expect_equal(attr(avg_delta, "b_coding"), drift_dm_default_b_coding())

  # test of one data
  delta_1 <- calc_stats(
    data_id[data_id$ID == 1, ],
    type = "delta_funs",
    minuends = "incomp",
    subtrahends = "comp"
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
      quantiles_dat = NULL,
      minuends = c("incomp", "comp"),
      subtrahends = c("comp"),
      dvs = c("Quant_corr", "Quant_err"),
      b_coding = b_coding
    ),
    "is not a data.frame"
  )

  expect_error(
    calc_delta_funs(
      quantiles_dat = quantiles,
      minuends = c("incomp", "comp"),
      subtrahends = c("comp"),
      dvs = c("Quant_corr", "Quant_err"),
      b_coding = b_coding
    ),
    "length of minuends and subtrahends"
  )

  expect_error(
    calc_delta_funs(
      quantiles_dat = quantiles,
      minuends = c("incomp", "comp"),
      subtrahends = c("comp", "incomp"),
      dvs = c("Quant_corr", "Quant_err", "Quant_corr"),
      b_coding = b_coding
    ),
    "several dvs"
  )

  tmp <- quantiles
  tmp <- rbind(tmp[1, ], tmp)
  expect_error(
    calc_delta_funs(
      quantiles_dat = tmp,
      minuends = c("incomp"),
      subtrahends = c("comp"),
      dvs = c("Quant_corr"),
      b_coding = b_coding
    ),
    "uniquely code"
  )

  expect_warning(
    expect_warning(
      calc_delta_funs(
        quantiles_dat = quantiles,
        minuends = c("incomp", "neutral", "bla"),
        subtrahends = c("incomp", "neutral", "uff"),
        dvs = c("Quant_corr", "Quant_err"),
        b_coding = b_coding
      ),
      "minuends: neutral, bla"
    ),
    "subtrahends: neutral, uff"
  )

  expect_error(
    calc_delta_funs(
      quantiles_dat = quantiles,
      minuends = c("incomp"),
      subtrahends = NULL,
      dvs = c("Quant_corr", "Quant_err"),
      b_coding = b_coding
    ),
    "subtrahends not provided"
  )

  expect_error(
    calc_delta_funs(
      quantiles_dat = quantiles,
      minuends = NULL,
      subtrahends = c("incomp"),
      dvs = c("Quant_corr", "Quant_err"),
      b_coding = b_coding
    ),
    "minuends not provided"
  )

  expect_error(
    calc_delta_funs(
      quantiles_dat = quantiles,
      minuends = "comp",
      subtrahends = "incomp",
      dvs = c("Quant_corr", "Quant_foo"),
      b_coding = b_coding
    ),
    "should be one of \"Quant_corr\""
  )

  temp <- quantiles
  colnames(temp)[5] <- "Quant_foo"
  expect_error(
    calc_delta_funs(
      quantiles_dat = temp,
      minuends = "comp",
      subtrahends = "incomp",
      dvs = c("Quant_corr"),
      b_coding = b_coding
    ),
    "unexpected column names"
  )
})


# FIT STATS ---------------------------------------------------------------

test_that("calc_fit_stats -> works as expected", {
  data <- data.frame(
    RT = c(0.431, 0.402, 0.344, 0.3322, 0.435),
    Error = c(0, 0, 1, 0, 1),
    Cond = c("null", "null", "null", "foo", "foo")
  )

  a_model <- drift_dm(
    c("a" = 4, "b" = 4),
    conds = c("null", "foo"),
    dx = 0.005,
    dt = 0.005,
    t_max = 1,
    subclass = "test"
  )
  obs_data(a_model) <- data
  cost_function(a_model) <- "neg_log_like"
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
  expect_equal(-a_model$cost_value, for_test_log_like)

  # AIC, BIC
  for_test_AIC <- -2 * for_test_log_like + 2 * 2
  for_test_BIC <- -2 * for_test_log_like + 2 * log(5)

  # RMSE
  agg_stats_obs <- calc_stats(data, type = c("cafs", "quantiles"), n_bins = 2)
  tmp <- a_model
  tmp$obs_data <- NULL
  agg_stats_pred <- calc_stats(tmp, type = c("cafs", "quantiles"), n_bins = 2)
  for_test_rmse_s <- calc_rmse(
    quants_pred = agg_stats_pred$quantiles$Quant_corr,
    cafs_pred = agg_stats_pred$cafs$P_corr,
    quants_obs = agg_stats_obs$quantiles$Quant_corr,
    cafs_obs = agg_stats_obs$cafs$P_corr
  )

  # get from function
  fit_stats <- calc_stats(a_model, "fit_stats", n_bins = 2)
  exp_stats <- data.frame(
    Log_Like = for_test_log_like,
    Neg_Log_Like = -for_test_log_like,
    AIC = for_test_AIC,
    BIC = for_test_BIC,
    RMSE_s = for_test_rmse_s,
    RMSE_ms = for_test_rmse_s * 1000
  )
  class(exp_stats) <- c("fit_stats", "stats_dm", "data.frame")
  expect_equal(fit_stats, exp_stats)

  # use AIC() -> k = 3
  expect_equal(AIC(a_model, k = 3), -2 * for_test_log_like + 2 * 3)
})

test_that("fit_stats -> validate and aggregate work as expected", {
  data_id <- data.frame(
    Log_Like = c(1, 2),
    Neg_Log_Like = c(-1, -2),
    AIC = c(2, 3),
    BIC = c(4, 5),
    RMSE_s = 0.1,
    RMSE_ms = 1
  )
  data_id <- apply(data_id, 1, \(one_row) {
    new_stats_dm(as.data.frame(t(one_row)), "fit_stats")
  })
  data_id <- do.call(rbind, data_id)
  data_id$ID = c(1, 2)
  data_id = data_id[c(ncol(data_id), 1:(ncol(data_id) - 1))]

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


# DENSITIES ---------------------------------------------------------------

test_that("calc_dens returns valid data.frame; t_max/discr and scale_mass", {
  # prepare a dummy model and set uneven trial numbers to check scale_mass
  model <- dmc_dummy
  data <- dmc_synth_data
  data <- data[1:400, ]
  obs_data(model) <- data

  t_max <- 2.9
  dt <- 0.01
  prms_solve(model)[c("dx", "dt", "t_max")] <- c(0.01, dt, t_max)

  # choose discr so that ceiling alignment is exercised
  discr <- 0.075 # -> max should be ceiling(t_max / discr) * discr

  out <- calc_stats(model, type = "dens", scale_mass = TRUE, discr = discr)

  # structure
  expect_s3_class(out, "data.frame")
  expect_equal(
    c("Source", "Cond", "Stat", "Time", "Dens_corr", "Dens_err"),
    names(out)
  )
  expect_setequal(unique(out$Cond), c("comp", "incomp"))
  expect_setequal(unique(out$Stat), c("hist", "kde", "pdf"))

  # t_max got aligned to a multiple of discr, and is strictly > max(rt)
  t_max_aligned <- ceiling(t_max / discr) * discr
  expect_equal(t_max_aligned / discr, 39)

  # HIST Stat integrates to the expected mixture weights
  ns <- c(sum(data$Cond == "comp"), sum(data$Cond == "incomp"))
  ws <- ns / (sum(ns) / length(ns))
  ws_comp <- ws[1]
  ws_incomp <- ws[2]

  # comp
  dens_hist_u_comp <- out$Dens_corr[out$Stat == "hist" & out$Cond == "comp"]
  dens_hist_l_comp <- out$Dens_err[out$Stat == "hist" & out$Cond == "comp"]
  n_comp_rtu <- sum(data$Cond == "comp" & data$Error == 0)
  n_comp_rtl <- sum(data$Cond == "comp" & data$Error == 1)
  w_rt_u_comp = n_comp_rtu / (n_comp_rtu + n_comp_rtl)
  w_rt_l_comp = 1 - w_rt_u_comp

  expect_equal(sum(dens_hist_u_comp * discr), w_rt_u_comp * ws_comp)
  expect_equal(sum(dens_hist_l_comp * discr), w_rt_l_comp * ws_comp)

  # incomp
  dens_hist_u_incomp <- out$Dens_corr[out$Stat == "hist" & out$Cond == "incomp"]
  dens_hist_l_incomp <- out$Dens_err[out$Stat == "hist" & out$Cond == "incomp"]
  n_incomp_rtu <- sum(data$Cond == "incomp" & data$Error == 0)
  n_incomp_rtl <- sum(data$Cond == "incomp" & data$Error == 1)
  w_rt_u_incomp = n_incomp_rtu / (n_incomp_rtu + n_incomp_rtl)
  w_rt_l_incomp = 1 - w_rt_u_incomp

  expect_equal(sum(dens_hist_u_incomp * discr), w_rt_u_incomp * ws_incomp)
  expect_equal(
    sum(dens_hist_l_incomp * discr, na.rm = TRUE),
    w_rt_l_incomp * ws_incomp
  )

  # KDE Stat integrates to the expected mixture weights
  # comp
  dens_kde_u_comp <- out$Dens_corr[out$Stat == "kde" & out$Cond == "comp"]
  dens_kde_l_comp <- out$Dens_err[out$Stat == "kde" & out$Cond == "comp"]
  expect_equal(sum(dens_kde_u_comp * discr), w_rt_u_comp * ws_comp)
  expect_equal(sum(dens_kde_l_comp * discr), w_rt_l_comp * ws_comp)

  # incomp
  dens_kde_u_incomp <- out$Dens_corr[out$Stat == "kde" & out$Cond == "incomp"]
  dens_kde_l_incomp <- out$Dens_err[out$Stat == "kde" & out$Cond == "incomp"]
  expect_equal(sum(dens_kde_u_incomp * discr), w_rt_u_incomp * ws_incomp)
  expect_equal(
    sum(dens_kde_l_incomp * discr, na.rm = TRUE),
    w_rt_l_incomp * ws_incomp
  )

  # PDFs integrate to the expected mixture weights
  # comp
  pdf_u_comp <- out$Dens_corr[out$Stat == "pdf" & out$Cond == "comp"]
  pdf_l_comp <- out$Dens_err[out$Stat == "pdf" & out$Cond == "comp"]
  expect_equal(sum(pdf_u_comp * dt) + sum(pdf_l_comp * dt), ws_comp)

  # incomp
  pdf_u_incomp <- out$Dens_corr[out$Stat == "pdf" & out$Cond == "incomp"]
  pdf_l_incomp <- out$Dens_err[out$Stat == "pdf" & out$Cond == "incomp"]
  expect_equal(sum(pdf_u_incomp * dt) + sum(pdf_l_incomp * dt), ws_incomp)

  # check the time ticks
  time_model <- seq(0, t_max, dt)
  time_obs <- seq(0, round(t_max_aligned / discr) * discr, discr)
  mids <- time_obs[-length(time_obs)] + diff(time_obs) / 2
  # comp hist, incomp hist, comp kde, incomp kde, comp pdf, incomp pdf
  time <- c(mids, mids, mids, mids, time_model, time_model)
  expect_equal(out$Time, time)
})

test_that("calc_dens_obs handles degenerate samples for KDE (<= 1 value)", {
  # One side with a single RT -> KDE returns NaNs; hist still defined
  rts_u <- 0.3
  rts_l <- rexp(50, rate = 3)
  out <- calc_dens_obs(rts_u, rts_l, one_cond = "A", discr = 0.02)

  kde_rows <- out[out$Stat == "kde", ]
  expect_true(all(is.nan(kde_rows$Dens_U))) # single-value KDE -> NaN
  expect_false(any(is.nan(kde_rows$Dens_L))) # enough values for KDE
})

test_that("calc_dens errors on inconsistent input pairs", {
  bc <- drift_dm_default_b_coding()
  tvec <- seq(0, 0.5, by = 0.01)
  pdf_u <- dnorm(tvec, mean = 0.25, sd = 0.08)
  pdf_l <- dnorm(tvec, mean = 0.25, sd = 0.08)

  # only one of pdf_u/pdf_l -> error
  expect_error(
    calc_dens(
      pdf_u = pdf_u,
      pdf_l = NULL,
      t_vec = tvec,
      one_cond = "A",
      b_coding = bc
    ),
    "pdf_l and pdf_u either have to be both NULL or not"
  )
  # only one of rts_u/rts_l -> error
  expect_error(
    calc_dens(rts_u = 0.3, rts_l = NULL, one_cond = "A", b_coding = bc),
    "rts_u and rts_l either have to be both NULL or not"
  )
})


test_that("calc_dens -> validate and aggregate work as expected", {
  data_id <- dRiftDM::ulrich_flanker_data
  data_id <- data_id[data_id$ID == c(1, 2), ]
  data_id <- data_id[data_id$Error == 0, ]
  obs_dens <- calc_stats(data_id, type = "dens")

  test <- aggregate(
    cbind(Dens_corr, Dens_err) ~ Time + Stat + Cond + Source,
    obs_dens,
    mean,
    na.rm = T,
    na.action = na.pass
  )
  test$Dens_err <- NA_real_

  # test what is returned
  avg_dens <- calc_stats(data_id, type = "dens", level = "group")
  expect_equal(avg_dens$Dens_corr, test$Dens_corr)
  expect_equal(avg_dens$Dens_err, test$Dens_err)

  expect_equal(
    class(avg_dens),
    c("densities", "sum_dist", "stats_dm", "data.frame")
  )
  expect_equal(attr(avg_dens, "b_coding"), drift_dm_default_b_coding())

  # test of one data
  dens_1 <- calc_stats(data_id[data_id$ID == 1, ], type = "dens")
  obs_dens_1 <- obs_dens[obs_dens$ID == 1, ]
  expect_equal(dens_1, obs_dens_1)

  # input checks of validate
  temp <- obs_dens
  attr(temp, "b_coding") <- NULL
  expect_error(validate_stats_dm(temp), "b_coding")

  temp <- obs_dens
  colnames(temp)[2] <- "foo"
  expect_error(validate_stats_dm(temp), "Source")

  temp <- obs_dens
  colnames(temp)[3] <- "foo"
  expect_error(validate_stats_dm(temp), "Cond")

  temp <- obs_dens
  colnames(temp)[4] <- "foo"
  expect_error(validate_stats_dm(temp), "Stat")

  temp <- obs_dens
  colnames(temp)[5] <- "foo"
  expect_error(validate_stats_dm(temp), "Time")

  temp <- obs_dens
  temp$Dens_corr <- NULL
  expect_error(validate_stats_dm(temp), "Dens_")
})


# CALC_STATS_PRED_OBS -----------------------------------------------------

test_that("calc_stats_pred_obs -> input checks for type and scale_mass", {
  expect_error(
    calc_stats_pred_obs(
      type = c("foo", "bar"),
      b_coding = NULL,
      conds = "foo"
    ),
    "type"
  )

  expect_error(
    calc_stats_pred_obs(
      type = "foo",
      b_coding = NULL,
      conds = "foo",
      scale_mass = 1
    ),
    "scale_mass"
  )
})


# DATA.FRAME --------------------------------------------------------------

test_that("calc_stats.data.frame -> input validation of flags", {
  some_data <- dRiftDM::dmc_synth_data

  # invalid 'type' -> match.arg error
  expect_error(
    calc_stats(some_data, type = "not_a_type"),
    "should be one of"
  )

  # 'resample' must be single logical
  expect_error(
    calc_stats(some_data, type = "quantiles", resample = 1:2),
    "resample must be a single logical"
  )

  # 'progress' must be 0 or 1
  expect_error(
    calc_stats(some_data, type = "quantiles", progress = -1),
    "progress must be 0 or 1"
  )

  # 'level' -> match.arg error
  expect_error(
    calc_stats(some_data, type = "quantiles", level = "foo"),
    "should be one of"
  )
})

test_that("calc_stats.data.frame -> deprecation of split_by_ID and average", {
  some_data <- dRiftDM::ulrich_flanker_data
  some_data <- some_data[some_data$ID %in% c(1, 2), ]

  # split_by_ID = TRUE -> level becomes "individual"
  lifecycle::expect_deprecated(
    res_ind <- calc_stats(some_data, type = "quantiles", split_by_ID = TRUE),
    "split_by_ID"
  )
  expect_true("ID" %in% names(res_ind)) # individual returns per-ID stats

  # split_by_ID = FALSE -> level becomes "group"
  lifecycle::expect_deprecated(
    res_grp <- calc_stats(some_data, type = "quantiles", split_by_ID = FALSE),
    "split_by_ID"
  )
  expect_false("ID" %in% names(res_grp)) # group-level has no ID column

  # average mirrors the same mapping
  lifecycle::expect_deprecated(
    res_grp2 <- calc_stats(some_data, type = "quantiles", average = TRUE),
    "average"
  )
  expect_false("ID" %in% names(res_grp2))

  lifecycle::expect_deprecated(
    res_ind2 <- calc_stats(some_data, type = "quantiles", average = FALSE),
    "average"
  )
  expect_true("ID" %in% names(res_ind2))
})

test_that("calc_stats.data.frame -> group level requires ID column", {
  # drop ID to trigger the error
  some_data <- dRiftDM::dmc_synth_data
  expect_error(
    calc_stats(some_data, type = "quantiles", level = "group"),
    "contains an 'ID' column"
  )
})


# DRIFT_DM --------------------------------------------------------------

test_that("calc_stats.drift_dm -> resampling not possible for fit_stats", {
  model <- dmc_dummy
  expect_warning(
    res1 <- calc_stats(model, type = "fit_stats", resample = TRUE),
    "setting `resampling = FALSE`"
  )

  # should be equal to setting resample = FALSE
  res2 <- calc_stats(model, type = "fit_stats", resample = FALSE)
  expect_identical(res1, res2)
})

test_that("calc_stats.drift_dm -> prob mass warning", {
  model <- dmc_dummy
  coef(model)["non_dec"] = 2.5
  expect_warning(
    res1 <- calc_stats(model, type = "fit_stats", resample = TRUE),
    "setting `resampling = FALSE`"
  )

  # should be equal to setting resample = FALSE
  res2 <- calc_stats(model, type = "fit_stats", resample = FALSE)
  expect_identical(res1, res2)
})

# FITS_IDS_DM -----------------------------------------------------------

test_that("calc_stats.fits_ids_dm -> works as expected", {
  all_fits <- get_example_fits("fits_ids")

  all_stats <- calc_stats(all_fits, c("fit_stats", "cafs"), progress = 0)

  # test against one single subject
  all_2 <- all_stats$fit_stats[all_stats$fit_stats$ID == 2, ]
  rownames(all_2) <- 1L
  sep_2 <- calc_stats(all_fits$all_fits$`2`, type = "fit_stats")
  expect_equal(class(all_2), class(sep_2))
  expect_equal(all_2[, -1], sep_2)

  # test against one single subject
  all_2 <- all_stats$cafs[all_stats$cafs$ID == 2, ]
  rownames(all_2) <- 1:nrow(all_2)
  sep_2 <- calc_stats(all_fits$all_fits$`2`, type = "cafs")
  expect_equal(class(all_2), class(sep_2))
  expect_equal(unpack_obj(all_2[, -1]), unpack_obj(sep_2))

  # test the direct aggregation (aggregation function was tested above)
  agg_stats <- calc_stats(all_fits, c("fit_stats", "cafs"), level = "group")
  expect_equal(agg_stats$fit_stats, aggregate_stats(all_stats$fit_stats))
  expect_equal(agg_stats$cafs, aggregate_stats(all_stats$cafs))
})


test_that("calc_stats.fits_ids_dm -> validates conds and progress", {
  fits_ids <- get_example_fits("fits_ids")

  # invalid condition name -> match.arg error
  expect_error(
    calc_stats(fits_ids, type = "quantiles", conds = "definitely_not_a_cond"),
    "should be one of"
  )

  # progress must be 0 or 1
  expect_error(
    calc_stats(fits_ids, type = "quantiles", progress = -1),
    "progress must be 0 or 1"
  )
})


test_that("calc_stats.fits_ids_dm -> resampling not possible for fit_stats", {
  fits_ids <- get_example_fits("fits_ids")
  expect_warning(
    res1 <- calc_stats(fits_ids, type = "fit_stats", resample = TRUE),
    "setting `resampling = FALSE`"
  )

  # should be equal to setting resample = FALSE
  res2 <- calc_stats(fits_ids, type = "fit_stats", resample = FALSE)
  expect_identical(res1, res2)
})

test_that("calc_stats.fits_ids_dm -> deprecation of average sets level", {
  fits_ids <- get_example_fits("fits_ids")

  # average = TRUE -> group
  lifecycle::expect_deprecated(
    res_grp <- calc_stats(fits_ids, type = "quantiles", average = TRUE),
    "average"
  )
  expect_false("ID" %in% names(res_grp))

  # average = FALSE -> individual
  lifecycle::expect_deprecated(
    res_ind <- calc_stats(fits_ids, type = "quantiles", average = FALSE),
    "average"
  )
  expect_true("ID" %in% names(res_ind))
})


test_that("calc_stats.fits_ids_dm -> respects conds subset", {
  fits_ids <- get_example_fits("fits_ids")
  valid <- conds(fits_ids)
  subset_conds <- valid[2]

  # calculate for one condition
  res1 <- calc_stats(
    fits_ids,
    type = "quantiles",
    conds = subset_conds,
    level = "individual",
    progress = 0
  )
  expect_true(all(res1$Cond %in% subset_conds))

  # calculate for both and check that the subset matches
  res2 <- calc_stats(
    fits_ids,
    type = "quantiles",
    level = "individual",
    progress = 0
  )
  res2 <- res2[res2$Cond == subset_conds, ]
  rownames(res2) <- NULL
  expect_equal(res1, res2)
})


# FITS_AGG_DM -------------------------------------------------------------

test_that("calc_stats.fits_agg_dm -> works as expected", {
  # group-level
  fits_agg <- get_example_fits("fits_agg_dm")
  all_stats <- calc_stats(fits_agg, c("fit_stats", "cafs"), progress = 0)

  # test cafs against direct call
  exp_cafs_pred <- calc_stats(fits_agg$drift_dm_obj, "cafs")
  is_cafs_pred <- all_stats$cafs[all_stats$cafs$Source == "pred", ]
  rownames(is_cafs_pred) <- NULL
  expect_equal(exp_cafs_pred, is_cafs_pred)

  exp_cafs_obs <- calc_stats(fits_agg$obs_data_ids, "cafs", level = "group")
  is_cafs_obs <- all_stats$cafs[all_stats$cafs$Source == "obs", ]
  rownames(is_cafs_obs) <- NULL
  expect_equal(exp_cafs_obs, is_cafs_obs)

  # test fit_stats against direct call
  exp_fit_stats <- calc_fit_stats(fits_agg$drift_dm_obj)
  expect_identical(all_stats$fit_stats, exp_fit_stats)

  # individual level
  caf_stats <- calc_stats(fits_agg, "cafs", progress = 0, level = "individual")
  expect_s3_class(caf_stats, "cafs")

  # check structure and only NAs for ID when pred
  expect_true("ID" %in% names(caf_stats))
  expect_true(all(is.na(caf_stats$ID[caf_stats$Source == "pred"])))
  expect_true(all(!is.na(caf_stats$ID[caf_stats$Source == "obs"])))

  # check against one single subject
  cafs_2 <- caf_stats[caf_stats$ID == 2 & caf_stats$Source == "obs", ]
  rownames(cafs_2) <- NULL
  data_2 = fits_agg$obs_data_ids[fits_agg$obs_data_ids$ID == 2, ]
  sep_2 <- calc_stats(data_2, type = "cafs")
  expect_equal(class(cafs_2), class(sep_2))
})


test_that("calc_stats.fits_agg_dm -> validates conds and progress", {
  fits_agg <- get_example_fits("fits_agg")

  # invalid condition name -> match.arg error
  expect_error(
    calc_stats(fits_agg, type = "quantiles", conds = "definitely_not_a_cond"),
    "should be"
  )

  # progress must be 0 or 1
  expect_error(
    calc_stats(fits_agg, type = "quantiles", progress = -1),
    "progress must be 0 or 1"
  )
})


test_that("calc_stats.fits_ids_dm -> resampling not possible for fit_stats", {
  fits_agg <- get_example_fits("fits_agg")
  expect_warning(
    res1 <- calc_stats(fits_agg, type = "fit_stats", resample = TRUE),
    "setting `resampling = FALSE`"
  )

  # should be equal to setting resample = FALSE
  res2 <- calc_stats(fits_agg, type = "fit_stats", resample = FALSE)
  expect_identical(res1, res2)
})


test_that("calc_stats.fits_agg_dm -> deprecation of average sets level", {
  fits_agg <- get_example_fits("fits_agg")

  # average = TRUE -> group
  lifecycle::expect_deprecated(
    res_grp <- calc_stats(fits_agg, type = "quantiles", average = TRUE),
    "average"
  )
  expect_false("ID" %in% names(res_grp))

  # average = FALSE -> individual
  lifecycle::expect_deprecated(
    res_ind <- calc_stats(fits_agg, type = "quantiles", average = FALSE),
    "average"
  )
  expect_true("ID" %in% names(res_ind))
})

test_that("calc_stats.fits_agg_dm -> n_sim messages are correctly triggered", {
  fits_agg <- get_example_fits("fits_agg")

  local_mocked_bindings(
    stats_resample_dm.drift_dm = function(...) {
      list(...)$n_sim
    },
    calc_stats.data.frame = function(...) NULL
  )

  # using the average trial
  expect_message(
    {
      cafs <- calc_stats(fits_agg, "cafs", resample = TRUE)
    },
    "average trial"
  )
  expect_identical(as.numeric(cafs), 100)

  # or some explicit value
  expect_message(
    {
      cafs <- calc_stats(fits_agg, "cafs", resample = TRUE, n_sim = 50)
    },
    "specified by 'n_sim'"
  )
  expect_identical(as.numeric(cafs), 50)
})


# RESAMPLING --------------------------------------------------------------

test_that("calc_stats.data.frame -> resampling at group level works", {
  data <- dRiftDM::ulrich_flanker_data
  withr::local_seed(1)
  cafs <- calc_stats(
    data,
    type = "cafs",
    resample = TRUE,
    progress = FALSE,
    R = 10,
    level = "group"
  )

  # do it manually...
  # get the cafs per subject
  cafs_standard <- calc_stats(
    data,
    type = "cafs",
    level = "individual"
  )
  cafs_split <- split(cafs_standard, cafs_standard$ID)
  stopifnot(sapply(cafs_split, \(x) unique(x$ID)) == names(cafs_split))

  # now do it manually
  withr::local_seed(1)
  pos_idxs = names(cafs_split)
  idx_list = replicate(
    n = 10,
    expr = sample(x = pos_idxs, size = length(pos_idxs), replace = TRUE),
    simplify = FALSE
  )

  stats <- lapply(idx_list, \(one_set) {
    boot_cafs <- cafs_split[one_set]
    boot_cafs <- do.call(rbind, boot_cafs)
    aggregate_stats(boot_cafs)
  })

  # test for equality....
  # no resampling and with resampling
  agg_standard = aggregate_stats(cafs_standard)
  expect_equal(agg_standard$P_corr, cafs$P_corr[cafs$Estimate == "orig"])

  # for manual resampling
  Ps_comp <- sapply(stats, \(x) x$P_corr) # rows are Ps, cols are runs
  range <- apply(Ps_comp, 1, stats::quantile, probs = c(0.025, 0.975))
  expect_equal(range[1, ], cafs$P_corr[cafs$Estimate == "2.5%"])
  expect_equal(range[2, ], cafs$P_corr[cafs$Estimate == "97.5%"])
})


test_that("calc_stats.fits_ids_dm -> resampling at group level works", {
  all_fits <- get_example_fits("fits_ids")
  withr::local_seed(1)
  cafs <- calc_stats(
    all_fits,
    type = "cafs",
    level = "group",
    resample = TRUE,
    R = 10
  )

  # get the cafs per subject
  cafs_per_subject <- calc_stats(
    all_fits,
    type = "cafs",
    level = "individual"
  )
  cafs_split <- split(cafs_per_subject, cafs_per_subject$ID)
  cafs_split <- lapply(cafs_split, \(x) x[x$Source == "pred", ])
  stopifnot(sapply(cafs_split, \(x) unique(x$ID)) == names(cafs_split))

  # now do it manually; we have to run the index creation twice to have
  # the same rng value (when calling resample, we no only perform resamples
  # for the model preds but also for the obs!)
  withr::local_seed(1)
  pos_idxs = names(cafs_split)
  idx_list = replicate(
    n = 20,
    expr = sample(x = pos_idxs, size = length(pos_idxs), replace = TRUE),
    simplify = FALSE
  )
  idx_list = idx_list[11:20]

  stats <- lapply(idx_list, \(one_set) {
    boot_cafs <- cafs_split[one_set]
    boot_cafs <- do.call(rbind, boot_cafs)
    aggregate_stats(boot_cafs)
  })

  # test for equality....
  # no resampling and with resampling
  agg_standard = aggregate_stats(cafs_per_subject)
  expect_equal(cafs$P_corr[cafs$Estimate == "orig"], agg_standard$P_corr)

  # for manual resampling
  Ps_comp <- sapply(stats, \(x) x$P_corr) # rows are Ps, cols are runs
  range <- apply(Ps_comp, 1, stats::quantile, probs = c(0.025, 0.975))
  cafs_pred <- cafs[cafs$Source == "pred", ]
  expect_equal(range[1, ], cafs_pred$P_corr[cafs_pred$Estimate == "2.5%"])
  expect_equal(range[2, ], cafs_pred$P_corr[cafs_pred$Estimate == "97.5%"])
})
