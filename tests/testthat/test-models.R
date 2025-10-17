test_that("testing DMC", {
  # get the component values
  a_dmc_model <- dmc_dm(dt = .001, dx = .001)
  all_comps <- comp_vals(a_dmc_model)

  a_dmc_model_im <- a_dmc_model
  suppressWarnings(solver(a_dmc_model_im) <- "im_zero")
  all_comps_im <- comp_vals(a_dmc_model_im)

  # check names of comp_values
  expect_identical(
    names(all_comps_im$comp),
    c("mu_vals", "mu_int_vals", "x_vals", "b_vals", "dt_b_vals", "nt_vals")
  )
  expect_identical(names(all_comps_im$comp), names(all_comps_im$incomp))

  expect_identical(
    names(all_comps$comp),
    c("mu_vals", "x_vals", "b_vals", "dt_b_vals", "nt_vals")
  )
  expect_identical(names(all_comps$comp), names(all_comps$incomp))

  # test equality of comp_values
  temp <- all_comps_im
  temp$comp$mu_int_vals <- NULL
  temp$incomp$mu_int_vals <- NULL
  expect_identical(temp, all_comps)

  ###  DMC tests for drift
  # test the drift rate
  mu_t <- all_comps$comp$mu_vals[c(0.002, 0.2) / 0.001 + 1]
  expect_equal(round(mu_t, 5), c(10.14106, 3.81684))

  mu_t <- all_comps$incomp$mu_vals[c(0.003, 0.3) / 0.001 + 1]
  expect_equal(round(mu_t, 5), c(-1.83182, 4.02443))

  # test integral of the drift rate
  mu_t <- all_comps_im$comp$mu_int_vals[c(0.002, 0.2) / 0.001 + 1]
  expect_equal(round(mu_t, 6), c(0.020929, 0.809158))

  mu_t <- all_comps_im$incomp$mu_int_vals[c(0.003, 0.3) / 0.001 + 1]
  expect_equal(round(mu_t, 6), c(-0.006914, 1.198872))

  ## DMC tests for drift
  # with a != 2
  a_dmc_model <- dmc_dm(instr = "a ~ => 2.1", dt = .001, dx = .001)
  all_comps <- comp_vals(a_dmc_model)

  # just to ensure comp_vals evaluates the integral
  a_dmc_model_im <- a_dmc_model
  suppressWarnings(solver(a_dmc_model_im) <- "im_zero")
  all_comps_im <- comp_vals(a_dmc_model_im)

  # drift rate
  mu_t <- all_comps$comp$mu_vals[c(0.002, 0.2) / 0.001 + 1]
  expect_equal(round(mu_t, 5), c(9.006200, 3.79129))
  mu_t <- all_comps$incomp$mu_vals[c(0.003, 0.3) / 0.001 + 1]
  expect_equal(round(mu_t, 5), c(-0.96272, 4.02928))

  # test integral of the drift rate
  mu_t <- all_comps_im$comp$mu_int_vals[c(0.002, 0.2) / 0.001 + 1]
  expect_equal(round(mu_t, 7), c(0.0175355, 0.810705))
  mu_t <- all_comps_im$incomp$mu_int_vals[c(0.003, 0.3) / 0.001 + 1]
  expect_equal(round(mu_t, 6), c(-0.002527, 1.198627))

  # test the boundary
  b_t <- all_comps$comp$b_vals[c(0.002, 0.2) / 0.001 + 1]
  expect_equal(b_t, c(0.6, 0.6))
  dt_b_t <- all_comps$comp$dt_b_vals[c(0.002, 0.2) / 0.001 + 1]
  expect_equal(dt_b_t, c(0, 0))

  # test the starting condition
  x_x <- all_comps$comp$x_vals
  x_seq <- seq(0, 1, length.out = a_dmc_model$prms_solve[["nx"]] + 1)
  d_x <- stats::dbeta(x_seq, 4, 4) / 2
  d_x <- d_x / (sum(d_x) * a_dmc_model$prms_solve[["dx"]])
  expect_identical(d_x, x_x)

  # test the non-decision time
  pdf_nt <- all_comps$comp$nt_vals
  pdf_test <- truncnorm::dtruncnorm(
    seq(0, 3, .001),
    a = 0,
    mean = 0.3,
    sd = .02
  )
  pdf_test <- pdf_test / (sum(pdf_test) * 0.001)
  expect_equal(pdf_test, pdf_nt)

  ##########
  # test with different component functions
  a_dmc_model <- dmc_dm(var_non_dec = F, var_start = F)
  all_comps <- comp_vals(a_dmc_model)

  # test start_vals
  expect_equal(all_comps$comp$x_vals, all_comps$incomp$x_vals)
  expect_equal(
    all_comps$comp$x_vals,
    x_dirac_0(
      NULL,
      a_dmc_model$prms_solve,
      seq(-1, 1, 0.02),
      NULL,
      NULL
    )
  )

  # test non_dec
  expect_equal(all_comps$comp$nt_vals, all_comps$incomp$nt_vals)
  expect_equal(
    all_comps$comp$nt_vals,
    nt_constant(
      c(non_dec = 0.3),
      a_dmc_model$prms_solve,
      seq(0, 3, 0.0075),
      NULL,
      NULL
    )
  )

  #####
  # Test scaling
  a_dmc_model <- dmc_dm(t_max = 1000, dt = 5, dx = .1, sigma = 4)
  a_dmc_model <- modify_flex_prms(
    a_dmc_model,
    instr = "
                                  muc ~ => 0.5
                                  b ~ => 75
                                  non_dec ~ => 300
                                  sd_non_dec ~ => 30
                                  tau ~ => 50
                                  A ~ comp => 20
                                  alpha ~ => 2"
  )
  quants_ms <- calc_stats(a_dmc_model, "quantiles")

  ###
  # compare solution in seconds with sigma = 1 and milliseconds with sigma = 4
  a_dmc_model <- dmc_dm(t_max = 1, dt = .005, dx = .1, sigma = 1)
  a_dmc_model <- modify_flex_prms(
    a_dmc_model,
    instr = "
                                  muc ~ => 3.952847
                                  b ~ => 0.5929271
                                  non_dec ~ => 0.300
                                  sd_non_dec ~ => 0.03
                                  tau ~ => 0.05
                                  A ~ comp => 0.1581139
                                  alpha ~ => 2"
  )
  quants_s <- calc_stats(a_dmc_model, "quantiles")
  diff_corr <- quants_s$Quant_corr * 1000 - quants_ms$Quant_corr
  diff_err <- quants_s$Quant_err * 1000 - quants_ms$Quant_err
  expect_true(all(abs(diff_corr) < 0.2))
  expect_true(all(abs(diff_err) < 0.2))

  ## roughly compare with DMCfun # 1
  a_dmc_model <- dmc_dm(dt = .005, dx = .005)
  a_dmc_model <- modify_flex_prms(
    a_dmc_model,
    instr = "
                                  muc ~ => 4
                                  b ~ => 0.6
                                  non_dec ~ => 0.3
                                  sd_non_dec ~ => 0.02
                                  tau ~ => 0.04
                                  A ~ comp => 0.1
                                  alpha ~ => 4"
  )
  sim_data <- DMCfun::dmcSim(
    drc = 0.5059644,
    bnds = 75.89466,
    amp = 12.64911,
    tau = 40,
    resMean = 300,
    resSD = 20,
    spShape = 4,
    spDist = 1,
    spLim = c(-75.89466, 75.89466),
    setSeed = T,
    printInputArgs = F,
    printResults = F
  )
  dmc_cafs <- calc_stats(a_dmc_model, type = "cafs", source = "pred")
  dmc_quants <- calc_stats(a_dmc_model, type = "quantiles", source = "pred")

  expect_true(
    all(
      abs(
        sim_data$delta$meanComp -
          dmc_quants$Quant_corr[dmc_quants$Cond == "comp"] * 1000
      ) <
        10
    )
  )

  expect_true(
    all(
      abs(
        sim_data$delta$meanIncomp -
          dmc_quants$Quant_corr[dmc_quants$Cond == "incomp"] * 1000
      ) <
        10
    )
  )

  expect_true(
    all(
      abs(
        sim_data$caf$accPerComp - dmc_cafs$P_err[dmc_cafs$Cond == "comp"]
      ) <
        .01
    )
  )

  expect_true(
    all(
      abs(
        sim_data$caf$accPerIncomp[2:5] -
          dmc_cafs$P_err[dmc_cafs$Cond == "incomp"][2:5]
      ) <
        .01
    )
  )

  expect_true(
    all(
      abs(
        sim_data$caf$accPerIncomp[1] -
          dmc_cafs$P_err[dmc_cafs$Cond == "incomp"][1]
      ) <
        .035
    )
  ) # kfe predict more fast errors in general
})


test_that("ratcliff_simple works as expected", {
  a_model <- ratcliff_dm()

  # test the drift rate
  mu_t <- a_model$comp_funs$mu_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1, ],
    prms_solve = a_model$prms_solve,
    t_vec = c(0.002, 0.2),
    one_cond = "w",
    ddm_opts = NULL
  )
  expect_equal(mu_t, c(3, 3))
  mu_t <- a_model$comp_funs$mu_int_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1, ],
    prms_solve = a_model$prms_solve,
    t_vec = c(0.002, 0.2),
    one_cond = "w",
    ddm_opts = NULL
  )
  expect_equal(mu_t, c(3 * 0.002, 3 * 0.2))

  # test the boundary
  b_t <- a_model$comp_funs$b_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1, ],
    prms_solve = a_model$prms_solve,
    t_vec = c(0.002, 0.2),
    one_cond = "w",
    ddm_opts = NULL
  )
  expect_equal(b_t, c(0.6, 0.6))
  dt_b_t <- a_model$comp_funs$dt_b_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1, ],
    prms_solve = a_model$prms_solve,
    t_vec = c(0.002, 0.2),
    one_cond = "w",
    ddm_opts = NULL
  )
  expect_equal(dt_b_t, c(0, 0))

  # test the starting condition
  x_seq <- seq(-1, 1, length.out = a_model$prms_solve[["nx"]] + 1)
  x_x <- a_model$comp_funs$x_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1, ],
    prms_solve = a_model$prms_solve,
    x_vec = x_seq,
    one_cond = "w",
    ddm_opts = NULL
  )
  d_x <- rep(0, a_model$prms_solve[["nx"]] + 1)
  d_x[(a_model$prms_solve[["nx"]] + 2) / 2] <- 1 / a_model$prms_solve[["dx"]]
  expect_identical(d_x, x_x)

  # test the non-decision time
  pdf_nt <- a_model$comp_funs$nt_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1, ],
    prms_solve = a_model$prms_solve,
    t_vec = seq(0, 3, a_model$prms_solve[["dt"]]),
    one_cond = "w",
    ddm_opts = NULL
  )
  pdf_test <- rep(0, a_model$prms_solve[["nt"]] + 1)
  pdf_test[0.3 / a_model$prms_solve[["dt"]] + 1] <- 1 /
    a_model$prms_solve[["dt"]]
  expect_equal(pdf_test, pdf_nt)

  # equal accuracy?
  pdfs <- re_evaluate_model(a_model)$pdfs[["null"]]
  pdfs[[1]] <- pdfs[[1]] / sum(pdfs[[1]])
  pdfs[[2]] <- pdfs[[2]] / sum(pdfs[[2]])
  expect_true(all(abs(pdfs[[1]] - pdfs[[2]]) < 0.001))
})


test_that("ratcliff with var. in non-dec or start point works as expected", {
  a_model <- ratcliff_dm(var_non_dec = T, dx = .001, dt = .001)

  # test the drift rate
  mu_t <- a_model$comp_funs$mu_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1, ],
    prms_solve = a_model$prms_solve,
    t_vec = c(0.002, 0.2),
    one_cond = "w",
    ddm_opts = NULL
  )
  expect_equal(mu_t, c(3, 3))
  mu_t <- a_model$comp_funs$mu_int_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1, ],
    prms_solve = a_model$prms_solve,
    t_vec = c(0.002, 0.2),
    one_cond = "w",
    ddm_opts = NULL
  )
  expect_equal(mu_t, c(3 * 0.002, 3 * 0.2))

  # test the boundary
  b_t <- a_model$comp_funs$b_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1, ],
    prms_solve = a_model$prms_solve,
    t_vec = c(0.002, 0.2),
    one_cond = "w",
    ddm_opts = NULL
  )
  expect_equal(b_t, c(0.6, 0.6))
  dt_b_t <- a_model$comp_funs$dt_b_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1, ],
    prms_solve = a_model$prms_solve,
    t_vec = c(0.002, 0.2),
    one_cond = "w",
    ddm_opts = NULL
  )
  expect_equal(dt_b_t, c(0, 0))

  # test the starting condition
  x_seq <- seq(-1, 1, length.out = a_model$prms_solve[["nx"]] + 1)
  x_x <- a_model$comp_funs$x_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1, ],
    prms_solve = a_model$prms_solve,
    x_vec = x_seq,
    one_cond = "w",
    ddm_opts = NULL
  )
  d_x <- rep(0, a_model$prms_solve[["nx"]] + 1)
  d_x[(a_model$prms_solve[["nx"]] + 2) / 2] <- 1 / a_model$prms_solve[["dx"]]
  expect_identical(d_x, x_x)

  # test the non-decision time
  pdf_nt <- a_model$comp_funs$nt_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1, ],
    prms_solve = a_model$prms_solve,
    t_vec = seq(0, 3, a_model$prms_solve[["dx"]]),
    one_cond = "w",
    ddm_opts = NULL
  )
  t_vec <- seq(0, a_model$prms_solve[["t_max"]], a_model$prms_solve[["dt"]])
  pdf_test <- numeric(length(t_vec))
  min_cut <- 0.3 - 0.05 / 2
  min_cut <- min_cut / a_model$prms_solve[["dt"]] + 1
  max_cut <- 0.3 + 0.05 / 2
  max_cut <- max_cut / a_model$prms_solve[["dt"]] + 1
  max_cut <- round(max_cut)
  min_cut <- round(min_cut)

  pdf_test[min_cut:max_cut] <- 1
  pdf_test <- pdf_test / (sum(pdf_test) * a_model$prms_solve[["dt"]])

  expect_equal(pdf_test, pdf_nt)

  pdfs <- re_evaluate_model(a_model)$pdfs[["null"]]
  pdfs[[1]] <- pdfs[[1]] / sum(pdfs[[1]])
  pdfs[[2]] <- pdfs[[2]] / sum(pdfs[[2]])
  expect_true(all(abs(pdfs[[1]] - pdfs[[2]]) < 0.001))

  # NOW THE VARIABLE START POINT
  a_model <- ratcliff_dm(var_start = T, dx = .001, dt = .001)

  # test the drift rate
  mu_t <- a_model$comp_funs$mu_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1, ],
    prms_solve = a_model$prms_solve,
    t_vec = c(0.002, 0.2),
    one_cond = "w",
    ddm_opts = NULL
  )
  expect_equal(mu_t, c(3, 3))
  mu_t <- a_model$comp_funs$mu_int_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1, ],
    prms_solve = a_model$prms_solve,
    t_vec = c(0.002, 0.2),
    one_cond = "w",
    ddm_opts = NULL
  )
  expect_equal(mu_t, c(3 * 0.002, 3 * 0.2))

  # test the boundary
  b_t <- a_model$comp_funs$b_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1, ],
    prms_solve = a_model$prms_solve,
    t_vec = c(0.002, 0.2),
    one_cond = "w",
    ddm_opts = NULL
  )
  expect_equal(b_t, c(0.6, 0.6))
  dt_b_t <- a_model$comp_funs$dt_b_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1, ],
    prms_solve = a_model$prms_solve,
    t_vec = c(0.002, 0.2),
    one_cond = "w",
    ddm_opts = NULL
  )
  expect_equal(dt_b_t, c(0, 0))

  # test the starting condition
  x_seq <- seq(-1, 1, length.out = a_model$prms_solve[["nx"]] + 1)
  x_x <- a_model$comp_funs$x_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1, ],
    prms_solve = a_model$prms_solve,
    x_vec = x_seq,
    one_cond = "w",
    ddm_opts = NULL
  )

  pdf_test <- dunif(
    x_seq,
    0 - 0.5 / 2,
    0 + 0.5 / 2
  )
  pdf_test <- pdf_test / (sum(pdf_test) * 0.001)
  expect_identical(pdf_test, x_x)

  # test the non-decision time
  pdf_nt <- a_model$comp_funs$nt_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1, ],
    prms_solve = a_model$prms_solve,
    t_vec = seq(0, 3, a_model$prms_solve[["dt"]]),
    one_cond = "w",
    ddm_opts = NULL
  )
  pdf_test <- rep(0, a_model$prms_solve[["nt"]] + 1)
  pdf_test[0.3 / a_model$prms_solve[["dt"]] + 1] <- 1 /
    a_model$prms_solve[["dt"]]
  expect_equal(pdf_test, pdf_nt)

  # NOW THE VARIABLE DRIFT RATE
  a_model <- ratcliff_dm(var_drift = T, dx = .005, dt = .005, t_max = 2.5)

  # check expected behavior
  a_model <- re_evaluate_model(a_model)
  expect_equal(length(a_model$pdfs$null$pdf_u), 501)
  expect_equal(length(a_model$pdfs$null$pdf_l), 501)
  expect_true(
    abs(sum(a_model$pdfs$null$pdf_l) + sum(a_model$pdfs$null$pdf_u)) *
      0.005 -
      1 <
      0.01
  )

  cafs <- calc_stats(a_model, type = "cafs")
  expect_true(all(diff(cafs$P_corr) < 0))
})


test_that("SSP model provides reasonable values", {
  a_model <- ssp_dm(
    dt = .005,
    dx = .005,
    instr = "
                   b ~ => 0.6
                   non_dec ~ => 0.3
                   range_non_dec ~ => 0.01
                   p ~ => 3.3
                   sd_0 ~ => 1.2
                   r ~ => 10"
  )

  x_vec <- seq(-1, 1, .005)
  t_vec <- seq(0, a_model$prms_solve[["t_max"]], .005)

  prms_model <- a_model$flex_prms_obj$prms_matrix[1, ]
  conds <- a_model$conds
  prms_solve <- a_model$prms_solve

  # check coefficients
  expect_equal(
    colnames(a_model$flex_prms_obj$prms_matrix),
    c("b", "non_dec", "range_non_dec", "p", "sd_0", "r", "sign")
  )
  expect_equal(
    names(coef(a_model)),
    c("b", "non_dec", "range_non_dec", "p", "sd_0")
  )

  # b_fun
  expect_equal(
    a_model$comp_funs$b_fun(
      prms_model,
      prms_solve,
      t_vec,
      one_cond = NA,
      ddm_opts = NULL
    ),
    rep(0.6, length(t_vec))
  )

  expect_equal(
    a_model$comp_funs$b_fun(
      prms_model,
      prms_solve,
      t_vec,
      one_cond = NA,
      ddm_opts = NULL
    ),
    rep(0.6, length(t_vec))
  )

  # dt_b_fun
  expect_equal(
    a_model$comp_funs$dt_b_fun(
      prms_model,
      prms_solve,
      t_vec,
      one_cond = NA,
      ddm_opts = NULL
    ),
    rep(0, length(t_vec))
  )

  expect_equal(
    a_model$comp_funs$dt_b_fun(
      prms_model,
      prms_solve,
      t_vec,
      one_cond = NA,
      ddm_opts = NULL
    ),
    rep(0, length(t_vec))
  )

  # x_fun
  exp_x <- rep(0, length(x_vec))
  exp_x[201] <- 1 / .005
  expect_equal(
    a_model$comp_funs$x_fun(
      prms_model,
      prms_solve,
      x_vec,
      one_cond = NA,
      ddm_opts = NULL
    ),
    exp_x
  )

  expect_equal(
    a_model$comp_funs$x_fun(
      prms_model,
      prms_solve,
      x_vec,
      one_cond = NA,
      ddm_opts = NULL
    ),
    exp_x
  )

  # mu_fun
  sd_t <- 1.2 - 10 * t_vec
  sd_t <- pmax(sd_t, .001)
  a_tar <- pnorm(q = 0.5, mean = 0, sd = sd_t) -
    pnorm(q = -0.5, mean = 0, sd = sd_t)
  a_fl <- 1 - a_tar
  prms_model <- a_model$flex_prms_obj$prms_matrix[1, ]
  expect_equal(
    a_model$comp_funs$mu_fun(
      prms_model,
      prms_solve,
      t_vec,
      one_cond = NA,
      ddm_opts = NULL
    ),
    3.3 * a_tar + 3.3 * a_fl
  )
  prms_model <- a_model$flex_prms_obj$prms_matrix[2, ]
  expect_equal(
    a_model$comp_funs$mu_fun(
      prms_model,
      prms_solve,
      t_vec,
      one_cond = NA,
      ddm_opts = NULL
    ),
    3.3 * a_tar - 3.3 * a_fl
  )

  # mu_int_fun
  expect_error(
    a_model$comp_funs$mu_int_fun(
      prms_model,
      prms_solve,
      t_vec,
      one_cond = NA,
      ddm_opts = NULL
    ),
    "this should not be called"
  )

  expect_error(
    a_model$comp_funs$mu_int_fun(
      prms_model,
      prms_solve,
      t_vec,
      one_cond = NA,
      ddm_opts = NULL
    ),
    "this should not be called"
  )

  # nt_fun
  exp_nt <- dunif(t_vec, min = 0.3 - 0.01 / 2, max = 0.3 + 0.01 / 2)
  exp_nt <- exp_nt / (sum(exp_nt) * .005)
  expect_equal(
    a_model$comp_funs$nt_fun(
      prms_model = prms_model,
      prms_solve = prms_solve,
      t_vec = t_vec,
      one_cond = NA,
      ddm_opts = NULL
    ),
    exp_nt
  )
})


test_that("SSP with var. in non-dec or start point works", {
  a_model <- ssp_dm(
    dt = .005,
    dx = .005,
    var_non_dec = FALSE,
    var_start = TRUE,
    instr = "b ~ => 0.6
             non_dec ~ => 0.3
             range_start ~ => 0.2
             p ~ => 3.3
             sd_0 ~ => 1.2
             r ~ => 10"
  )

  # check coefficients
  expect_equal(
    colnames(a_model$flex_prms_obj$prms_matrix),
    c("b", "non_dec", "p", "sd_0", "r", "range_start", "sign")
  )
  expect_equal(
    names(coef(a_model)),
    c("b", "non_dec", "p", "sd_0", "range_start")
  )

  # check the component functions
  x_vec <- seq(-1, 1, .005)
  t_vec <- seq(0, a_model$prms_solve[["t_max"]], .005)

  prms_model <- a_model$flex_prms_obj$prms_matrix[1, ]
  conds <- a_model$conds
  prms_solve <- a_model$prms_solve

  # b_fun
  expect_equal(
    a_model$comp_funs$b_fun(
      prms_model,
      prms_solve,
      t_vec,
      one_cond = NA,
      ddm_opts = NULL
    ),
    rep(0.6, length(t_vec))
  )

  expect_equal(
    a_model$comp_funs$b_fun(
      prms_model,
      prms_solve,
      t_vec,
      one_cond = NA,
      ddm_opts = NULL
    ),
    rep(0.6, length(t_vec))
  )

  # dt_b_fun
  expect_equal(
    a_model$comp_funs$dt_b_fun(
      prms_model,
      prms_solve,
      t_vec,
      one_cond = NA,
      ddm_opts = NULL
    ),
    rep(0, length(t_vec))
  )

  expect_equal(
    a_model$comp_funs$dt_b_fun(
      prms_model,
      prms_solve,
      t_vec,
      one_cond = NA,
      ddm_opts = NULL
    ),
    rep(0, length(t_vec))
  )

  # x_fun
  exp_x <- dunif(x_vec, min = -0.2 / 2, max = 0.2 / 2)
  exp_x <- exp_x / (sum(exp_x) * .005)
  expect_equal(
    a_model$comp_funs$x_fun(
      prms_model,
      prms_solve,
      x_vec,
      one_cond = NA,
      ddm_opts = NULL
    ),
    exp_x
  )

  expect_equal(
    a_model$comp_funs$x_fun(
      prms_model,
      prms_solve,
      x_vec,
      one_cond = NA,
      ddm_opts = NULL
    ),
    exp_x
  )

  # mu_fun
  sd_t <- 1.2 - 10 * t_vec
  sd_t <- pmax(sd_t, .001)
  a_tar <- pnorm(q = 0.5, mean = 0, sd = sd_t) -
    pnorm(q = -0.5, mean = 0, sd = sd_t)
  a_fl <- 1 - a_tar
  prms_model <- a_model$flex_prms_obj$prms_matrix[1, ]
  expect_equal(
    a_model$comp_funs$mu_fun(
      prms_model,
      prms_solve,
      t_vec,
      one_cond = NA,
      ddm_opts = NULL
    ),
    3.3 * a_tar + 3.3 * a_fl
  )
  prms_model <- a_model$flex_prms_obj$prms_matrix[2, ]
  expect_equal(
    a_model$comp_funs$mu_fun(
      prms_model,
      prms_solve,
      t_vec,
      one_cond = NA,
      ddm_opts = NULL
    ),
    3.3 * a_tar - 3.3 * a_fl
  )

  # mu_int_fun
  expect_error(
    a_model$comp_funs$mu_int_fun(
      prms_model,
      prms_solve,
      t_vec,
      one_cond = NA,
      ddm_opts = NULL
    ),
    "this should not be called"
  )

  expect_error(
    a_model$comp_funs$mu_int_fun(
      prms_model,
      prms_solve,
      t_vec,
      one_cond = NA,
      ddm_opts = NULL
    ),
    "this should not be called"
  )

  # nt_fun
  exp_nt <- rep(0, length(t_vec))
  exp_nt[0.3 / 0.005 + 1] = 1 / .005
  expect_equal(
    a_model$comp_funs$nt_fun(
      prms_model = prms_model,
      prms_solve = prms_solve,
      t_vec = t_vec,
      one_cond = NA,
      ddm_opts = NULL
    ),
    exp_nt
  )
})


# TEST INDIVIDUAL MODEL COMPONENTS ----------------------------------------

test_that("mu_constant", {
  mu_vals <- mu_constant(
    prms_model = c("muc" = 4),
    prms_solve = NULL,
    t_vec = c(0, 0.5, 1),
    one_cond = NULL,
    ddm_opts = NULL
  )
  expect_equal(mu_vals, rep(4, 3))

  mu_vals <- mu_int_constant(
    prms_model = c("muc" = 4),
    prms_solve = NULL,
    t_vec = c(0, 0.5, 1),
    one_cond = NULL,
    ddm_opts = NULL
  )
  expect_equal(mu_vals, 4 * c(0, 0.5, 1))

  # input checks mu_constant
  expect_error(
    mu_constant(
      prms_model = c("muc" = "4"),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "not a single number"
  )

  expect_error(
    mu_constant(
      prms_model = c("muc" = 5),
      prms_solve = NULL,
      t_vec = c(0),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "not a numeric vector"
  )

  # input checks mu_int_constant
  expect_error(
    mu_int_constant(
      prms_model = c("muc" = "4"),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "not a single number"
  )

  expect_error(
    mu_int_constant(
      prms_model = c("muc" = 5),
      prms_solve = NULL,
      t_vec = c(0),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "not a numeric vector"
  )
})


test_that("mu_dmc", {
  # return values are tested when testing DMC, here just input checks
  # input checks mu_dmc
  expect_error(
    mu_dmc(
      prms_model = c(muc = "4", tau = 0.3, a = 2, A = 0.2),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "muc is not a single number"
  )

  expect_error(
    mu_dmc(
      prms_model = list(muc = 4, tau = c(0.3, "0.4"), a = 2, A = 0.2),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "tau is not a single number"
  )

  expect_error(
    mu_dmc(
      prms_model = list(muc = 4, tau = 0.3, a = list(2, 4), A = 0.2),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "a is not a single number"
  )

  expect_error(
    mu_dmc(
      prms_model = list(muc = 4, tau = 0.3, a = 2, A = NULL),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "A is not a single number"
  )

  expect_error(
    mu_dmc(
      prms_model = list(muc = 4, tau = 0.3, a = 2, A = 0.2),
      prms_solve = NULL,
      t_vec = c(0, 0.5, "1"),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "not a numeric vector"
  )

  # input checks mu_int_dmc
  expect_error(
    mu_int_dmc(
      prms_model = c(muc = "4", tau = 0.3, a = 2, A = 0.2),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "muc is not a single number"
  )

  expect_error(
    mu_int_dmc(
      prms_model = list(muc = 4, tau = c(0.3, "0.4"), a = 2, A = 0.2),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "tau is not a single number"
  )

  expect_error(
    mu_int_dmc(
      prms_model = list(muc = 4, tau = 0.3, a = list(2, 4), A = 0.2),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "a is not a single number"
  )

  expect_error(
    mu_int_dmc(
      prms_model = list(muc = 4, tau = 0.3, a = 2, A = NULL),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "A is not a single number"
  )

  expect_error(
    mu_int_dmc(
      prms_model = list(muc = 4, tau = 0.3, a = 2, A = 0.2),
      prms_solve = NULL,
      t_vec = c(0, 0.5, "1"),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "not a numeric vector"
  )
})


test_that("mu_ssp", {
  # return values are tested when testing DMC, here just input checks
  # input checks mu_dmc
  expect_error(
    mu_ssp(
      prms_model = c(p = "3", sd_0 = 1.2, r = 10, sign = 1),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "p is not a single number"
  )

  expect_error(
    mu_ssp(
      prms_model = list(p = 3, sd_0 = NA, r = 10, sign = 1),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "sd_0 is not a single number"
  )

  expect_error(
    mu_ssp(
      prms_model = list(p = 3, sd_0 = 1.2, r = NA, sign = 1),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "r is not a single number"
  )

  expect_error(
    mu_ssp(
      prms_model = list(p = 3, sd_0 = 1.2, r = 10, sign = NA),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "sign is not a single number"
  )

  expect_error(
    mu_ssp(
      prms_model = list(p = 3, sd_0 = 1.2, r = 10, sign = 1),
      prms_solve = NULL,
      t_vec = c(0, 0.5, "1"),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "not a numeric vector"
  )
})


test_that("x_beta", {
  # return values are tested when testing DMC, here just input checks

  expect_error(
    x_beta(
      prms_model = c(alpha = "4"),
      prms_solve = c(dx = .02),
      x_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "alpha is not a single number"
  )

  expect_error(
    x_beta(
      prms_model = c(alpha = 4),
      prms_solve = c(dx = .02),
      x_vec = c("1", 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "x_vec is not a numeric vector"
  )

  # input checks mu_int_dmc
  expect_error(
    mu_int_dmc(
      prms_model = c(muc = "4", tau = 0.3, a = 2, A = 0.2),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "muc is not a single number"
  )

  expect_error(
    mu_int_dmc(
      prms_model = list(muc = 4, tau = c(0.3, "0.4"), a = 2, A = 0.2),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "tau is not a single number"
  )

  expect_error(
    mu_int_dmc(
      prms_model = list(muc = 4, tau = 0.3, a = list(2, 4), A = 0.2),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "a is not a single number"
  )

  expect_error(
    mu_int_dmc(
      prms_model = list(muc = 4, tau = 0.3, a = 2, A = NULL),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "A is not a single number"
  )

  expect_error(
    mu_int_dmc(
      prms_model = list(muc = 4, tau = 0.3, a = 2, A = 0.2),
      prms_solve = NULL,
      t_vec = c(0, 0.5, "1"),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "not a numeric vector"
  )
})


test_that("x_dirac_0", {
  x_vals <- x_dirac_0(
    prms_model = NULL,
    prms_solve = c(dx = .01),
    x_vec = c(-1, -0.5, 0, 0.5, 1),
    one_cond = NULL,
    ddm_opts = NULL
  )
  expect_equal(x_vals, c(0, 0, 1 / .01, 0, 0))

  # input check
  expect_error(
    x_dirac_0(
      prms_model = NULL,
      prms_solve = c(dx = "4"),
      x_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "not a single number"
  )

  expect_error(
    x_dirac_0(
      prms_model = NULL,
      prms_solve = c(dx = .01),
      x_vec = c(0, 0.5, "1"),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "not a numeric vector"
  )
})


test_that("x_uniform", {
  x_vals <- x_uniform(
    prms_model = c(range_start = 1),
    prms_solve = c(dx = .01),
    x_vec = c(-1, -0.51, -0.5, -0.25, 0, 0.25, 0.5, 0.51, 1),
    one_cond = NULL,
    ddm_opts = NULL
  )
  expect_equal(x_vals, c(0, 0, 20, 20, 20, 20, 20, 0, 0))

  # input check
  expect_error(
    x_uniform(
      prms_model = c(range_start = "0.1"),
      prms_solve = c(dx = .01),
      x_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "not a single number"
  )

  expect_error(
    x_uniform(
      prms_model = c(range_start = 0.1),
      prms_solve = c(dx = .01),
      x_vec = c(0, 0.5, "1"),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "not a numeric vector"
  )
})


test_that("b_constant", {
  b_vals <- b_constant(
    prms_model = c("b" = 4),
    prms_solve = NULL,
    t_vec = c(0, 0.5, 1),
    one_cond = NULL,
    ddm_opts = NULL
  )
  expect_equal(b_vals, rep(4, 3))

  b_vals <- dt_b_constant(
    prms_model = c("b" = 4),
    prms_solve = NULL,
    t_vec = c(0, 0.5, 1),
    one_cond = NULL,
    ddm_opts = NULL
  )
  expect_equal(b_vals, rep(0, 3))

  # input check b_constant
  expect_error(
    b_constant(
      prms_model = c(b = "0.1"),
      prms_solve = c(dx = .01),
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "not a single number"
  )

  expect_error(
    b_constant(
      prms_model = c(b = 0.1),
      prms_solve = c(dx = .01),
      t_vec = c(0, 0.5, "1"),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "not a numeric vector"
  )

  # input check dt_b_constant
  expect_error(
    dt_b_constant(
      prms_model = c(b = 0.1),
      prms_solve = c(dx = .01),
      t_vec = c(0, 0.5, "1"),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "not a numeric vector"
  )
})


test_that("nt_constant", {
  t_vec <- seq(0, 1, 0.01)
  nt_vals <- nt_constant(
    prms_model = c("non_dec" = 0.3),
    prms_solve = c(t_max = 1, dt = .01),
    t_vec = t_vec,
    one_cond = NULL,
    ddm_opts = NULL
  )
  exp_vals <- numeric(length = 101)
  exp_vals[31] <- 1 / .01
  expect_equal(nt_vals, exp_vals)

  # input checks
  expect_error(
    nt_constant(
      prms_model = c(non_dec = "0.1"),
      prms_solve = c(t_max = 1, dt = .01),
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "not a single number"
  )

  expect_error(
    nt_constant(
      prms_model = c(non_dec = 0.1),
      prms_solve = c(t_max = "1", dt = .01),
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "t_max is not a single number"
  )

  expect_error(
    nt_constant(
      prms_model = c(non_dec = 0.1),
      prms_solve = list(t_max = 1, dt = c(.01, 0.1)),
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "dt is not a single number"
  )

  expect_error(
    nt_constant(
      prms_model = c(non_dec = -0.1),
      prms_solve = c(t_max = 1, dt = .5),
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "larger than t_max or smaller than 0"
  )

  expect_error(
    nt_constant(
      prms_model = c(non_dec = 2),
      prms_solve = c(t_max = 1, dt = .5),
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "larger than t_max or smaller than 0"
  )

  expect_error(
    nt_constant(
      prms_model = c(non_dec = 0.5),
      prms_solve = c(t_max = 1, dt = .5),
      t_vec = c(0, 0.5, "1"),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "not a numeric vector"
  )

  expect_error(
    nt_constant(
      prms_model = c(non_dec = 0.5),
      prms_solve = c(t_max = 1, dt = .5),
      t_vec = c(0),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "not a numeric vector with more than one entry"
  )
})


test_that("nt_uniform", {
  t_vec <- seq(0, 1, 0.01)
  nt_vals <- nt_uniform(
    prms_model = c(non_dec = 0.3, range_non_dec = 0.1),
    prms_solve = c(t_max = 1, dt = .01),
    t_vec = t_vec,
    one_cond = NULL,
    ddm_opts = NULL
  )
  exp_vals <- dunif(x = t_vec, min = 0.25, max = 0.35)
  expect_equal(nt_vals, exp_vals)

  # input check
  expect_error(
    nt_uniform(
      prms_model = c(non_dec = "0.1", range_non_dec = 0.1),
      prms_solve = c(t_max = 1, dt = .01),
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "non_dec is not a single number"
  )

  expect_error(
    nt_uniform(
      prms_model = list(non_dec = 0.1, range_non_dec = c(0.1, 0.2)),
      prms_solve = c(t_max = 1, dt = .01),
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "range_non_dec is not a single number"
  )

  expect_error(
    nt_uniform(
      prms_model = c(non_dec = 0.1, range_non_dec = 0.1),
      prms_solve = c(t_max = "1", dt = .01),
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "t_max is not a single number"
  )

  expect_error(
    nt_uniform(
      prms_model = c(non_dec = 0.1, range_non_dec = 0.1),
      prms_solve = list(t_max = 1, dt = c(.01, 0.05)),
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "dt is not a single number"
  )

  expect_error(
    nt_uniform(
      prms_model = c(non_dec = -0.1, range_non_dec = 0.1),
      prms_solve = c(t_max = 1, dt = .01),
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "larger than t_max or smaller than 0"
  )

  expect_error(
    nt_uniform(
      prms_model = c(non_dec = 2, range_non_dec = 0.1),
      prms_solve = c(t_max = 1, dt = .01),
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "larger than t_max or smaller than 0"
  )

  expect_error(
    nt_uniform(
      prms_model = c(non_dec = 0.5, range_non_dec = 0.01),
      prms_solve = c(t_max = 1, dt = .01),
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "range_non_dec should not be smaller than dt"
  )

  expect_error(
    nt_uniform(
      prms_model = c(non_dec = 0.5, range_non_dec = 0.05),
      prms_solve = c(t_max = 1, dt = .01),
      t_vec = c(0, 0.5, "1"),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "not a numeric vector"
  )
})


test_that("nt_truncated", {
  # return values are tested when testing DMC/SSP, here just input checks
  expect_error(
    nt_truncated_normal(
      prms_model = c(non_dec = "0.1", sd_non_dec = 0.05),
      prms_solve = c(t_max = 1, dt = .01),
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "non_dec is not a single number"
  )

  expect_error(
    nt_truncated_normal(
      prms_model = list(non_dec = 0.1, sd_non_dec = "0.05"),
      prms_solve = c(t_max = 1, dt = .01),
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "sd_non_dec is not a single number"
  )

  expect_error(
    nt_truncated_normal(
      prms_model = c(non_dec = 0.1, sd_non_dec = 0.05),
      prms_solve = c(t_max = "1", dt = .01),
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "t_max is not a single number"
  )

  expect_error(
    nt_truncated_normal(
      prms_model = c(non_dec = 0.1, sd_non_dec = 0.05),
      prms_solve = list(t_max = 1, dt = c(.01, 0.05)),
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "dt is not a single number"
  )

  expect_error(
    nt_truncated_normal(
      prms_model = c(non_dec = -0.1, sd_non_dec = 0.05),
      prms_solve = c(t_max = 1, dt = .01),
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "larger than t_max or smaller than 0"
  )

  expect_error(
    nt_truncated_normal(
      prms_model = c(non_dec = 2, sd_non_dec = 0.05),
      prms_solve = c(t_max = 1, dt = .01),
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "larger than t_max or smaller than 0"
  )

  expect_error(
    nt_truncated_normal(
      prms_model = c(non_dec = 0.1, sd_non_dec = 0.005),
      prms_solve = c(t_max = 1, dt = .01),
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "sd_non_dec should not be smaller than dt"
  )

  expect_error(
    nt_truncated_normal(
      prms_model = c(non_dec = 0.1, sd_non_dec = 0.005),
      prms_solve = c(t_max = 1, dt = .01),
      t_vec = c(0, 0.5, "1"),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "not a numeric vector"
  )
})


test_that("b_hyperbol", {
  t_vec <- seq(0, 1, 0.0001)
  b_vals <- b_hyperbol(
    prms_model = c(b0 = 75, kappa = 0.6, t05 = 0.15),
    prms_solve = c(t_max = 1, dt = .0001),
    t_vec = t_vec,
    one_cond = NULL,
    ddm_opts = NULL
  )
  exp_vals <- 75 * (1 - 0.6 * (t_vec / (t_vec + 0.15)))
  expect_equal(exp_vals, b_vals)

  # numerical differentiation to check the derivative of b_hyperbol
  numerical_derivative <- function(f, x, h) {
    (f[x + 1] - f[x]) / h # forward difference formula
  }
  exp_vals <- numerical_derivative(b_vals, 1:100, h = 0.0001)

  t_vec <- seq(.0001 / 2, 1, .0001)
  dt_b_vals <- dt_b_hyperbol(
    prms_model = c(b0 = 75, kappa = 0.6, t05 = 0.15),
    prms_solve = c(t_max = 1, dt = .0001),
    t_vec = t_vec,
    one_cond = NULL,
    ddm_opts = NULL
  )
  dt_b_vals <- dt_b_vals[1:100]

  expect_true(all(abs(dt_b_vals - exp_vals) < .001))

  # input checks b_hyperbol
  expect_error(
    b_hyperbol(
      prms_model = c(b0 = "75", kappa = 0.6, t05 = 0.15),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "b0 is not a single number"
  )

  expect_error(
    b_hyperbol(
      prms_model = list(b0 = 75, kappa = NA, t05 = 0.15),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "kappa is not a single number"
  )

  expect_error(
    b_hyperbol(
      prms_model = list(b0 = 75, kappa = 0.6, t05 = c(0.2, 0.1)),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "t05 is not a single number"
  )

  expect_error(
    b_hyperbol(
      prms_model = list(b0 = 75, kappa = 0.6, t05 = 0.15),
      prms_solve = NULL,
      t_vec = c(0, 0.5, "1"),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "not a numeric vector"
  )

  # input checks dt_b_hyperbol
  expect_error(
    dt_b_hyperbol(
      prms_model = c(b0 = "75", kappa = 0.6, t05 = 0.15),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "b0 is not a single number"
  )

  expect_error(
    dt_b_hyperbol(
      prms_model = list(b0 = 75, kappa = NA, t05 = 0.15),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "kappa is not a single number"
  )

  expect_error(
    dt_b_hyperbol(
      prms_model = list(b0 = 75, kappa = 0.6, t05 = c(0.2, 0.1)),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "t05 is not a single number"
  )

  expect_error(
    dt_b_hyperbol(
      prms_model = list(b0 = 75, kappa = 0.6, t05 = 0.15),
      prms_solve = NULL,
      t_vec = c(0, 0.5, "1"),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "not a numeric vector"
  )
})


test_that("b_weibull", {
  t_vec <- seq(0, 1, 0.0001)
  b_vals <- b_weibull(
    prms_model = c(b0 = 75, lambda = 0.5, k = 3, kappa = 1),
    prms_solve = c(t_max = 1, dt = .0001),
    t_vec = t_vec,
    one_cond = NULL,
    ddm_opts = NULL
  )
  # as described in Eq 1 by Hawkins
  exp_vals <- 150 - (1 - exp(-(t_vec / 0.5)^3)) * (150 - 75)
  expect_equal(exp_vals - 75, b_vals)

  # numerical differentiation to check the derivative of b_hyperbol
  numerical_derivative <- function(f, x, h) {
    (f[x + 1] - f[x]) / h # forward difference formula
  }
  exp_vals <- numerical_derivative(b_vals, 1:100, h = 0.0001)

  t_vec <- seq(.0001 / 2, 1, .0001)
  dt_b_vals <- dt_b_weibull(
    prms_model = c(b0 = 75, lambda = 0.5, k = 3, kappa = 1),
    prms_solve = c(t_max = 1, dt = .0001),
    t_vec = t_vec,
    one_cond = NULL,
    ddm_opts = NULL
  )
  dt_b_vals <- dt_b_vals[1:100]

  expect_true(all(abs(dt_b_vals - exp_vals) < .001))

  # input checks b_weibull
  expect_error(
    b_weibull(
      prms_model = c(b0 = "75", lambda = 0.5, k = 3, kappa = 1),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "b0 is not a single number"
  )

  expect_error(
    b_weibull(
      prms_model = list(b0 = 75, lambda = NA, k = 3, kappa = 1),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "lambda is not a single number"
  )

  expect_error(
    b_weibull(
      prms_model = list(b0 = 75, lambda = 0.5, k = NA, kappa = 1),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "k is not a single number"
  )

  expect_error(
    b_weibull(
      prms_model = list(b0 = 75, lambda = 0.5, k = 3, kappa = c(1, 2)),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "kappa is not a single number"
  )

  expect_error(
    b_weibull(
      prms_model = list(b0 = 75, lambda = 0.5, k = 3, kappa = 1),
      prms_solve = NULL,
      t_vec = c(0),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "not a numeric vector with more than one entry"
  )

  # input checks dt_b_weibull
  expect_error(
    dt_b_weibull(
      prms_model = c(b0 = "75", lambda = 0.5, k = 3, kappa = 1),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "b0 is not a single number"
  )

  expect_error(
    dt_b_weibull(
      prms_model = list(b0 = 75, lambda = NA, k = 3, kappa = 1),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "lambda is not a single number"
  )

  expect_error(
    dt_b_weibull(
      prms_model = list(b0 = 75, lambda = 0.5, k = NA, kappa = 1),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "k is not a single number"
  )

  expect_error(
    dt_b_weibull(
      prms_model = list(b0 = 75, lambda = 0.5, k = 3, kappa = c(1, 2)),
      prms_solve = NULL,
      t_vec = c(0, 0.5, 1),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "kappa is not a single number"
  )

  expect_error(
    dt_b_weibull(
      prms_model = list(b0 = 75, lambda = 0.5, k = 3, kappa = 1),
      prms_solve = NULL,
      t_vec = c(0),
      one_cond = NULL,
      ddm_opts = NULL
    ),
    "not a numeric vector with more than one entry"
  )
})


# ensure correct assignment of the component_shelf ------------------------

test_that("component_shelf", {
  all_comp_funs <- component_shelf()
  for (i in names(all_comp_funs)) {
    expect_equal(all_comp_funs[[i]], get(i))
  }
})


test_that("test_dummy", {
  a_model <- drift_dm(c(a = 2, b = 3), "comp", subclass = "test")
  a_model$comp_funs$mu_fun <- dummy_t
  expect_error(comp_vals(a_model), "should not be called")
})


# get_lower_upper works as expected ------------------------------------

test_that("get_lower_upper returns expected bounds for Ratcliff components.", {
  model <- ratcliff_dm(var_drift = TRUE, var_start = TRUE, var_non_dec = TRUE)
  res <- get_lower_upper(model)

  expect_named(res, c("lower", "upper"))
  exp_prms <- c("muc", "b", "non_dec", "range_non_dec", "range_start", "sd_muc")
  expect_equal(names(res$lower), exp_prms)
  expect_equal(names(res$upper), exp_prms)

  # check the values
  expect_equal(res$lower[["muc"]], 0.5)
  expect_equal(res$upper[["muc"]], 9)

  expect_equal(res$lower[["b"]], 0.15)
  expect_equal(res$upper[["b"]], 1.20)

  expect_equal(res$lower[["non_dec"]], 0.15)
  expect_equal(res$upper[["non_dec"]], 0.60)

  expect_true(res$lower[["range_non_dec"]] >= 0.01)
  expect_true(res$upper[["range_non_dec"]] <= 0.4)

  expect_true(res$lower[["range_start"]] >= 0.01)
  expect_equal(res$upper[["range_start"]], 1.5)

  expect_equal(res$lower[["sd_muc"]], 0.01)
  expect_equal(res$upper[["sd_muc"]], 3.00)

  # is lower < upper?
  expect_true(all(res$lower < res$upper))
})


test_that("get_lower_upper returns expected bounds for DMC components.", {
  model <- dmc_dummy
  res <- get_lower_upper(model)

  expect_named(res, c("lower", "upper"))
  exp_prms <- c("muc", "b", "non_dec", "sd_non_dec", "tau", "A", "alpha")
  expect_equal(names(res$lower), exp_prms)
  expect_equal(names(res$upper), exp_prms)

  # fixed-value checks
  expect_equal(res$lower[["muc"]], 0.5)
  expect_equal(res$upper[["muc"]], 9)

  expect_equal(res$lower[["b"]], 0.15)
  expect_equal(res$upper[["b"]], 1.20)

  expect_equal(res$lower[["non_dec"]], 0.15)
  expect_equal(res$upper[["non_dec"]], 0.60)

  expect_equal(res$lower[["tau"]], 0.015)
  expect_equal(res$upper[["tau"]], 0.25)

  expect_equal(res$lower[["A"]], 0.005)
  expect_equal(res$upper[["A"]], 0.3)

  expect_equal(res$lower[["alpha"]], 2)
  expect_equal(res$upper[["alpha"]], 8)

  # value checks depending on dt
  dt <- prms_solve(model)["dt"]
  expect_equal(res$lower[["sd_non_dec"]], 0.005)
  expect_equal(res$upper[["sd_non_dec"]], 0.1)

  # lower < upper
  expect_true(all(res$lower < res$upper))
})


test_that("get_lower_upper returns expected bounds for SSP components.", {
  # var_non_dec = FALSE to catch all cases
  model <- ssp_dm(var_non_dec = FALSE)
  res <- get_lower_upper(model)

  expect_named(res, c("lower", "upper"))
  exp_prms <- c("b", "non_dec", "p", "sd_0")
  expect_equal(names(res$lower), exp_prms)
  expect_equal(names(res$upper), exp_prms)

  # fixed-value checks
  expect_equal(res$lower[["b"]], 0.15)
  expect_equal(res$upper[["b"]], 1.20)

  expect_equal(res$lower[["non_dec"]], 0.15)
  expect_equal(res$upper[["non_dec"]], 0.60)

  expect_equal(res$lower[["p"]], 1)
  expect_equal(res$upper[["p"]], 7)

  expect_equal(res$lower[["sd_0"]], 0.5)
  expect_equal(res$upper[["sd_0"]], 3.2)

  # lower < upper
  expect_true(all(res$lower < res$upper))
})


test_that("get_lower_upper -> warns correctly (and warns can be toggled off)", {
  model <- dmc_dummy

  # define an unknown component
  weird_fun <- function(...) {}
  model$comp_funs$mu_fun <- weird_fun

  # with warn = TRUE, we should see a warning
  expect_warning(
    res <- get_lower_upper(model, warn = TRUE),
    regexp = "Cannot provide default values"
  )

  # result should still contain the known parameter
  expect_named(res, c("lower", "upper"))
  exp_names = setdiff(names(coef(model)), c("muc", "A", "tau", "a"))
  expect_equal(exp_names, names(res$lower))
  expect_equal(exp_names, names(res$upper))

  # no warning if warn = FALSE
  expect_no_warning(res2 <- get_lower_upper(model, warn = FALSE))
  expect_identical(res2, res)
})


test_that("get_lower_upper considers dx and dt", {
  model <- ratcliff_dm(var_non_dec = TRUE, var_start = TRUE, dx = .1, dt = 0.1)
  res <- get_lower_upper(model)

  expect_equal(res$lower[["range_non_dec"]], 0.105)
  expect_equal(res$upper[["range_non_dec"]], 0.4)

  expect_equal(res$lower[["range_start"]], 0.105)
  expect_equal(res$upper[["range_start"]], 1.5)
})
