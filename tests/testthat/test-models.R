test_that("testing DMC", {

  # get the component values
  a_dmc_model <- dmc_dm()
  all_comps = comp_vals(a_dmc_model)

  a_dmc_model_im = a_dmc_model
  suppressWarnings(solver(a_dmc_model_im) <- "im_zero")
  all_comps_im = comp_vals(a_dmc_model_im)

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
  temp = all_comps_im
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
  a_dmc_model <- dmc_dm(instr = "a ~ => 2.1")
  all_comps = comp_vals(a_dmc_model)

  a_dmc_model_im = a_dmc_model
  # just to ensure comp_vals evaluates the integral
  suppressWarnings(solver(a_dmc_model_im) <- "im_zero")
  all_comps_im = comp_vals(a_dmc_model_im)

  # drift rate
  mu_t <- all_comps$comp$mu_vals[c(0.002, 0.2) / 0.001 + 1]
  expect_equal(round(mu_t, 5), c(8.99534, 3.79313))
  mu_t <- all_comps$incomp$mu_vals[c(0.003, 0.3) / 0.001 + 1]
  expect_equal(round(mu_t, 5), c(-0.91730, 4.02898))

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
  pdf_test <- truncnorm::dtruncnorm(seq(0, 3, .001), a = 0, mean = 0.3, sd = .02)
  pdf_test <- pdf_test / (sum(pdf_test) * 0.001)
  expect_equal(pdf_test, pdf_nt)


  ##########
  # test with different component functions
  a_dmc_model <- dmc_dm(var_non_dec = F, var_start = F)
  all_comps = comp_vals(a_dmc_model)

  # test start_vals
  expect_equal(all_comps$comp$x_vals, all_comps$incomp$x_vals)
  expect_equal(all_comps$comp$x_vals,
               x_dirac_0(NULL, a_dmc_model$prms_solve, seq(-1, 1, 0.001),
                         NULL, NULL))

  # test non_dec
  expect_equal(all_comps$comp$nt_vals, all_comps$incomp$nt_vals)
  expect_equal(all_comps$comp$nt_vals,
               nt_constant(c(non_dec = 0.3),
                           a_dmc_model$prms_solve,
                           seq(0, 3, 0.001),
                           NULL, NULL))




  #####
  # Test scaling
  a_dmc_model <- dmc_dm(t_max = 1000, dt = 5, dx = .1, sigma = 4)
  a_dmc_model <- modify_flex_prms(a_dmc_model, instr = "
                                  muc ~ => 0.5
                                  b ~ => 75
                                  non_dec ~ => 300
                                  sd_non_dec ~ => 30
                                  tau ~ => 50
                                  A ~ comp => 20
                                  alpha ~ => 2")
  pdfs_comp <- re_evaluate_model(a_dmc_model)$pdfs[["comp"]]


  ###
  # compare solution in seconds with sigma = 1 and milliseconds with sigma = 4
  a_dmc_model <- dmc_dm(t_max = 1, dt = .005, dx = .1, sigma = 1)
  a_dmc_model <- modify_flex_prms(a_dmc_model, instr = "
                                  muc ~ => 3.952847
                                  b ~ => 0.5929271
                                  non_dec ~ => 0.300
                                  sd_non_dec ~ => 0.03
                                  tau ~ => 0.05
                                  A ~ comp => 0.1581139
                                  alpha ~ => 2")

  pdfs_comp_s <- re_evaluate_model(a_dmc_model)$pdfs[["comp"]]
  expect_true(all(abs(pdfs_comp_s[[1]] / 1000 - pdfs_comp[[1]]) < 1e-8))
  expect_true(all(abs(pdfs_comp_s[[2]] / 1000 - pdfs_comp[[2]]) < 1e-8))


  ## roughly compare with DMCfun # 1
  a_dmc_model <- dmc_dm()
  a_dmc_model <- modify_flex_prms(a_dmc_model, instr = "
                                  muc ~ => 4
                                  b ~ => 0.6
                                  non_dec ~ => 0.3
                                  sd_non_dec ~ => 0.02
                                  tau ~ => 0.04
                                  A ~ comp => 0.1
                                  alpha ~ => 4")
  sim_data <- DMCfun::dmcSim(
    drc = 0.5059644,
    bnds = 75.89466,
    amp = 12.64911,
    tau = 40,
    resMean = 300,
    resSD = 20,
    spShape = 4,
    spDist = 1,
    spLim = c(-75.89466, 75.89466), setSeed = T,
    printInputArgs = F, printResults = F
  )
  dmc_cafs <- calc_stats(a_dmc_model, type = "cafs", source = "pred")
  dmc_quants <- calc_stats(a_dmc_model, type = "quantiles", source = "pred")

  expect_true(
    all(abs(
      sim_data$delta$meanComp -
        dmc_quants$Quant_corr[dmc_quants$Cond == "comp"] * 1000
    ) < 10)
  )

  expect_true(
    all(abs(
      sim_data$delta$meanIncomp -
        dmc_quants$Quant_corr[dmc_quants$Cond == "incomp"] * 1000
    ) < 10)
  )

  expect_true(
    all(abs(
      sim_data$caf$accPerComp - dmc_cafs$P_err[dmc_cafs$Cond == "comp"]
    ) < .01)
  )

  expect_true(
    all(abs(
      sim_data$caf$accPerIncomp[2:5] -
        dmc_cafs$P_err[dmc_cafs$Cond == "incomp"][2:5]
    ) < .01)
  )

  expect_true(
    all(abs(
      sim_data$caf$accPerIncomp[1] -
        dmc_cafs$P_err[dmc_cafs$Cond == "incomp"][1]
    ) < .035)
  ) # kfe predict more fast errors in general
})


test_that("ratcliff_simple works as expected", {
  a_model <- ratcliff_dm()

  # test the drift rate
  mu_t <- a_model$comp_funs$mu_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1,],
    prms_solve = a_model$prms_solve,
    t_vec = c(0.002, 0.2), one_cond = "w",
    ddm_opts = NULL
  )
  expect_equal(mu_t, c(3, 3))
  mu_t <- a_model$comp_funs$mu_int_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1,],
    prms_solve = a_model$prms_solve,
    t_vec = c(0.002, 0.2), one_cond = "w",
    ddm_opts = NULL
  )
  expect_equal(mu_t, c(3 * 0.002, 3 * 0.2))

  # test the boundary
  b_t <- a_model$comp_funs$b_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1,],
    prms_solve = a_model$prms_solve,
    t_vec = c(0.002, 0.2), one_cond = "w",
    ddm_opts = NULL
  )
  expect_equal(b_t, c(0.6, 0.6))
  dt_b_t <- a_model$comp_funs$dt_b_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1,],
    prms_solve = a_model$prms_solve,
    t_vec = c(0.002, 0.2), one_cond = "w",
    ddm_opts = NULL
  )
  expect_equal(dt_b_t, c(0, 0))

  # test the starting condition
  x_seq <- seq(-1, 1, length.out = a_model$prms_solve[["nx"]] + 1)
  x_x <- a_model$comp_funs$x_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1,],
    prms_solve = a_model$prms_solve,
    x_vec = x_seq, one_cond = "w",
    ddm_opts = NULL
  )
  d_x <- rep(0, a_model$prms_solve[["nx"]] + 1)
  d_x[(a_model$prms_solve[["nx"]] + 2) / 2] <- 1 / a_model$prms_solve[["dx"]]
  expect_identical(d_x, x_x)

  # test the non-decision time
  pdf_nt <- a_model$comp_funs$nt_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1,],
    prms_solve = a_model$prms_solve,
    t_vec = seq(0, 3, a_model$prms_solve[["dt"]]),
    one_cond = "w",
    ddm_opts = NULL
  )
  pdf_test <- rep(0, a_model$prms_solve[["nt"]] + 1)
  pdf_test[0.3 / a_model$prms_solve[["dt"]] + 1] <- 1 / a_model$prms_solve[["dt"]]
  expect_equal(pdf_test, pdf_nt)


  # equal accuracy?
  pdfs <- re_evaluate_model(a_model)$pdfs[["null"]]
  pdfs[[1]] <- pdfs[[1]] / sum(pdfs[[1]])
  pdfs[[2]] <- pdfs[[2]] / sum(pdfs[[2]])
  expect_true(all(abs(pdfs[[1]] - pdfs[[2]]) < 0.001))
})



test_that("ratcliff with var. in non-dec or start point works as expected", {
  a_model <- ratcliff_dm(var_non_dec = T)

  # test the drift rate
  mu_t <- a_model$comp_funs$mu_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1,],
    prms_solve = a_model$prms_solve,
    t_vec = c(0.002, 0.2), one_cond = "w",
    ddm_opts = NULL
  )
  expect_equal(mu_t, c(3, 3))
  mu_t <- a_model$comp_funs$mu_int_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1,],
    prms_solve = a_model$prms_solve,
    t_vec = c(0.002, 0.2), one_cond = "w",
    ddm_opts = NULL
  )
  expect_equal(mu_t, c(3 * 0.002, 3 * 0.2))

  # test the boundary
  b_t <- a_model$comp_funs$b_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1,],
    prms_solve = a_model$prms_solve,
    t_vec = c(0.002, 0.2), one_cond = "w",
    ddm_opts = NULL
  )
  expect_equal(b_t, c(0.6, 0.6))
  dt_b_t <- a_model$comp_funs$dt_b_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1,],
    prms_solve = a_model$prms_solve,
    t_vec = c(0.002, 0.2), one_cond = "w",
    ddm_opts = NULL
  )
  expect_equal(dt_b_t, c(0, 0))

  # test the starting condition
  x_seq <- seq(-1, 1, length.out = a_model$prms_solve[["nx"]] + 1)
  x_x <- a_model$comp_funs$x_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1,],
    prms_solve = a_model$prms_solve,
    x_vec = x_seq, one_cond = "w",
    ddm_opts = NULL
  )
  d_x <- rep(0, a_model$prms_solve[["nx"]] + 1)
  d_x[(a_model$prms_solve[["nx"]] + 2) / 2] <- 1 / a_model$prms_solve[["dx"]]
  expect_identical(d_x, x_x)

  # test the non-decision time
  pdf_nt <- a_model$comp_funs$nt_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1,],
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
  a_model <- ratcliff_dm(var_start = T)

  # test the drift rate
  mu_t <- a_model$comp_funs$mu_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1,],
    prms_solve = a_model$prms_solve,
    t_vec = c(0.002, 0.2), one_cond = "w",
    ddm_opts = NULL
  )
  expect_equal(mu_t, c(3, 3))
  mu_t <- a_model$comp_funs$mu_int_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1,],
    prms_solve = a_model$prms_solve,
    t_vec = c(0.002, 0.2), one_cond = "w",
    ddm_opts = NULL
  )
  expect_equal(mu_t, c(3 * 0.002, 3 * 0.2))

  # test the boundary
  b_t <- a_model$comp_funs$b_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1,],
    prms_solve = a_model$prms_solve,
    t_vec = c(0.002, 0.2), one_cond = "w",
    ddm_opts = NULL
  )
  expect_equal(b_t, c(0.6, 0.6))
  dt_b_t <- a_model$comp_funs$dt_b_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1,],
    prms_solve = a_model$prms_solve,
    t_vec = c(0.002, 0.2), one_cond = "w",
    ddm_opts = NULL
  )
  expect_equal(dt_b_t, c(0, 0))

  # test the starting condition
  x_seq <- seq(-1, 1, length.out = a_model$prms_solve[["nx"]] + 1)
  x_x <- a_model$comp_funs$x_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1,],
    prms_solve = a_model$prms_solve,
    x_vec = x_seq, one_cond = "w",
    ddm_opts = NULL
  )

  pdf_test <- dunif(
    x_seq, 0 - 0.05 / 2,
    0 + 0.05 / 2
  )
  expect_identical(pdf_test, x_x)

  # test the non-decision time
  pdf_nt <- a_model$comp_funs$nt_fun(
    prms_model = a_model$flex_prms_obj$prms_matrix[1,],
    prms_solve = a_model$prms_solve,
    t_vec = seq(0, 3, a_model$prms_solve[["dt"]]),
    one_cond = "w",
    ddm_opts = NULL
  )
  pdf_test <- rep(0, a_model$prms_solve[["nt"]] + 1)
  pdf_test[0.3 / a_model$prms_solve[["dt"]] + 1] <- 1 / a_model$prms_solve[["dt"]]
  expect_equal(pdf_test, pdf_nt)
})


test_that("SSP model provides reasonable values", {
  a_model = ssp_dm(dt = .005, dx = .005, instr = "
                   b ~ => 0.6
                   non_dec ~ => 0.3
                   sd_non_dec ~ => 0.01
                   p ~ => 3.3
                   sd_0 ~ => 1.2
                   r ~ => 10")

  x_vec = seq(-1, 1, .005)
  t_vec = seq(0, a_model$prms_solve[["t_max"]], .005)


  prms_model = a_model$flex_prms_obj$prms_matrix[1,]
  conds = a_model$conds
  prms_solve = a_model$prms_solve

  # b_fun
  expect_equal(
    a_model$comp_funs$b_fun(prms_model, prms_solve, t_vec, one_cond = NA,
                            ddm_opts = NULL),
    rep(0.6, length(t_vec))
  )

  expect_equal(
    a_model$comp_funs$b_fun(prms_model, prms_solve, t_vec, one_cond = NA,
                            ddm_opts = NULL),
    rep(0.6, length(t_vec))
  )

  # dt_b_fun
  expect_equal(
    a_model$comp_funs$dt_b_fun(prms_model, prms_solve, t_vec, one_cond = NA,
                               ddm_opts = NULL),
    rep(0, length(t_vec))
  )

  expect_equal(
    a_model$comp_funs$dt_b_fun(prms_model, prms_solve, t_vec, one_cond = NA,
                               ddm_opts = NULL),
    rep(0, length(t_vec))
  )

  # x_fun
  exp_x = rep(0, length(x_vec))
  exp_x[201] = 1 / .005
  expect_equal(
    a_model$comp_funs$x_fun(prms_model, prms_solve, x_vec, one_cond = NA,
                            ddm_opts = NULL),
    exp_x
  )

  expect_equal(
    a_model$comp_funs$x_fun(prms_model, prms_solve, x_vec, one_cond = NA,
                            ddm_opts = NULL),
    exp_x
  )


  # mu_fun
  sd_t = 1.2 - 10 * t_vec
  sd_t = pmax(sd_t, .001)
  a_tar = pnorm(q = 0.5, mean = 0, sd = sd_t) - pnorm(q = -0.5, mean = 0, sd = sd_t)
  a_fl = 1 - a_tar
  prms_model = a_model$flex_prms_obj$prms_matrix[1,]
  expect_equal(
    a_model$comp_funs$mu_fun(prms_model, prms_solve, t_vec,
                             one_cond = NA, ddm_opts = NULL),
    3.3 * a_tar + 3.3 * a_fl
  )
  prms_model = a_model$flex_prms_obj$prms_matrix[2,]
  expect_equal(
    a_model$comp_funs$mu_fun(prms_model, prms_solve, t_vec,
                             one_cond = NA, ddm_opts = NULL),
    3.3 * a_tar - 3.3 * a_fl
  )

  # mu_int_fun
  expect_error(
    a_model$comp_funs$mu_int_fun(prms_model, prms_solve, t_vec,
                                 one_cond = NA, ddm_opts = NULL),
    "this should not be called"
  )

  expect_error(
    a_model$comp_funs$mu_int_fun(prms_model, prms_solve, t_vec,
                                 one_cond = NA, ddm_opts = NULL),
    "this should not be called"
  )

  # nt_fun
  exp_nt <- truncnorm::dtruncnorm(t_vec, a = 0, mean = 0.3, sd = 0.01)
  expect_equal(
    a_model$comp_funs$nt_fun(prms_model = prms_model, prms_solve = prms_solve, t_vec = t_vec,
                             one_cond = NA, ddm_opts = NULL),
    exp_nt
  )
})










