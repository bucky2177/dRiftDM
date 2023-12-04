test_that("testing DMC", {
  a_dmc_model <- dmc_dm()

  # test the drift rate
  mu_t <- a_dmc_model$comp_funs$mu_fun(a_dmc_model, c(0.002, 0.2), "comp")
  expect_equal(round(mu_t, 5), c(10.14106, 3.81684))
  mu_t <- a_dmc_model$comp_funs$mu_fun(a_dmc_model, c(0.003, 0.3), "incomp")
  expect_equal(round(mu_t, 5), c(-1.83182, 4.02443))

  # test integral of the drift rate
  mu_t <- a_dmc_model$comp_funs$mu_int_fun(a_dmc_model, c(0.002, 0.2), "comp")
  expect_equal(round(mu_t, 6), c(0.020929, 0.809158))
  mu_t <- a_dmc_model$comp_funs$mu_int_fun(a_dmc_model, c(0.003, 0.3), "incomp")
  expect_equal(round(mu_t, 6), c(-0.006914, 1.198872))

  # test the drift rate with a != 2
  temp <- a_dmc_model
  temp$prms_model[["a"]] <- 2.1
  mu_t <- a_dmc_model$comp_funs$mu_fun(temp, c(0.002, 0.2), "comp")
  expect_equal(round(mu_t, 5), c(8.99534, 3.79313))
  mu_t <- a_dmc_model$comp_funs$mu_fun(temp, c(0.003, 0.3), "incomp")
  expect_equal(round(mu_t, 5), c(-0.91730, 4.02898))

  # test integral of the drift rate
  mu_t <- a_dmc_model$comp_funs$mu_int_fun(temp, c(0.002, 0.2), "comp")
  expect_equal(round(mu_t, 7), c(0.0175355, 0.810705))
  mu_t <- a_dmc_model$comp_funs$mu_int_fun(temp, c(0.003, 0.3), "incomp")
  expect_equal(round(mu_t, 6), c(-0.002527, 1.198627))


  # test the boundary
  b_t <- a_dmc_model$comp_funs$b_fun(a_dmc_model, c(0.002, 0.2), "comp")
  expect_equal(b_t, c(0.6, 0.6))
  dt_b_t <- a_dmc_model$comp_funs$dt_b_fun(a_dmc_model, c(0.002, 0.2), "comp")
  expect_equal(dt_b_t, c(0, 0))

  # test the starting condition
  x_seq <- seq(-1, 1, length.out = a_dmc_model$prms_solve[["nx"]] + 1)
  x_x <- a_dmc_model$comp_funs$x_fun(a_dmc_model, x_seq, "comp")
  x_seq <- seq(0, 1, length.out = a_dmc_model$prms_solve[["nx"]] + 1)
  d_x <- stats::dbeta(x_seq, 4, 4) / 2
  expect_identical(d_x, x_x)

  # test the non-decision time
  pdf_nt <- a_dmc_model$comp_funs$nt_fun(a_dmc_model, seq(0, 1, .005),
    one_cond = "comp"
  )
  pdf_test <- truncnorm::dtruncnorm(seq(0, 1, .005), a = 0, mean = 0.3, sd = .02)
  expect_equal(pdf_test, pdf_nt)

  #####
  # compare with kfe created by Thomas's python code
  a_dmc_model <- dmc_dm(t_max = 1000, dt = 5, dx = .1, sigma = 4)
  a_dmc_model <- set_model_prms(a_dmc_model, c(0.5, 75, 300, 30, 50, 20, 2))

  pdfs_comp <- calc_pdfs(a_dmc_model, "comp", "kfe")

  # get the pdfs from the python code....
  pdf_u_comp <- read.table(test_path("fixtures", "pdf_u_cong.txt"))$V1
  pdf_l_comp <- read.table(test_path("fixtures", "pdf_l_cong.txt"))$V1
  # convolute it correctly
  pdf_nt <- a_dmc_model$comp_funs$nt_fun(a_dmc_model,
    seq(0, 1000, 5),
    one_cond = "comp"
  )
  pdf_u_comp <- stats::convolve(pdf_nt, rev(pdf_u_comp)) * 5
  pdf_l_comp <- stats::convolve(pdf_nt, rev(pdf_l_comp)) * 5
  expect_equal(pdf_u_comp, pdfs_comp[[1]])
  expect_equal(pdf_l_comp, pdfs_comp[[2]])

  ###
  # compare solution in seconds with sigma = 1 and milliseconds with sigma = 4
  a_dmc_model <- dmc_dm(t_max = 1, dt = .005, dx = .1, sigma = 1)
  a_dmc_model <- set_model_prms(a_dmc_model, c(
    3.952847,
    0.5929271, 0.300, 0.03, 0.05,
    0.1581139, 2
  ))

  pdfs_comp_s <- calc_pdfs(a_dmc_model, "comp", "kfe")
  expect_true(all(abs(pdfs_comp_s[[1]] / 1000 - pdfs_comp[[1]]) < 1e-8))
  expect_true(all(abs(pdfs_comp_s[[2]] / 1000 - pdfs_comp[[2]]) < 1e-8))


  ## roughly compare with DMCfun # 1
  a_dmc_model <- dmc_dm()
  a_dmc_model <- set_model_prms(a_dmc_model, c(4, 0.6, 0.3, 0.02, 0.04, 0.1, 4))
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
  dmc_cafs <- calc_cafs(a_dmc_model, type = "pred")
  dmc_quants <- calc_quantiles(a_dmc_model, type = "pred")

  expect_true(
    all(abs(
      sim_data$delta$meanComp -
        dmc_quants$Quant_Corr[dmc_quants$Cond == "comp"] * 1000
    ) < 10)
  )

  expect_true(
    all(abs(
      sim_data$delta$meanIncomp -
        dmc_quants$Quant_Corr[dmc_quants$Cond == "incomp"] * 1000
    ) < 10)
  )

  expect_true(
    all(abs(
      sim_data$caf$accPerComp - dmc_cafs$P_Corr[dmc_cafs$Cond == "comp"]
    ) < .01)
  )

  expect_true(
    all(abs(
      sim_data$caf$accPerIncomp[2:5] -
        dmc_cafs$P_Corr[dmc_cafs$Cond == "incomp"][2:5]
    ) < .01)
  )

  expect_true(
    all(abs(
      sim_data$caf$accPerIncomp[1] -
        dmc_cafs$P_Corr[dmc_cafs$Cond == "incomp"][1]
    ) < .035)
  ) # kfe predict more fast errors in general



  ## roughly compare with DMCfun # 2
  a_dmc_model <- dmc_dm()
  a_dmc_model <- set_model_prms(a_dmc_model, c(4, 0.6, 0.3, 0.02, 0.04, 0, 4))
  funs <- get_default_functions()
  a_dmc_model$comp_funs$x_fun <- funs$x_fun # no starting point distribution
  sim_data <- DMCfun::dmcSim(
    drc = 0.5059644,
    bnds = 75.89466,
    amp = 0,
    tau = 40,
    resMean = 300,
    resSD = 20,
    spShape = 4,
    spDist = 0,
    spLim = c(-75.89466, 75.89466), setSeed = T,
    printInputArgs = F, printResults = F
  )
  dmc_cafs <- calc_cafs(a_dmc_model, type = "pred")
  dmc_quants <- calc_quantiles(a_dmc_model, type = "pred")

  expect_true(
    all(abs(
      sim_data$delta$meanComp -
        dmc_quants$Quant_Corr[dmc_quants$Cond == "comp"] * 1000
    ) < 10)
  )

  expect_true(
    all(abs(
      sim_data$delta$meanIncomp -
        dmc_quants$Quant_Corr[dmc_quants$Cond == "incomp"] * 1000
    ) < 10)
  )

  expect_true(
    all(abs(
      sim_data$caf$accPerComp - dmc_cafs$P_Corr[dmc_cafs$Cond == "comp"]
    ) < .01)
  )

  expect_true(
    all(abs(
      sim_data$caf$accPerIncomp[2:5] -
        dmc_cafs$P_Corr[dmc_cafs$Cond == "incomp"][2:5]
    ) < .01)
  )

  expect_true(
    all(abs(
      sim_data$caf$accPerIncomp[1] -
        dmc_cafs$P_Corr[dmc_cafs$Cond == "incomp"][1]
    ) < .03)
  ) # starting condition kicks in more strongly with kfe
})


test_that("ratcliff_simple works as expected", {
  a_model <- ratcliff_dm()

  # test the drift rate
  mu_t <- a_model$comp_funs$mu_fun(a_model, c(0.002, 0.2), "w")
  expect_equal(mu_t, c(3, 3))
  mu_t <- a_model$comp_funs$mu_int_fun(a_model, c(0.002, 0.2), "w")
  expect_equal(mu_t, c(3 * 0.002, 3 * 0.2))

  # test the boundary
  b_t <- a_model$comp_funs$b_fun(a_model, c(0.002, 0.2), "w")
  expect_equal(b_t, c(0.6, 0.6))
  dt_b_t <- a_model$comp_funs$dt_b_fun(a_model, c(0.002, 0.2), "w")
  expect_equal(dt_b_t, c(0, 0))

  # test the starting condition
  x_seq <- seq(-1, 1, length.out = a_model$prms_solve[["nx"]] + 1)
  x_x <- a_model$comp_funs$x_fun(a_model, x_seq, "w")
  d_x <- rep(0, a_model$prms_solve[["nx"]] + 1)
  d_x[(a_model$prms_solve[["nx"]] + 2) / 2] <- 1 / a_model$prms_solve[["dx"]]
  expect_identical(d_x, x_x)

  # test the non-decision time
  pdf_nt <- a_model$comp_funs$nt_fun(a_model, seq(0, 3, a_model$prms_solve[["dt"]]),
    one_cond = "comp"
  )
  pdf_test <- rep(0, a_model$prms_solve[["nt"]] + 1)


  pdf_test[0.3 / a_model$prms_solve[["dt"]] + 1] <- 1 / a_model$prms_solve[["dt"]]
  expect_equal(pdf_test, pdf_nt)

  pdfs <- calc_pdfs(a_model, "w", "kfe")
  pdfs[[1]] <- pdfs[[1]] / sum(pdfs[[1]])
  pdfs[[2]] <- pdfs[[2]] / sum(pdfs[[2]])
  expect_true(all(abs(pdfs[[1]] - pdfs[[2]]) < 0.001))



  # roughly compare with DMCfun
  sim_data <- DMCfun::dmcSim(
    drc = 0.3794733,
    bnds = 75.89466,
    amp = 0,
    tau = 40,
    resMean = 300,
    resSD = 0,
    resDist = 0,
    spShape = 4,
    spDist = 0,
    spLim = c(-75.89466, 75.89466), setSeed = T,
    printInputArgs = F, printResults = F
  )
  r_cafs <- calc_cafs(a_model, type = "pred")
  r_quants <- calc_quantiles(a_model, type = "pred")
  r_quants$Quant_Corr <- r_quants$Quant_Corr - 0.3

  expect_true(
    all(abs(
      sim_data$delta$meanComp -
        r_quants$Quant_Corr[r_quants$Cond == "null"] * 1000
    ) < 10)
  )


  expect_true(
    all(abs(
      sim_data$caf$accPerComp - r_cafs$P_Corr[r_cafs$Cond == "null"]
    ) < .01)
  )
})

test_that("model components fail as expected", {
  t_vec <- seq(0, 1, 0.01)
  x_vec <- seq(-1, 1, 0.01)

  # no ddm input
  a <- "foo"
  class(a) <- "bar"

  expect_error(
    mu_constant(a, t_vec = t_vec, one_cond = "NULL"), "not of type drift_dm"
  )
  expect_error(
    mu_int_constant(a, t_vec = t_vec, one_cond = "NULL"),
    "not of type drift_dm"
  )

  expect_error(
    b_constant(a, t_vec = t_vec, one_cond = "NULL"),
    "not of type drift_dm"
  )

  expect_error(
    nt_constant(a, t_vec = t_vec, one_cond = "NULL"),
    "not of type drift_dm"
  )

  expect_error(
    mu_dmc_dm(a, t_vec = t_vec, one_cond = "NULL"),
    "not of type drift_dm"
  )

  expect_error(
    mu_int_dmc_dm(a, t_vec = t_vec, one_cond = "NULL"),
    "not of type drift_dm"
  )

  expect_error(
    x_beta(a, x_vec = x_vec, one_cond = "NULL"),
    "not of type drift_dm"
  )

  expect_error(
    nt_truncated_normal(a, t_vec = t_vec, one_cond = "NULL"),
    "not of type drift_dm"
  )


  # wrong space parameter
  a_model <- ratcliff_dm()

  expect_error(
    mu_constant(a_model, t_vec = 1, one_cond = "NULL"),
    "not a vector"
  )
  expect_error(
    mu_int_constant(a_model, t_vec = 1, one_cond = "NULL"),
    "not a vector"
  )

  expect_error(
    b_constant(a_model, t_vec = 1, one_cond = "NULL"),
    "not a vector"
  )

  expect_error(
    nt_constant(a_model, t_vec = 1, one_cond = "NULL"),
    "not a vector"
  )

  a_model <- dmc_dm()

  expect_error(
    mu_dmc_dm(a_model, t_vec = 1, one_cond = "NULL"),
    "not a vector"
  )

  expect_error(
    mu_int_dmc_dm(a_model, t_vec = 1, one_cond = "NULL"),
    "not a vector"
  )

  expect_error(
    x_beta(a_model, x_vec = 1, one_cond = "NULL"),
    "not a vector"
  )

  expect_error(
    nt_truncated_normal(a_model, t_vec = 1, one_cond = "NULL"),
    "not a vector"
  )
})
