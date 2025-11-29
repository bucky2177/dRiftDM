# HELPER FUNCTIONS REQUIRED FOR MCMC ALGORITHMS ---------------------------

test_that("full_crossover works as expected", {
  prms <- matrix(seq(1, 6, 1), nrow = 2, ncol = 3)
  pis <- c(-10, -5, 0)
  lls <- c(-100, -50, 0)

  # get the chains involved in the crossover
  withr::local_preserve_seed()
  set.seed(1) # reproduce random selection of chains
  selected_mn <- vapply(1:3, \(x) sample(c(1:3)[-x], 2), FUN.VALUE = rep(1L, 2))

  # Simulate acceptance: only second proposal gets accepted
  mock_call_log_posterior_m <- function(...) {
    list(
      pis = c(-20, 5, -10),
      lls = c(-200, 50, -100),
      accept = c(FALSE, TRUE, FALSE)
    )
  }

  # calculate the expected proposal
  exp <- prms[, 2] +
    (prms[, selected_mn[1, 2]] - prms[, selected_mn[2, 2]]) * 1.5

  with_mocked_bindings(
    call_log_posterior_m = mock_call_log_posterior_m,
    {
      withr::local_preserve_seed()
      set.seed(1) # same seed as above
      out <- full_crossover(prms, pis, lls, gamma = 1.5, b = 0.001)

      expect_named(
        out,
        c(
          "new_prms_across_chains",
          "new_pis_across_chains",
          "new_log_likes_across_chains"
        )
      )
      expect_type(out, "list")
      expect_equal(ncol(out$new_prms_across_chains), 3)

      # First column should be unchanged
      expect_equal(out$new_prms_across_chains[, 1], prms[, 1])
      # Second column should be changed
      diff_exp <- out$new_prms_across_chains[, 2] - exp
      expect_true(all(abs(diff_exp) <= 0.001))
      # Third column should be unchanged
      expect_equal(out$new_prms_across_chains[, 3], prms[, 3])

      # Log-posterior and likelihood updated correctly
      expect_equal(out$new_pis_across_chains, c(-10, 5, 0))
      expect_equal(out$new_log_likes_across_chains, c(-100, 50, 0))
    }
  )
})


test_that("full_crossover -> default gamma and non-default b work as expected ", {
  prms <- matrix(seq(1, 6, 1), nrow = 2, ncol = 3)
  pis <- c(-10, -5, 0)
  lls <- c(-100, -50, 0)

  # get the chains involved in the crossover
  withr::local_preserve_seed()
  set.seed(1) # reproduce random selection of chains
  selected_mn <- vapply(1:3, \(x) sample(c(1:3)[-x], 2), FUN.VALUE = rep(1L, 2))

  # Simulate acceptance: first and second proposal gets accepted
  mock_call_log_posterior_m <- function(...) {
    list(
      pis = c(-20, 5, -10),
      lls = c(-200, 50, -100),
      accept = c(TRUE, FALSE, TRUE)
    )
  }

  # calculate the expected proposals
  exp_1 <- prms[, 1] +
    (prms[, selected_mn[1, 1]] - prms[, selected_mn[2, 1]]) * 1.19
  exp_3 <- prms[, 3] +
    (prms[, selected_mn[1, 3]] - prms[, selected_mn[2, 3]]) * 1.19

  with_mocked_bindings(
    call_log_posterior_m = mock_call_log_posterior_m,
    {
      withr::local_preserve_seed()
      set.seed(1) # same seed as above
      out <- full_crossover(prms, pis, lls, b = 0.05)

      expect_named(
        out,
        c(
          "new_prms_across_chains",
          "new_pis_across_chains",
          "new_log_likes_across_chains"
        )
      )
      expect_type(out, "list")
      expect_equal(ncol(out$new_prms_across_chains), 3)

      # First column should be changed
      diff_exp_1 <- abs(out$new_prms_across_chains[, 1] - exp_1)
      # not ideal.. because noise values might still be smaller than 0.001
      # by chance, but works with this seed
      expect_true(all(diff_exp_1 > 0.001 & diff_exp_1 <= 0.05))

      # Second column should be unchanged
      expect_equal(out$new_prms_across_chains[, 2], prms[, 2])

      # Third column should be changed
      diff_exp_3 <- abs(out$new_prms_across_chains[, 3] - exp_3)
      # not ideal.. because noise values might still be smaller than 0.001
      # by chance, but works with this seed
      expect_true(all(diff_exp_3 > 0.001 & diff_exp_3 <= 0.05))

      # Log-posterior and likelihood updated correctly
      expect_equal(out$new_pis_across_chains, c(-20, -5, -10))
      expect_equal(out$new_log_likes_across_chains, c(-200, -50, -100))
    }
  )
})


test_that("migration_crossover works as expected", {
  withr::local_preserve_seed()
  set.seed(4) # seed was set to get selected_chains 3,1,2

  # define chains
  prms <- matrix(c(1, 1, 2, 2, 3, 3), nrow = 2) # 2 parameters, 3 chains (labeled)
  pis <- c(40, 20, 10)
  lls <- c(140, 120, 110)
  # => after cycling, proposal for 1 and 2 should be rejected, for 3 it
  # should be accepted

  # mock log_posterior_m (proposal_pi is just pis of which_chain)
  mock_call_log_posterior_m <-
    function(proposal_mat, prev_prms_mat, prev_pis, prev_lls, ...) {
      which_chain <- proposal_mat[1, ]
      proposal_pi <- pis[which_chain]
      proposal_ll <- lls[which_chain]
      accept <- proposal_pi > prev_pis
      prev_pis[accept] <- proposal_pi[accept]
      prev_lls[accept] <- proposal_ll[accept]
      return(
        list(pis = prev_pis, lls = prev_lls, accept = accept)
      )
    }

  with_mocked_bindings(
    call_log_posterior_m = mock_call_log_posterior_m,
    {
      result <- migration_crossover(
        prms_across_chains = prms,
        pis_across_chains = pis,
        log_likes_across_chains = lls,
        b = 0.0 # no noise to keep it deterministic
      )

      expect_type(result, "list")
      expect_named(
        result,
        c(
          "new_prms_across_chains",
          "new_pis_across_chains",
          "new_log_likes_across_chains"
        )
      )
      expect_equal(result$new_pis_across_chains, c(40, 20, 40))
      expect_equal(result$new_log_likes_across_chains, c(140, 120, 140))

      # At least one chain's parameters should have changed
      expect_identical(result$new_prms_across_chains, prms[, c(1, 2, 1)])
    },
  )
})


test_that("call_log_posterior_m accepts better proposals", {
  proposal_mat <- matrix(c(0.2, 0.05), nrow = 1)
  prev_mat <- matrix(c(0.1, 0.1), nrow = 1)
  prev_pis <- c(-10, -10)
  prev_lls <- c(-100, -100)

  mock_log_post <- function(thetas_one_subj_mat, ...) {
    list(
      posterior_vals = c(-5, -20),
      log_like_vals = c(-50, -200)
    )
  }

  with_mocked_bindings(
    log_posterior_lower = mock_log_post,
    {
      out <- call_log_posterior_m(
        proposal_mat = proposal_mat,
        prev_prms_mat = prev_mat,
        prev_pis = prev_pis,
        prev_lls = prev_lls,
        level = "lower",
        re_eval = FALSE
      )

      expect_type(out, "list")
      expect_named(out, c("pis", "lls", "accept"))
      expect_equal(length(out$accept), 2)
      expect_true(out$accept[1]) # proposal > prev = more likely
      expect_false(out$accept[2]) # worse proposal should be rejected
      expect_equal(out$pis, c(-5, -10))
      expect_equal(out$lls, c(-50, -100))
    }
  )
})


test_that("call_log_posterior_m sometimes accepts worse proposals", {
  proposal_mat <- matrix(NA, nrow = 1, ncol = 5000)
  prev_mat <- matrix(1, nrow = 1, ncol = 5000)
  prev_pis <- rep(-log(0.5), 5000)
  prev_lls <- rep(-10, 5000)

  mock_log_post <- function(thetas_one_subj_mat, ...) {
    list(
      posterior_vals = rep(0, ncol(thetas_one_subj_mat)),
      log_like_vals = rep(-5, ncol(thetas_one_subj_mat))
    )
  }
  # => results in proposal - previous = log(0.5) and thus 50% acceptance rate

  withr::local_preserve_seed()
  set.seed(1)
  with_mocked_bindings(
    log_posterior_lower = mock_log_post,
    {
      out <- call_log_posterior_m(
        proposal_mat = proposal_mat,
        prev_prms_mat = prev_mat,
        prev_pis = prev_pis,
        prev_lls = prev_lls,
        level = "lower",
        re_eval = FALSE
      )

      expect_named(out, c("pis", "lls", "accept"))
      expect_true(abs(0.5 - mean(out$accept)) < 0.02)
    }
  )
})


test_that("call_log_posterior_m -> re_eval = TRUE for non-hierarchical case", {
  proposal_mat <- matrix(c(0.2, 0.05), nrow = 1)
  prev_mat <- matrix(c(0.1, 0.1), nrow = 1)
  prev_pis <- c(999, 999) # dummy values that should be overwritten
  prev_lls <- c(999, 999)

  withr::local_preserve_seed()
  set.seed(1)

  was_called <- 0
  mock_log_post <- function(thetas_one_subj_mat, ...) {
    was_called <<- was_called + 1
    if (all(thetas_one_subj_mat == prev_mat)) {
      return(list(posterior_vals = c(8, 8), log_like_vals = c(80, 80)))
    } else {
      return(list(posterior_vals = c(-10, 20), log_like_vals = c(-50, 200)))
    }
  }

  with_mocked_bindings(
    log_posterior_lower = mock_log_post,
    {
      out <- call_log_posterior_m(
        proposal_mat = proposal_mat,
        prev_prms_mat = prev_mat,
        prev_pis = prev_pis,
        prev_lls = prev_lls,
        level = "lower",
        re_eval = TRUE
      )

      # Test acceptance based on new `prev_pis = -8`
      expect_false(out$accept[1]) # -10 < 8
      expect_true(out$accept[2]) # 20 > 8

      # Test updated pis/lls reflect acceptances and re-eval
      expect_equal(out$pis, c(8, 20))
      expect_equal(out$lls, c(80, 200))

      # should be called twice
      expect_equal(was_called, 2)
    }
  )
})


test_that("call_log_posterior_m -> re_eval = TRUE for hierarchical case", {
  proposal_mat <- matrix(c(0.2, 0.05), nrow = 1)
  prev_mat <- matrix(c(0.1, 0.1), nrow = 1)
  prev_pis <- c(999, 999) # dummy values that should be overwritten
  prev_lls <- c(999, 999)

  withr::local_preserve_seed()
  set.seed(1)

  was_called <- 0
  mock_log_post <- function(phi_j_mat, ...) {
    was_called <<- was_called + 1
    if (all(phi_j_mat == prev_mat)) {
      return(list(posterior_vals = c(8, 8), log_like_vals = c(80, 80)))
    } else {
      return(list(posterior_vals = c(-10, 20), log_like_vals = c(-50, 200)))
    }
  }

  with_mocked_bindings(
    log_posterior_hyper = mock_log_post,
    {
      out <- call_log_posterior_m(
        proposal_mat = proposal_mat,
        prev_prms_mat = prev_mat,
        prev_pis = prev_pis,
        prev_lls = prev_lls,
        level = "hyper",
        re_eval = TRUE
      )

      # Test acceptance based on new `prev_pis = -8`
      expect_false(out$accept[1]) # -10 < 8
      expect_true(out$accept[2]) # 20 > 8

      # Test updated pis/lls reflect acceptances and re-eval
      expect_equal(out$pis, c(8, 20))
      expect_equal(out$lls, c(80, 200))

      # should be called twice
      expect_equal(was_called, 2)
    }
  )
})


test_that("crossover dispatches correctly", {
  fake_full_crossover <- function(...) "full called"
  fake_migration_crossover <- function(...) "migration called"

  with_mocked_bindings(
    full_crossover = fake_full_crossover,
    {
      result <- crossover("diff")
      expect_equal(result, "full called")
    }
  )

  with_mocked_bindings(
    full_crossover = fake_migration_crossover,
    {
      result <- crossover("diff")
      expect_equal(result, "migration called")
    }
  )
})

# HELPER FUNCTIONS REQUIRED FOR MCMC --------------------------------------

test_that("log_posterior_hyper works as expected", {
  withr::local_preserve_seed()
  set.seed(1)

  phi_j_mat <- matrix(rnorm(6, 1, 1), nrow = 2, ncol = 3) # 3 chains
  theta_j_mat <- matrix(rnorm(12, 1, 1), nrow = 3, ncol = 4) # 3 chains x 4 subjects
  rownames(phi_j_mat) <- c("mean", "sd")
  temperatures <- c(1, 2, 3)

  # Mock prior for individuals: log density = -1 per individual
  log_prior_lower_fun <- purrr::partial(dnorm, log = TRUE)

  # Mock prior for hyperparameters: log density = -2 per chain
  log_prior_hyper_fun <- function(x) {
    dnorm(x[1, ], mean = 1, sd = 1, log = TRUE) +
      dgamma(x[2, ], shape = 1, rate = 1, log = TRUE)
  }

  out <- log_posterior_hyper(
    phi_j_mat = phi_j_mat,
    theta_j_mat = theta_j_mat,
    log_prior_lower_fun = log_prior_lower_fun,
    log_prior_hyper_fun = log_prior_hyper_fun,
    temperatures = temperatures
  )

  # calculate the expected values
  log_likes <- dnorm(
    theta_j_mat,
    mean = phi_j_mat[1, ],
    sd = phi_j_mat[2, ],
    log = TRUE
  )
  log_likes <- rowSums(log_likes) * temperatures
  posterior <- log_likes + log_prior_hyper_fun(phi_j_mat)

  expect_named(out, c("posterior_vals", "log_like_vals"))
  expect_equal(length(out$posterior_vals), 3)
  expect_equal(length(out$log_like_vals), 3)
  expect_equal(out$posterior_vals, posterior)
  expect_equal(out$log_like_vals, log_likes)
})


test_that("log_posterior_lower -> works in the hierarchical case", {
  withr::local_preserve_seed()
  set.seed(1)
  model_subj <- ratcliff_dummy
  prms_solve(model_subj)[c("t_max", "dx", "dt")] <- c(1, 0.05, 0.01)

  # create matrices
  thetas <- matrix(
    c(2, 3, 4, 0.4, 0.5, 0.6, 0.2, 0.3, 0.4),
    nrow = 3,
    byrow = TRUE
  )
  rownames(thetas) <- c("muc", "b", "non_dec")
  phis <- matrix(
    c(
      3,
      4,
      5,
      0.5,
      0.6,
      0.7,
      0.3,
      0.4,
      0.5, # hyper-means
      1,
      2,
      3,
      0.1,
      0.2,
      0.3,
      0.2,
      0.3,
      0.4
    ), # hyper-sds
    nrow = 6,
    byrow = TRUE,
  )
  rownames(phis) <- c("M-muc", "M-b", "M-non_dec", "S-muc", "S-b", "S-non_dec")
  temperatures <- c(1, 2, 3)

  # and the prior functions
  prior <- get_default_prior_settings(
    drift_dm_obj = model_subj,
    level = "lower"
  )
  prior <- prior$log_dens_priors

  # get the likelihoods
  lls <- sapply(1:3, \(x) {
    coef(model_subj) <- thetas[, x]
    -re_evaluate_model(model_subj)$cost_value
  })

  # get the log-posteriors
  d_prior_1 <- dtnorm(
    x = thetas[1, ],
    mean = phis[1, ],
    sd = phis[4, ],
    log = TRUE
  )
  d_prior_2 <- dtnorm(
    x = thetas[2, ],
    mean = phis[2, ],
    sd = phis[5, ],
    log = TRUE
  )
  d_prior_3 <- dtnorm(
    x = thetas[3, ],
    mean = phis[3, ],
    sd = phis[6, ],
    log = TRUE
  )
  d_prior <- rowSums(cbind(d_prior_1, d_prior_2, d_prior_3))

  # calculate the expected value
  posteriors <- lls * temperatures + d_prior

  # call the function
  result <- log_posterior_lower(
    thetas_one_subj_mat = thetas,
    all_phis_mat = phis,
    model_subj = model_subj,
    log_prior_lower_funs = prior,
    temperatures = temperatures,
    suppress_warnings = TRUE
  )

  # test expectations
  expect_named(result, c("posterior_vals", "log_like_vals"))
  expect_equal(lls, result$log_like_vals)
  expect_equal(posteriors, result$posterior_vals)
})


test_that("log_posterior_lower -> works in the non-hierarchical case", {
  withr::local_preserve_seed()
  set.seed(1)
  model_subj <- ratcliff_dummy
  prms_solve(model_subj)[c("t_max", "dx", "dt")] <- c(1, 0.05, 0.01)

  # create matrices
  thetas <- matrix(
    c(2, 3, 4, 0.4, 0.5, 0.6, 0.2, 0.3, 0.4),
    nrow = 3,
    byrow = TRUE
  )
  rownames(thetas) <- c("muc", "b", "non_dec")
  temperatures <- c(1, 2, 3)

  # and the prior functions
  prior <- get_default_prior_settings(
    drift_dm_obj = model_subj,
    level = "none",
    means = c(non_dec = 0.2),
    lower = c(non_dec = 0.1),
    upper = c(non_dec = 1)
  )
  prior <- prior$log_dens_priors

  # get the likelihoods
  lls <- sapply(1:3, \(x) {
    coef(model_subj) <- thetas[, x]
    -re_evaluate_model(model_subj)$cost_value
  })

  # get the log-posteriors
  muc <- coef(model_subj)[["muc"]]
  b <- coef(model_subj)[["b"]]

  d_prior_1 <- dtnorm(x = thetas[1, ], mean = muc, sd = muc, log = TRUE)
  d_prior_2 <- dtnorm(x = thetas[2, ], mean = b, sd = b, log = TRUE)
  d_prior_3 <- dtnorm(
    x = thetas[3, ],
    mean = 0.2,
    sd = 0.2,
    lower = 0.1,
    upper = 1,
    log = TRUE
  )
  d_prior <- rowSums(cbind(d_prior_1, d_prior_2, d_prior_3))

  # calculate the expected value
  posteriors <- lls * temperatures + d_prior

  # call the function
  result <- log_posterior_lower(
    thetas_one_subj_mat = thetas,
    all_phis_mat = NULL,
    model_subj = model_subj,
    log_prior_lower_funs = prior,
    temperatures = temperatures,
    suppress_warnings = TRUE
  )

  # test expectations
  expect_named(result, c("posterior_vals", "log_like_vals"))
  expect_equal(lls, result$log_like_vals)
  expect_equal(posteriors, result$posterior_vals)
})


# CUSTOM DISTRIBUTIONS ----------------------------------------------------

test_that("dtnorm works as expected", {
  test_trunc <- truncnorm::dtruncnorm

  # compare density values with dtruncnorm
  x <- seq(0, 1, 0.01)
  y_vals <- dtnorm(x = x, mean = 0.5, sd = 0.2, lower = 0, upper = 0.9)
  exp <- test_trunc(x = x, a = 0, b = 0.9, mean = 0.5, sd = 0.2)
  expect_equal(y_vals, exp)

  # works with log as well?
  y_vals <- dtnorm(
    x = x,
    mean = 0.5,
    sd = 0.2,
    lower = 0,
    upper = 0.9,
    log = TRUE
  )
  exp <- log(test_trunc(x = x, a = 0, b = 0.9, mean = 0.5, sd = 0.2))
  expect_equal(y_vals, exp)

  # expect zeros and Infs when lower > upper
  expect_equal(dtnorm(x, lower = Inf, upper = -Inf), rep(0, length(x)))
  expect_equal(
    suppressWarnings(dtnorm(x, lower = Inf, upper = -Inf, log = TRUE)),
    rep(-Inf, length(x))
  )

  # test for correct parameter recycling (for correct usage in
  # log_posterior_hyper)
  mat <- matrix(seq(0, 1, length.out = 20), nrow = 5, ncol = 4)
  means <- seq(0.3, 0.7, length.out = 5)
  sds <- seq(0.1, 0.5, length.out = 5)

  result <- dtnorm(mat, mean = means, sd = sds, lower = 0, upper = 1)
  expect_equal(dim(result), dim(mat))
  exps <- vapply(
    seq_len(nrow(mat)),
    \(i) {
      test_trunc(x = mat[i, ], a = 0, b = 1, mean = means[i], sd = sds[i])
    },
    FUN.VALUE = numeric(ncol(mat))
  )
  expect_equal(result, t(exps))
})

test_that("dtnorm handles boundary cases, extreme values, and NA values", {
  # NA and Infs as input
  x <- c(-Inf, 0, 0.5, 1, Inf, NA)

  y <- dtnorm(x, mean = 0.5, sd = 0.2, lower = 0, upper = 1)
  expect_true(is.na(y[6]))
  expect_equal(y[1], 0)
  expect_equal(y[5], 0)

  y <- dtnorm(x, mean = 0.5, sd = 0.2, lower = 0, upper = 1, log = TRUE)
  expect_true(is.na(y[6]))
  expect_equal(y[1], -Inf)
  expect_equal(y[5], -Inf)

  # boundary cases
  x <- c(-1, 0, 0.5, 1, 2)
  res <- dtnorm(x, mean = 0.5, sd = 1, lower = 0, upper = 1, log = TRUE)
  expect_equal(res[1], -Inf)
  expect_equal(res[5], -Inf)
  expect_true(all(is.finite(res[2:4])))

  x <- c(0.1, 0.5, 0.9)
  expect_equal(dtnorm(x, lower = 1, upper = 1), rep(0, 3))
  expect_equal(dtnorm(x, lower = 1, upper = 1, log = TRUE), rep(-Inf, 3))
  expect_equal(dtnorm(x, lower = 0.5, upper = 0.5), c(0, Inf, 0))
})

# rtnorm is already used and tested alongside simulate_values

# PRIOR FUNCTIONS (DENSITIES AND RANDOM GENERATION) -----------------------

test_that("d_default_prior_hyper -> works as expected", {
  # Input values (vector)
  x <- c(0.5, 1.5)
  mean <- 0.2
  sd <- 1
  lower <- 0
  upper <- Inf
  shape <- 2
  rate <- 1

  # obtained output
  log_val <- d_default_prior_hyper(
    x = x,
    mean = mean,
    sd = sd,
    lower = lower,
    upper = upper,
    shape = shape,
    rate = rate,
    log = TRUE
  )
  nonlog_val <- d_default_prior_hyper(
    x = x,
    mean = mean,
    sd = sd,
    lower = lower,
    upper = upper,
    shape = shape,
    rate = rate,
    log = FALSE
  )

  # test - log
  exp_log <- dtnorm(
    x[1],
    mean = mean,
    sd = sd,
    lower = lower,
    upper = upper,
    log = TRUE
  ) +
    stats::dgamma(x = x[2], shape = shape, rate = rate, log = TRUE)
  expect_identical(log_val, exp_log)

  # test - nonlog
  exp_nonlog <- dtnorm(
    x[1],
    mean = mean,
    sd = sd,
    lower = lower,
    upper = upper
  ) *
    stats::dgamma(
      x = x[2],
      shape = shape,
      rate = rate
    )
  expect_identical(nonlog_val, exp_nonlog)

  # test matrix input
  x_mat <- matrix(c(0.5, 1.5, 0.75, 1.25), nrow = 2) # [mean; sd]
  val_1 <- d_default_prior_hyper(
    x = c(0.5, 1.5),
    mean = 0.2,
    sd = 1,
    lower = 0,
    upper = Inf,
    shape = 2,
    rate = 1,
    log = TRUE
  )
  val_2 <- d_default_prior_hyper(
    x = c(0.75, 1.25),
    mean = 0.3,
    sd = 2,
    lower = -5,
    upper = 5,
    shape = 3,
    rate = 2,
    log = TRUE
  )
  val_mat <- d_default_prior_hyper(
    x = x_mat,
    mean = c(0.2, 0.3),
    sd = c(1, 2),
    lower = c(0, -5),
    upper = c(Inf, 5),
    shape = c(2, 3),
    rate = c(1, 2),
    log = TRUE
  )
  expect_identical(val_mat, c(val_1, val_2))

  # test wrong input
  expect_error(
    d_default_prior_hyper(
      x = c(1),
      mean = 0,
      sd = 1,
      lower = 0,
      upper = 1,
      shape = 2,
      rate = 1,
      log = FALSE
    )
  )
  mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)
  expect_error(
    d_default_prior_hyper(
      x = mat,
      mean = 0,
      sd = 1,
      lower = 0,
      upper = 1,
      shape = 2,
      rate = 1,
      log = FALSE
    )
  )
})

test_that("d_default_prior_hyper handles extreme and NA values", {
  x_bad <- c(0.5, NA)
  val <- d_default_prior_hyper(
    x = x_bad,
    mean = 0.2,
    sd = 1,
    lower = 0,
    upper = Inf,
    shape = 2,
    rate = 1,
    log = TRUE
  )
  expect_true(is.na(val))

  x_neg <- c(0.5, -1)
  val_neg <- d_default_prior_hyper(
    x = x_neg,
    mean = 0.2,
    sd = 1,
    lower = 0,
    upper = Inf,
    shape = 2,
    rate = 1,
    log = TRUE
  )
  expect_equal(val_neg, -Inf)
})


test_that("r_default_prior_hyper -> works as expected", {
  withr::local_preserve_seed()
  set.seed(456)
  n <- 1000
  lower <- 0
  upper <- 1
  shape <- 2
  rate <- 1
  mean <- 0.5
  sd <- 0.25

  out <- r_default_prior_hyper(
    n = n,
    mean = mean,
    sd = sd,
    lower = lower,
    upper = upper,
    shape = shape,
    rate = rate
  )

  expect_true(all(out[1, ] >= lower & out[1, ] <= upper))
  expect_true(all(out[2, ] > 0)) # rgamma always positive
  expect_true(all(rownames(out) == c("r_m", "r_sd")))
  expect_true(is.matrix(out))
  expect_equal(dim(out), c(2, n))

  # quantiles should match roughly
  exp_quants <- qgamma(c(0.25, 0.5, 0.75), shape = shape, rate = rate)
  obs_quants <- quantile(out[2, ], probs = c(0.25, 0.5, 0.75))
  obs_quants <- unname(obs_quants)
  expect_equal(obs_quants, exp_quants, tolerance = 0.1)

  # quantiles should match roughly
  exp_quants <- truncnorm::qtruncnorm(
    c(0.25, 0.5, 0.75),
    a = lower,
    b = upper,
    mean = mean,
    sd = sd
  )
  obs_quants <- quantile(out[1, ], probs = c(0.25, 0.5, 0.75))
  obs_quants <- unname(obs_quants)
  expect_equal(obs_quants, exp_quants, tolerance = 0.1)

  # check if return value is true for n = 1
  out_vec <- r_default_prior_hyper(
    n = 1,
    mean = mean,
    sd = sd,
    lower = lower,
    upper = upper,
    shape = shape,
    rate = rate
  )
  expect_true(all(names(out_vec) == c("r_m", "r_sd")))
  expect_length(out_vec, 2)
  expect_true(is.vector(out_vec))

  # check failure for n < 1
  expect_error(
    r_default_prior_hyper(
      n = 0,
      mean = mean,
      sd = sd,
      lower = lower,
      upper = upper,
      shape = shape,
      rate = rate
    )
  )
})

test_that("get_default_prior_settings -> returns correct structure", {
  drift_dm_obj <- ratcliff_dummy
  mean <- c(muc = 0.5, b = 0.6)

  out <- get_default_prior_settings(
    drift_dm_obj,
    level = "none",
    means = mean,
    sd = mean
  )
  expect_s3_class(out, "ddm_prior_settings")
  expect_named(out, c("log_dens_priors", "r_priors"))
  expect_equal(attr(out, "level"), "none")
  expect_length(out$log_dens_priors, 3)
  expect_length(out$r_priors, 3)

  out <- get_default_prior_settings(
    drift_dm_obj,
    level = "hyper",
    means = mean,
    sd = mean
  )
  expect_equal(attr(out, "level"), "hyper")

  out <- get_default_prior_settings(
    drift_dm_obj,
    level = "lower",
    means = mean,
    sd = mean
  )
  expect_equal(attr(out, "level"), "lower")
})

test_that("get_default_prior_settings -> creates working functions", {
  drift_dm_obj <- ratcliff_dummy
  mean <- c(muc = 1)
  sd <- c(muc = 0)

  #####
  # checks for none
  out <- get_default_prior_settings(
    drift_dm_obj,
    level = "none",
    means = mean,
    sds = sd
  )

  # Should return a log-density and random sampler function
  expect_type(out$log_dens_priors$muc, "closure")
  expect_type(out$r_priors$non_dec, "closure")

  # Should return expected value
  expect_equal(out$log_dens_priors$muc(c(0, 1)), c(-Inf, Inf))
  expect_equal(out$r_priors$muc(1), 1)

  # should return expected dimension
  expect_length(out$log_dens_priors$non_dec(c(0, 1)), 2)
  expect_length(out$r_priors$non_dec(1), 1)

  ###
  # checks for hyper
  out <- get_default_prior_settings(
    drift_dm_obj,
    level = "hyper",
    means = mean,
    sds = sd
  )

  # should return expected dimension
  expect_length(out$log_dens_priors$non_dec(c(0, 1)), 1)
  expect_length(out$r_priors$non_dec(1), 2)

  ###
  # checks for lower
  out <- get_default_prior_settings(
    drift_dm_obj,
    level = "lower",
    means = mean,
    sds = sd
  )

  # should return expected dimension
  expect_length(out$log_dens_priors$non_dec(c(0, 1)), 2)
  expect_length(out$r_priors$non_dec(1), 1)
})

test_that("get_default_prior_settings -> returns correct density values", {
  # Create dummy drift_dm_obj (not used in this path)
  drift_dm_obj <- ratcliff_dummy

  coef(drift_dm_obj)["b"] <- 0.5
  mean <- c(muc = 3)
  sd <- c(muc = 2)
  lower <- c(muc = 1)
  upper <- c(muc = 5)
  shape <- c(muc = 1)
  rate <- c(muc = 2)

  ### Test none
  # Get prior settings
  prior <- get_default_prior_settings(
    drift_dm_obj = drift_dm_obj,
    level = "none",
    means = mean,
    sds = sd,
    lower = lower,
    upper = upper
  )

  # Manually compute expected density
  expected_log_density_b <- dtnorm(
    0.4,
    mean = 0.5,
    sd = 0.5,
    lower = -Inf,
    upper = Inf,
    log = TRUE
  )
  expected_log_density_muc <- dtnorm(
    3.5,
    mean = 3,
    sd = 2,
    lower = 1,
    upper = 5,
    log = TRUE
  )
  # Compare with the generated function
  actual_log_density_b <- prior$log_dens_priors$b(0.4)
  actual_log_density_muc <- prior$log_dens_priors$muc(3.5)

  expect_equal(actual_log_density_b, expected_log_density_b)
  expect_equal(actual_log_density_muc, expected_log_density_muc)

  ### Test hyper
  # Get prior settings
  prior <- get_default_prior_settings(
    drift_dm_obj = drift_dm_obj,
    level = "hyper",
    means = mean,
    sds = sd,
    lower = lower,
    upper = upper,
    shape = shape,
    rate = rate
  )

  # Manually compute expected density
  expected_log_density_b <- d_default_prior_hyper(
    c(0.4, 1),
    mean = 0.5,
    sd = 0.5,
    lower = -Inf,
    upper = Inf,
    shape = 1,
    rate = 1,
    log = TRUE
  )
  expected_log_density_muc <- d_default_prior_hyper(
    c(3.5, 2),
    mean = 3,
    sd = 2,
    lower = 1,
    upper = 5,
    shape = 1,
    rate = 2,
    log = TRUE
  )
  # Compare with the generated function
  actual_log_density_b <- prior$log_dens_priors$b(c(0.4, 1))
  actual_log_density_muc <- prior$log_dens_priors$muc(c(3.5, 2))

  expect_equal(actual_log_density_b, expected_log_density_b)
  expect_equal(actual_log_density_muc, expected_log_density_muc)

  ### Test hyper
  # Get prior settings
  prior <- get_default_prior_settings(
    drift_dm_obj = drift_dm_obj,
    level = "lower",
    means = mean,
    sds = sd,
    lower = lower,
    upper = upper,
    shapes = shape,
    rates = rate
  )

  # Manually compute expected density
  expected_log_density_b <- dtnorm(
    c(0.4),
    mean = 0.5,
    sd = 0.5,
    lower = -Inf,
    upper = Inf,
    log = TRUE
  )
  expected_log_density_muc <- dtnorm(
    3.5,
    mean = 3,
    sd = 2,
    lower = 1,
    upper = 5,
    log = TRUE
  )

  # Compare with the generated function
  actual_log_density_b <- prior$log_dens_priors$b(0.4, mean = 0.5, sd = 0.5)
  actual_log_density_muc <- prior$log_dens_priors$muc(3.5, mean = 3, sd = 2)

  expect_equal(actual_log_density_b, expected_log_density_b)
  expect_equal(actual_log_density_muc, expected_log_density_muc)
})


# ESTIMATION FUNCTIONS ----------------------------------------------------

test_that("estimate_bayes_one_subj runs and returns correct structure", {
  # minimal test example to ensure that the function runs and returns
  # the expected object
  some_model <- ratcliff_dummy
  prms_solve(some_model)[c("dx", "dt", "t_max")] <- c(0.01, 0.01, 1)
  withr::local_seed(123)
  sink(file = file.path(tempdir(), "capture.txt"))
  result <- estimate_bayes_one_subj(
    drift_dm_obj = some_model,
    sampler = "DE-MCMC",
    n_chains = 3,
    burn_in = 1,
    samples = 2,
    prob_migration = 0.1,
    prob_re_eval = 0.1,
    verbose = 0
  )
  sink()

  # check the returned list
  expect_type(result, "list")
  expect_named(result, c("theta", "pis_theta", "lls_theta"))
  expect_equal(attr(result, "data_model"), some_model)

  # Check dimensions and naming of theta
  expect_equal(dim(result$theta), c(3, 3, 2))
  expect_equal(dimnames(result$theta)[[1]], names(coef(some_model)))
  expect_equal(dimnames(result$theta)[[2]], as.character(1:3))
  expect_equal(dimnames(result$theta)[[3]], as.character(3:4))

  # Check dimensions and naming of pis
  expect_equal(dim(result$pis_theta), c(3, 2))
  expect_equal(dimnames(result$pis_theta)[[1]], as.character(1:3))
  expect_equal(dimnames(result$pis_theta)[[2]], as.character(3:4))

  # Check dimensions and naming of pis
  expect_equal(dim(result$lls_theta), c(3, 2))
  expect_equal(dimnames(result$lls_theta)[[1]], as.character(1:3))
  expect_equal(dimnames(result$lls_theta)[[2]], as.character(3:4))
})


test_that("estimate_bayes_h runs and returns correct structure", {
  # minimal test example to ensure the function runs and returns
  # the expected object
  some_model <- ratcliff_dummy
  prms_solve(some_model)[c("dx", "dt", "t_max")] <- c(0.01, 0.01, 1)

  # create minimal hierarchical structure by duplicating data
  d <- ratcliff_synth_data
  obs_data_ids <- rbind(
    cbind(d, ID = 10),
    cbind(d, ID = 2)
  )

  sink(file = file.path(tempdir(), "capture.txt"))
  result <- estimate_bayes_h(
    drift_dm_obj = some_model,
    obs_data_ids = obs_data_ids,
    sampler = "DE-MCMC",
    n_chains = 3,
    burn_in = 1,
    samples = 2,
    n_cores = 1,
    prob_migration = 0.1,
    prob_re_eval = 0.1,
    verbose = 0
  )
  sink()

  # check the returned list
  expect_type(result, "list")
  expect_named(
    result,
    c("phi", "pis_phi", "lls_phi", "theta", "pis_theta", "lls_theta")
  )

  # check that the data_model attribute is a named list of models
  expect_true(is.list(attr(result, "data_model")))
  expect_equal(names(attr(result, "data_model")), c("10", "2"))
  obs_data(some_model) <- obs_data_ids[obs_data_ids$ID == 10, ]
  expect_equal(attr(result, "data_model")[[1]], some_model)
  obs_data(some_model) <- obs_data_ids[obs_data_ids$ID == 2, ]
  expect_equal(attr(result, "data_model")[[2]], some_model)

  # Check dimensions of phi array
  expect_equal(dim(result$phi), c(6, 3, 2))
  higher_prms <- paste0(c("M-", "S-"), rep(names(coef(some_model)), each = 2))
  expect_equal(dimnames(result$phi)[[1]], higher_prms)
  expect_equal(dimnames(result$phi)[[2]], as.character(1:3))
  expect_equal(dimnames(result$phi)[[3]], as.character(3:4))

  # Check pis_phi and lls_phi dimensions
  expect_equal(dim(result$pis_phi), c(3, 3, 2))
  expect_equal(dimnames(result$pis_phi)[[1]], names(coef(some_model)))
  expect_equal(dimnames(result$pis_phi)[[2]], as.character(1:3))
  expect_equal(dimnames(result$pis_phi)[[3]], as.character(3:4))

  expect_equal(dim(result$lls_phi), dim(result$pis_phi))
  expect_equal(dimnames(result$lls_phi), dimnames(result$pis_phi))

  # Check theta array dimensions
  ID_names <- as.character(unique(obs_data_ids$ID))
  expect_equal(dim(result$theta), c(3, 3, 2, 2))
  expect_equal(dimnames(result$theta)[[1]], names(coef(some_model)))
  expect_equal(dimnames(result$theta)[[2]], as.character(1:3))
  expect_equal(dimnames(result$theta)[[3]], ID_names)
  expect_equal(dimnames(result$theta)[[4]], as.character(3:4))

  # Check pis_theta and lls_theta
  expect_equal(dim(result$pis_theta), c(3, 2, 2))
  expect_equal(dimnames(result$pis_theta)[[1]], as.character(1:3))
  expect_equal(dimnames(result$pis_theta)[[2]], ID_names)
  expect_equal(dimnames(result$pis_theta)[[3]], as.character(3:4))

  expect_equal(dim(result$lls_theta), dim(result$pis_theta))
  expect_equal(dimnames(result$lls_theta), dimnames(result$pis_theta))
})


test_that("create_temperatures returns correct quantiles for tide", {
  result <- create_temperatures(20L, "TIDE")
  exp <- qbeta(p = seq(0, 1, length.out = 20), shape1 = 0.3, shape2 = 1)
  expect_equal(exp, result)
})


test_that("create_temperatures returns 1s quantiles for de_mcmc", {
  result <- create_temperatures(20L, "DE-MCMC")
  exp <- rep(1, 20L)
  expect_equal(exp, result)
})

test_that("create_temperatures input checks work", {
  expect_error(create_temperatures(20L, "de_ma"), "'arg'")
  expect_error(create_temperatures(20, "de_ma"), "is.integer")
  expect_error(create_temperatures(c(20L, 10L), "de_ma"), "length")
})
