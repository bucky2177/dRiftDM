# estimate_dm -> sep_c works as expected

    Code
      all_fits <- estimate_dm(drift_dm_obj = tmp, obs_data = data, lower = l_u$lower,
      upper = l_u$upper, messaging = TRUE, verbose = 0, progress = 0, control = list(
        VTR = 0, trace = FALSE))
    Message
      Using the data supplied via the 'obs_data' argument.
      Using optimizer 'DEoptim'.
      Fitting the model separately to multiple participants (cost function:'neg_log_like'). The result will be a fit object of type 'fits_ids_dm'.

# estimate_dm -> agg_c works as expected

    Code
      estimate_dm(drift_dm_obj = tmp, obs_data = data, approach = "agg_c", lower = l_u$
        lower, upper = l_u$upper, messaging = TRUE, verbose = 0, control = list(VTR = 0.3,
        trace = FALSE, NP = 30))
    Message
      Using the data supplied via the 'obs_data' argument.
      Using optimizer 'DEoptim'.
      Changing the 'cost_function' to 'rmse'.
      Aggregated data has been set to the model.
      Fitting the model to aggregated data across participants. The returned object will of type 'fits_agg_dm'.
    Output
      Fit approach: aggregated - classical
      Fitted model type: ratcliff_dm, drift_dm
      Optimizer: DEoptim
      Convergence: TRUE
      N Individuals: 2 
      Average Trial Numbers:
       300 trials null

# estimate_dm -> sep_b works as expected

    Code
      all_fits <- estimate_dm(drift_dm_obj = tmp, obs_data = data, approach = "sep_b",
        lower = l_u$lower, messaging = TRUE, verbose = 0, burn_in = 1, progress = 0,
        samples = 1, n_chains = 3, n_cores = 2, seed = 1)
    Message
      Using the data supplied via the 'obs_data' argument.
      Using optimizer 'DE-MCMC'.
      Fitting the model separaetely to multiple participants using the Bayesian framework. For now, the result will be a list of 'mcmc_dm' objects.

# estimate_dm throws warning/errors for unreasonable input

    Code
      out <- estimate_dm(drift_dm_obj = model, obs_data = data, approach = "sep_b",
        optimizer = "Nelder-Mead", samples = 1, burn_in = 1, n_chains = 4, progress = 0,
        verbose = 0, messaging = TRUE)
    Message
      Using the data supplied via the 'obs_data' argument.
      The requested optimizer ('Nelder-Mead') is not compatible with the specified approach ('sep_b'). Using the following optimizer instead: 'DE-MCMC'.
      Changing the 'cost_function' to 'neg_log_like', to get the log-likelihood for Bayesian inference.
      Fitting a single data set/participant using the Bayesian framework. The result will be a fit object of type 'mcmc_dm'.

# estimate_classical -> start_vals are applied and correctly mapped

    Code
      suppressWarnings(estimate_classical(model, verbose = 2, optimizer = "Nelder-Mead",
        control = list(maxit = 1), start_vals = c(b = 0.3, non_dec = 0.2, muc = 2)))
    Message
      Starting optimizer 'Nelder-Mead' with the following starting values:
      muc=2, b=0.3, non_dec=0.2
      Parameters
      muc = 2
      b = 0.3
      non_dec = 0.2
      ==> gave a neg_log_like of 726.033
      Parameters
      muc = 2.2
      b = 0.3
      non_dec = 0.2
      ==> gave a neg_log_like of 817.28
      Parameters
      muc = 2
      b = 0.33
      non_dec = 0.2
      ==> gave a neg_log_like of 231.146
      Parameters
      muc = 2
      b = 0.3
      non_dec = 0.22
      ==> gave a neg_log_like of 561.235
      Parameters
      muc = 1.8
      b = 0.32
      non_dec = 0.213
      ==> gave a neg_log_like of 318.992
      Parameters
      muc = 1.9
      b = 0.315
      non_dec = 0.21
      ==> gave a neg_log_like of 443.313
      Optimization routine exited after 6 function evaluations
      Final Parameters
      muc = 2
      b = 0.33
      non_dec = 0.2
      ==> gave a neg_log_like of 231.146
    Output
      Class(es) ratcliff_dm, drift_dm
      Optimizer: Nelder-Mead
      Convergence: FALSE
      Message: maxit reached
      
      Parameter Values:
           muc    b non_dec
      null   2 0.33     0.2
      
      Parameter Settings:
           muc b non_dec
      null   1 2       3
      
      Deriving PDFS:
        solver: kfe
        values: sigma=1, t_max=3, dt=0.01, dx=0.01, nt=300, nx=200
      
      Cost Function: neg_log_like
      
      Observed Data: 300 trials null
      

---

    Code
      suppressWarnings(res <- estimate_classical(model, verbose = 2, optimizer = "Nelder-Mead",
        control = list(maxit = 1), start_vals = start_vals, round_digits = 2))
    Message
      Optimization run: 1
      Starting optimizer 'Nelder-Mead' with the following starting values:
      muc=1, b=0.3, non_dec=0.2
      Parameters
      muc = 1
      b = 0.3
      non_dec = 0.2
      ==> gave a neg_log_like of 605.08
      Parameters
      muc = 1.1
      b = 0.3
      non_dec = 0.2
      ==> gave a neg_log_like of 603.7
      Parameters
      muc = 1
      b = 0.33
      non_dec = 0.2
      ==> gave a neg_log_like of 97.8
      Parameters
      muc = 1
      b = 0.3
      non_dec = 0.22
      ==> gave a neg_log_like of 446.99
      Parameters
      muc = 1.067
      b = 0.32
      non_dec = 0.213
      ==> gave a neg_log_like of 165.93
      Parameters
      muc = 1.05
      b = 0.315
      non_dec = 0.21
      ==> gave a neg_log_like of 331.43
      Optimization routine exited after 6 function evaluations
      Final Parameters
      muc = 1
      b = 0.33
      non_dec = 0.2
      ==> gave a neg_log_like of 97.8
      Optimization run: 2
      Starting optimizer 'Nelder-Mead' with the following starting values:
      muc=3, b=0.5, non_dec=0.3
      Parameters
      muc = 3
      b = 0.5
      non_dec = 0.3
      ==> gave a neg_log_like of -290.5
      Parameters
      muc = 3.3
      b = 0.5
      non_dec = 0.3
      ==> gave a neg_log_like of -296.69
      Parameters
      muc = 3
      b = 0.55
      non_dec = 0.3
      ==> gave a neg_log_like of -248.17
      Parameters
      muc = 3
      b = 0.5
      non_dec = 0.33
      ==> gave a neg_log_like of 292.2
      Parameters
      muc = 3.2
      b = 0.533
      non_dec = 0.27
      ==> gave a neg_log_like of -297.55
      Parameters
      muc = 3.3
      b = 0.55
      non_dec = 0.24
      ==> gave a neg_log_like of -241.01
      Optimization routine exited after 6 function evaluations
      Final Parameters
      muc = 3.3
      b = 0.5
      non_dec = 0.3
      ==> gave a neg_log_like of -296.69
      Optimization run: 3
      Starting optimizer 'Nelder-Mead' with the following starting values:
      muc=7, b=0.9, non_dec=0.4
      Parameters
      muc = 7
      b = 0.9
      non_dec = 0.4
      ==> gave a neg_log_like of 3424.94
      Parameters
      muc = 7.7
      b = 0.9
      non_dec = 0.4
      ==> gave a neg_log_like of 3486.63
      Parameters
      muc = 7
      b = 0.99
      non_dec = 0.4
      ==> gave a neg_log_like of 3496.08
      Parameters
      muc = 7
      b = 0.9
      non_dec = 0.44
      ==> gave a neg_log_like of 4077.76
      Parameters
      muc = 7.467
      b = 0.96
      non_dec = 0.36
      ==> gave a neg_log_like of 2458.69
      Parameters
      muc = 7.7
      b = 0.99
      non_dec = 0.32
      ==> gave a neg_log_like of 777.43
      Optimization routine exited after 6 function evaluations
      Final Parameters
      muc = 7
      b = 0.9
      non_dec = 0.4
      ==> gave a neg_log_like of 3424.94
      Optimization run 2 yielded the smallest cost value

# estimate_classical_wrapper -> start_vals correctly passed forward

    Code
      suppressWarnings(estimate_classical_wrapper(model, obs_data_ids = data,
        verbose = 2, optimizer = "Nelder-Mead", control = list(maxit = 1),
        start_vals = start_vals, progress = 0))
    Message
      Optimization run: 1
      Starting optimizer 'Nelder-Mead' with the following starting values:
      muc=2, b=0.4, non_dec=0.2
      Parameters
      muc = 2
      b = 0.4
      non_dec = 0.2
      ==> gave a neg_log_like of -58.517
      Parameters
      muc = 2.2
      b = 0.4
      non_dec = 0.2
      ==> gave a neg_log_like of -52.08
      Parameters
      muc = 2
      b = 0.44
      non_dec = 0.2
      ==> gave a neg_log_like of -108.055
      Parameters
      muc = 2
      b = 0.4
      non_dec = 0.22
      ==> gave a neg_log_like of -116.698
      Parameters
      muc = 1.8
      b = 0.427
      non_dec = 0.213
      ==> gave a neg_log_like of -121.226
      Parameters
      muc = 1.6
      b = 0.44
      non_dec = 0.22
      ==> gave a neg_log_like of -154.525
      Optimization routine exited after 6 function evaluations
      Final Parameters
      muc = 2
      b = 0.4
      non_dec = 0.22
      ==> gave a neg_log_like of -116.698
      Optimization run: 2
      Starting optimizer 'Nelder-Mead' with the following starting values:
      muc=3, b=0.5, non_dec=0.3
      Parameters
      muc = 3
      b = 0.5
      non_dec = 0.3
      ==> gave a neg_log_like of -290.503
      Parameters
      muc = 3.3
      b = 0.5
      non_dec = 0.3
      ==> gave a neg_log_like of -296.691
      Parameters
      muc = 3
      b = 0.55
      non_dec = 0.3
      ==> gave a neg_log_like of -248.17
      Parameters
      muc = 3
      b = 0.5
      non_dec = 0.33
      ==> gave a neg_log_like of 292.202
      Parameters
      muc = 3.2
      b = 0.533
      non_dec = 0.27
      ==> gave a neg_log_like of -297.552
      Parameters
      muc = 3.3
      b = 0.55
      non_dec = 0.24
      ==> gave a neg_log_like of -241.01
      Optimization routine exited after 6 function evaluations
      Final Parameters
      muc = 3.3
      b = 0.5
      non_dec = 0.3
      ==> gave a neg_log_like of -296.691
      Optimization run 2 yielded the smallest cost value
      Optimization run: 1
      Starting optimizer 'Nelder-Mead' with the following starting values:
      muc=4, b=0.6, non_dec=0.4
      Parameters
      muc = 4
      b = 0.6
      non_dec = 0.4
      ==> gave a neg_log_like of 3227.319
      Parameters
      muc = 4.4
      b = 0.6
      non_dec = 0.4
      ==> gave a neg_log_like of 3222.03
      Parameters
      muc = 4
      b = 0.66
      non_dec = 0.4
      ==> gave a neg_log_like of 3275.903
      Parameters
      muc = 4
      b = 0.6
      non_dec = 0.44
      ==> gave a neg_log_like of 3933.346
      Parameters
      muc = 4.267
      b = 0.64
      non_dec = 0.36
      ==> gave a neg_log_like of 2067.861
      Parameters
      muc = 4.4
      b = 0.66
      non_dec = 0.32
      ==> gave a neg_log_like of 193.854
      Optimization routine exited after 6 function evaluations
      Final Parameters
      muc = 4.4
      b = 0.6
      non_dec = 0.4
      ==> gave a neg_log_like of 3222.03
      Optimization run: 2
      Starting optimizer 'Nelder-Mead' with the following starting values:
      muc=5, b=0.7, non_dec=0.5
      Parameters
      muc = 5
      b = 0.7
      non_dec = 0.5
      ==> gave a neg_log_like of 4743.55
      Parameters
      muc = 5.5
      b = 0.7
      non_dec = 0.5
      ==> gave a neg_log_like of 4739.862
      Parameters
      muc = 5
      b = 0.77
      non_dec = 0.5
      ==> gave a neg_log_like of 4766.348
      Parameters
      muc = 5
      b = 0.7
      non_dec = 0.55
      ==> gave a neg_log_like of 4995.709
      Parameters
      muc = 5.333
      b = 0.747
      non_dec = 0.45
      ==> gave a neg_log_like of 4162.772
      Parameters
      muc = 5.5
      b = 0.77
      non_dec = 0.4
      ==> gave a neg_log_like of 3344.136
      Optimization routine exited after 6 function evaluations
      Final Parameters
      muc = 5.5
      b = 0.7
      non_dec = 0.5
      ==> gave a neg_log_like of 4739.862
      Optimization run 1 yielded the smallest cost value
    Output
      Fit approach: separately - classical
      Fitted model type: ratcliff_dm, drift_dm
      Optimizer: Nelder-Mead 
      Convergence: Failed for 2 participants 
      N Individuals: 2 
      Average Trial Numbers:
       300 trials null
      Cost Function: neg_log_like

---

    Code
      suppressWarnings(res <- estimate_classical_wrapper(model, obs_data_ids = data,
        verbose = 2, optimizer = "Nelder-Mead", control = list(maxit = 1),
        start_vals = start_vals, progress = 0))
    Message
      Starting optimizer 'Nelder-Mead' with the following starting values:
      muc=2, b=0.4, non_dec=0.2
      Parameters
      muc = 2
      b = 0.4
      non_dec = 0.2
      ==> gave a neg_log_like of -58.517
      Parameters
      muc = 2.2
      b = 0.4
      non_dec = 0.2
      ==> gave a neg_log_like of -52.08
      Parameters
      muc = 2
      b = 0.44
      non_dec = 0.2
      ==> gave a neg_log_like of -108.055
      Parameters
      muc = 2
      b = 0.4
      non_dec = 0.22
      ==> gave a neg_log_like of -116.698
      Parameters
      muc = 1.8
      b = 0.427
      non_dec = 0.213
      ==> gave a neg_log_like of -121.226
      Parameters
      muc = 1.6
      b = 0.44
      non_dec = 0.22
      ==> gave a neg_log_like of -154.525
      Optimization routine exited after 6 function evaluations
      Final Parameters
      muc = 2
      b = 0.4
      non_dec = 0.22
      ==> gave a neg_log_like of -116.698
      Starting optimizer 'Nelder-Mead' with the following starting values:
      muc=4, b=0.6, non_dec=0.4
      Parameters
      muc = 4
      b = 0.6
      non_dec = 0.4
      ==> gave a neg_log_like of 3227.319
      Parameters
      muc = 4.4
      b = 0.6
      non_dec = 0.4
      ==> gave a neg_log_like of 3222.03
      Parameters
      muc = 4
      b = 0.66
      non_dec = 0.4
      ==> gave a neg_log_like of 3275.903
      Parameters
      muc = 4
      b = 0.6
      non_dec = 0.44
      ==> gave a neg_log_like of 3933.346
      Parameters
      muc = 4.267
      b = 0.64
      non_dec = 0.36
      ==> gave a neg_log_like of 2067.861
      Parameters
      muc = 4.4
      b = 0.66
      non_dec = 0.32
      ==> gave a neg_log_like of 193.854
      Optimization routine exited after 6 function evaluations
      Final Parameters
      muc = 4.4
      b = 0.6
      non_dec = 0.4
      ==> gave a neg_log_like of 3222.03

# estimate_classical_wrapper -> multiple core usage works

    Code
      suppressWarnings(res_3 <- estimate_classical_wrapper(model, obs_data_ids = data,
        optimizer = "DEoptim", n_cores = 2, lower = l_u$lower, upper = l_u$upper,
        progress = 0, control = list(VTR = -300, trace = TRUE),
        parallelization_strategy = 2, seed = 1))
    Output
      Iteration: 1 bestvalit: -320.789483 bestmemit:    3.564939    0.399337    0.298039
      Iteration: 1 bestvalit: -320.789483 bestmemit:    3.564939    0.399337    0.298039

---

    Code
      suppressWarnings(res_4 <- estimate_classical_wrapper(model, obs_data_ids = data,
        optimizer = "DEoptim", n_cores = 2, lower = l_u$lower, upper = l_u$upper,
        progress = 0, control = list(VTR = -300, trace = TRUE),
        parallelization_strategy = 1, seed = 1))
      coef(res_4)
    Output
      Object Type: coefs_dm
      
        ID   muc     b non_dec
      1  1 4.132 0.503   0.295
      2  2 3.265 0.399   0.313
      
      (access the data.frame's columns/rows as usual)

