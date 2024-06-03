# print works as expected

    Code
      print(a_dmc_model)
    Output
      Class(es): dmc_dm, drift_dm
      
      Model Parameters:
        values: muc=4, b=0.6, non_dec=0.3, sd_non_dec=0.02, tau=0.04, a=2, A=0.1, alpha=4
        free: muc, b, non_dec, sd_non_dec, tau, A, alpha
      
      Conditions: comp, incomp
      
      Deriving PDFs:
        solver: kfe
        values: sigma=1, t_max=3, dt=0.001, dx=0.001, nt=3000, nx=2000
      
      Observed Data: NULL

# summary works as expected

    Code
      print(summary_model)
    Output
      Class(es): dmc_dm, drift_dm
      
      Observed Data:
                   min. 1st qu. median  mean 3rd qu.  max.   n
      corr comp   0.331   0.436  0.479 0.507   0.549 1.075 292
      corr incomp 0.313   0.474  0.528 0.543   0.592 0.879 268
      err comp    0.428   0.458  0.526 0.564   0.621 0.871   8
      err incomp  0.302   0.398  0.452 0.458   0.498 0.771  32
      
      
      Number of Model Parameters:
      total  free 
          8     7 
      
      
      Model Parameter Values:
       muc   b non_dec sd_non_dec  tau a   A alpha
         4 0.6     0.3       0.02 0.04 2 0.1     4
      
      
      Fit Indices:
      log(like)       aic       bic 
        164.669  -315.339  -284.560 
      -------
      
      Conds: comp, incomp
      Free Parameters: muc, b, non_dec, sd_non_dec, tau, A, alpha
      Solver: kfe
      Settings: sigma=1, t_max=3, dt=0.005, dx=0.005, nt=600, nx=400

