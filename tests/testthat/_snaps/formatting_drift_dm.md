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

# summary works as expected

    Code
      print(summary_model)
    Output
      Class(es): dmc_dm, drift_dm
      
      Observed Data:
                      min. 1st qu. median      mean 3rd qu.  max.   n
      correct comp   0.283 0.34075  0.382 0.4208288 0.47825 0.775 292
      correct incomp 0.275 0.37200  0.424 0.4415128 0.48400 0.911 273
      error comp     0.319 0.34450  0.393 0.3925000 0.41400 0.499   8
      error incomp   0.264 0.30300  0.330 0.3380370 0.36700 0.476  27
      
      
      Number of Model Parameters:
      total  free 
          8     7 
      
      
      Model Parameter Values:
       muc   b non_dec sd_non_dec  tau a   A alpha
         4 0.6     0.3       0.02 0.04 2 0.1     4
      
      
      Fit Indices:
      log(like)       aic       bic 
       488.2633 -962.5265 -931.7480 
      -------
      
      Conds: comp, incomp
      Free Parameters: muc, b, non_dec, sd_non_dec, tau, A, alpha
      Solver: kfe
      Settings: sigma=1, t_max=3, dt=0.005, dx=0.005, nt=600, nx=400

