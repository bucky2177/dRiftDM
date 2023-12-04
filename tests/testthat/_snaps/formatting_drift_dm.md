# print works as expected

    Code
      print(a_dmc_model)
    Output
      Class(es): dmc_dm, drift_dm
      
      Model Parameters:
        values: muc=4, b=0.6, non_dec=0.3, sd_non_dec=0.02, tau=0.04, a=2, A=0.1, alpha=4
        free: muc, b, non_dec, sd_non_dec, tau, A, alpha
      
      Conditions: comp, incomp
      
      Discretization:
        solver: kfe
        values: sigma=1, t_max=3, dt=0.001, dx=0.001, nt=3000, nx=2000

# summary works as expected

    Code
      print(summary_model)
    Output
      Class(es): dmc_dm, drift_dm
      
      Observed Data:
                      min. 1st qu. median      mean 3rd qu.  max.   n
      correct comp   0.283  0.3415  0.382 0.4209588   0.479 0.775 291
      correct incomp 0.275  0.3720  0.423 0.4414249   0.484 0.911 273
      error comp     0.319  0.3480  0.401 0.4002222   0.450 0.499   9
      error incomp   0.264  0.3030  0.331 0.3382593   0.367 0.476  27
      
      
      Number of Model Parameters:
      total  free 
          8     7 
      
      
      Model Parameter Values:
       muc   b non_dec sd_non_dec  tau a   A alpha
         4 0.6     0.3       0.02 0.04 2 0.1     4
      
      
      Fit Indices:
      log(like)       aic       bic 
       483.9702 -953.9405 -923.1620 
      -------
      
      Conds: comp, incomp
      Free Parameters: muc, b, non_dec, sd_non_dec, tau, A, alpha
      Solver: kfe
      Settings: sigma=1, t_max=3, dt=0.005, dx=0.005, nt=600, nx=400

