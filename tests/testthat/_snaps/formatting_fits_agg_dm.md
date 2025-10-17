# print.fits_agg_dm prints header via summary and returns invisibly

    Code
      print(fits_agg)
    Output
      Fit approach: aggregated - classical
      Fitted model type: ratcliff_dm, drift_dm
      Optimizer: nmkb
      Convergence: TRUE
      N Individuals: 3 
      Average Trial Numbers:
       100 trials null

# print.summary.fits_agg_dm prints header only when requested

    Code
      print(sum_obj, just_header = TRUE)
    Output
      Fit approach: aggregated - classical
      Fitted model type: ratcliff_dm, drift_dm
      Optimizer: nmkb
      Convergence: TRUE
      N Individuals: 3 
      Average Trial Numbers:
       100 trials null

---

    Code
      print(sum_obj)
    Output
      Fit approach: aggregated - classical
      Fitted model type: ratcliff_dm, drift_dm
      Optimizer: nmkb
      Convergence: TRUE
      N Individuals: 3 
      Average Trial Numbers:
       100 trials null
      
      Parameters:
             muc     b non_dec
      null 2.891 0.534   0.294
      
      Cost Function: rmse
      
      Fit Indices:
          Log_Like Neg_Log_Like          AIC          BIC       RMSE_s      RMSE_ms 
                NA           NA           NA           NA        0.009        9.382 
      
      -------
      Deriving PDFS:
        solver: kfe
        values: sigma=1, t_max=3, dt=0.005, dx=0.005, nt=600, nx=400

---

    Code
      print(sum_obj, round_digits = 2)
    Output
      Fit approach: aggregated - classical
      Fitted model type: ratcliff_dm, drift_dm
      Optimizer: nmkb
      Convergence: TRUE
      N Individuals: 3 
      Average Trial Numbers:
       100 trials null
      
      Parameters:
            muc    b non_dec
      null 2.89 0.53    0.29
      
      Cost Function: rmse
      
      Fit Indices:
          Log_Like Neg_Log_Like          AIC          BIC       RMSE_s      RMSE_ms 
                NA           NA           NA           NA         0.01         9.38 
      
      -------
      Deriving PDFS:
        solver: kfe
        values: sigma=1, t_max=3, dt=0.005, dx=0.005, nt=600, nx=400

