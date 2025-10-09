# print.drift_dm (old) works as expected

    Code
      print(a_model)
    Output
      Class(es) ratcliff_dm, drift_dm
      
      Parameter Values:
             muc    b non_dec
      null 3.352 0.59   0.313
      
      Parameter Settings:
           muc b non_dec
      null   1 2       3
      
      Deriving PDFS:
        solver: kfe
        values: sigma=1, t_max=1, dt=0.01, dx=0.01, nt=100, nx=200
      
      Observed Data: 300 trials null
      

# print.drift_dm (new) works as expected

    Code
      print(a_model)
    Output
      Class(es) dmc_dm, drift_dm
      Optimizer: nmkb
      Convergence: TRUE
      
      Parameter Values:
               muc     b non_dec sd_non_dec   tau a      A alpha
      comp   4.731 0.438   0.344      0.033 0.028 2  0.107 6.504
      incomp 4.731 0.438   0.344      0.033 0.028 2 -0.107 6.504
      
      Parameter Settings:
             muc b non_dec sd_non_dec tau a A alpha
      comp   1   2 3       4          5   0 6 7    
      incomp 1   2 3       4          5   0 d 7    
      
      Special Dependencies:
      A ~ incomp == -(A ~ comp)
      
      Custom Parameters:
             peak_l
      comp    0.028
      incomp  0.028
      
      Deriving PDFS:
        solver: kfe
        values: sigma=1, t_max=3, dt=0.01, dx=0.01, nt=300, nx=200
      
      Cost Function: neg_log_like
      
      Observed Data: 168 trials comp; 168 trials incomp
      

# summary.drift_dm (old) works as expected

    Code
      print(summary_model)
    Output
      Class(es) ratcliff_dm, drift_dm
      
      Parameter Values:
             muc    b non_dec
      null 3.352 0.59   0.313
      
      Parameter Settings:
           muc b non_dec
      null   1 2       3
      
      Observed Data:
                min. 1st qu. median  mean 3rd qu. max.   n
      corr null 0.33    0.40   0.45 0.480    0.53 0.91 294
      err null  0.35    0.37   0.44 0.467    0.51 0.69   6
      
      Fit Indices:
          Log_Like Neg_Log_Like          AIC          BIC       RMSE_s      RMSE_ms 
          274.7254    -274.7254    -543.4507    -532.3394       0.0127      12.7273 
      
      -------
      Deriving PDFS:
        solver: kfe
        values: sigma=1, t_max=1, dt=0.01, dx=0.01, nt=100, nx=200
      
      Boundary Coding:
        upper: corr 
        lower: err 
        expected data column: Error (corr = 0; err = 1) 

