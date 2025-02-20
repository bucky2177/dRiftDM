# print.drift_dm works as expected

    Code
      print(a_dmc_model)
    Output
      Class(es): dmc_dm, drift_dm
      
      Current Parameter Matrix:
             muc   b non_dec sd_non_dec  tau a    A alpha
      comp     4 0.6     0.3       0.02 0.04 2  0.1     4
      incomp   4 0.6     0.3       0.02 0.04 2 -0.1     4
      
      Unique Parameters:
             muc b non_dec sd_non_dec tau a A alpha
      comp   1   2 3       4          5   0 6 7    
      incomp 1   2 3       4          5   0 d 7    
      
      Special Dependencies:
      A ~ incomp == -(A ~ comp)
      
      Custom Parameters:
             peak_l
      comp     0.04
      incomp   0.04
      
      Deriving PDFs:
        solver: kfe
        values: sigma=1, t_max=3, dt=0.001, dx=0.001, nt=3000, nx=2000
      
      Observed Data: 300 trials comp; 300 trials incomp

# summary.drift_dm works as expected

    Code
      print(summary_model)
    Output
      Class(es): dmc_dm, drift_dm
      
      Current Parameter Matrix:
             muc   b non_dec sd_non_dec  tau a    A alpha
      comp     4 0.6     0.3       0.02 0.04 2  0.1     4
      incomp   4 0.6     0.3       0.02 0.04 2 -0.1     4
      
      Unique Parameters:
             muc b non_dec sd_non_dec tau a A alpha
      comp   1   2 3       4          5   0 6 7    
      incomp 1   2 3       4          5   0 d 7    
      
      Special Dependencies:
      A ~ incomp == -(A ~ comp)
      
      Custom Parameters:
             peak_l
      comp     0.04
      incomp   0.04
      
      Observed Data:
                   min. 1st qu. median  mean 3rd qu.  max.   n
      corr comp   0.331   0.436  0.479 0.507   0.549 1.075 292
      corr incomp 0.313   0.474  0.528 0.543   0.592 0.879 268
      err comp    0.428   0.458  0.526 0.564   0.621 0.871   8
      err incomp  0.302   0.398  0.452 0.458   0.498 0.771  32
      
      Fit Indices:
      Log_Like      AIC      BIC 
       137.067 -260.133 -229.355 
      
      -------
      Solver: kfe
      Settings: sigma=1, t_max=3, dt=0.001, dx=0.001, nt=3000, nx=2000

