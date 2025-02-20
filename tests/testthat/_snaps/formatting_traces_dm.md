# print.traces_dm_list works as expected

    Code
      print(traces)
    Output
      Class(es): traces_dm_list
      
      Time space:
      0.000, 0.001, 0.002, 0.003 ... 3.000 
      
      Condition: comp 
      ~>  0.000, -0.017, -0.007, -0.031 ...  0.606 
      ~>  0.000,  0.016,  0.060,  0.038 ...  0.659 
      
      Condition: incomp 
      ~>  0.000, -0.070, -0.072, -0.081 ...  0.680 
      ~>  0.000,  0.021,  0.041,  0.080 ...  0.604 

# print.traces_dm works as expected

    Code
      print(traces, print_steps = 6, print_k = 3)
    Output
      ~>  0.000, -0.017, -0.007, -0.031,  0.023 ...  0.606 
      ~>  0.000,  0.016,  0.060,  0.038, -0.010 ...  0.659 
      ~>  0.000, -0.065, -0.063, -0.068, -0.048 ...  0.606 
      ...

# summary.traces_dm works as expected

    Code
      print(summary_traces)
    Output
      Starting Points Added: no
      
      Number of Traces: 10
      
      Summary of First Passage Times:
        mean     sd p_corr  p_err 
       0.111  0.055  1.000  0.000 
      
      
      Orginal Parameter Values:
             muc          b    non_dec sd_non_dec        tau          a          A 
            4.00       0.60       0.30       0.02       0.04       2.00       0.10 
           alpha 
            4.00 
      
      -------
      Original Model Class(es): dmc_dm, drift_dm
      Settings: sigma=1, t_max=3, dt=0.001, dx=0.001, nt=3000, nx=2000

# summary.traces_dm_list works as expected

    Code
      print(summary_traces_list)
    Output
      Starting Points Added:
        comp incomp 
         yes     no 
      
      
      Number of Traces:
        comp incomp 
           5     10 
      
      
      Summary of First Passage Times:
              mean    sd p_corr p_err
      comp   0.143 0.054      1     0
      incomp 0.154 0.053      1     0
      
      
      Orginal Parameter Values:
             muc   b non_dec sd_non_dec  tau a    A alpha
      comp     4 0.6     0.3       0.02 0.04 2  0.1     4
      incomp   4 0.6     0.3       0.02 0.04 2 -0.1     4
      
      -------
      Original Model Class(es): dmc_dm, drift_dm
      
      Settings:
             sigma t_max    dt    dx  nt  nx
      comp       0     3 0.005 0.005 600 400
      incomp     1     3 0.005 0.005 600 400
      

