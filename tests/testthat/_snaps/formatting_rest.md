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

---

    Code
      print(traces, print_steps = 6, print_k = 3)
    Output
      ~>  0.000, -0.017, -0.007, -0.031,  0.023 ...  0.606 
      ~>  0.000,  0.016,  0.060,  0.038, -0.010 ...  0.659 
      ~>  0.000, -0.065, -0.063, -0.068, -0.048 ...  0.606 
      ...

