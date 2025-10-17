# print.mcmc works as expected

    Code
      print(mcmc_obj)
    Output
      Sampler: DE-MCMC 
      Hierarchical: FALSE 
      No. Parameters: 3 
      No. Chains: 10 
      Iterations Per Chain: 200 

# summary.mcmc_dm works as expected (full object)

    Code
      print(summary_obj)
    Output
      Sampler: DE-MCMC 
      Hierarchical: FALSE 
      No. Parameters: 3 
      No. Chains: 10 
      Iterations Per Chain: 200 
      
      -------
      Parameter Summary: Basic Statistics
               Mean    SD Naive SE Time-series SE
      muc     3.059 0.204    0.005          0.018
      b       0.411 0.013    0.000          0.001
      non_dec 0.299 0.004    0.000          0.000
      
      Gelman-Rubin Statistics
          muc       b non_dec 
        1.102   1.182   1.190 
      
      Effective Sample Size
          muc       b non_dec 
      191.578 205.012 179.752 

