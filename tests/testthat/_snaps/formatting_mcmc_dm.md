# print.mcmc works as expected

    Code
      print(mcmc_obj)
    Output
      Sampler: DE-MCMC 
      Hierarchical: FALSE 
      No. Parameters: 3 
      No. Chains: 20 
      Iterations Per Chain: 200 

# summary.mcmc_dm works as expected (full object)

    Code
      print(summary_obj)
    Output
      Sampler: DE-MCMC 
      Hierarchical: FALSE 
      No. Parameters: 3 
      No. Chains: 20 
      Iterations Per Chain: 200 
      
      -------
      Parameter Summary: Basic Statistics
               Mean    SD Naive SE Time-series SE
      muc     3.082 0.187    0.003          0.010
      b       0.411 0.013    0.000          0.001
      non_dec 0.300 0.003    0.000          0.000
      
      Gelman-Rubin Statistics
          muc       b non_dec 
        1.039   1.046   1.048 
      
      Effective Sample Size
          muc       b non_dec 
      363.421 353.772 428.942 

