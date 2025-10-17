# print.stats_dm works as expected

    Code
      print(some_stats, some = TRUE, print_rows = 3, show_header = FALSE, show_note = FALSE)
    Output
        Source   Cond Bin P_corr
      1    obs   comp   1  1.000
      4    obs   comp   4  0.983
      7    obs incomp   2  0.867
      ...

# print.basic_stats works as expected

    Code
      print(basic_obj)
    Output
      Type of Statistic: basic_stats
      
        Source Cond Mean_corr Mean_err SD_corr SD_err P_corr
      1    obs null     0.414    0.417   0.086  0.094   0.93
      
      (access the data.frame's columns/rows as usual)

# print.cafs works as expected

    Code
      print(cafs_obj)
    Output
      Type of Statistic: cafs
      
         Source   Cond Bin P_corr
      1     obs   comp   1  1.000
      2     obs   comp   2  0.950
      3     obs   comp   3  0.984
      4     obs   comp   4  0.983
      5     obs   comp   5  0.950
      6     obs incomp   1  0.733
      7     obs incomp   2  0.867
      8     obs incomp   3  0.950
      9     obs incomp   4  0.950
      10    obs incomp   5  0.967
      ...
      
      (access the data.frame's columns/rows as usual)

# print.quantiles works as expected

    Code
      print(quantiles_obj)
    Output
      Type of Statistic: quantiles
      
         Source   Cond Prob Quant_corr Quant_err
      1     obs   comp  0.1      0.401     0.448
      2     obs   comp  0.2      0.427     0.458
      3     obs   comp  0.3      0.441     0.463
      4     obs   comp  0.4      0.463     0.489
      5     obs   comp  0.5      0.479     0.526
      6     obs   comp  0.6      0.503     0.568
      7     obs   comp  0.7      0.535     0.613
      8     obs   comp  0.8      0.577     0.622
      9     obs   comp  0.9      0.648     0.698
      10    obs incomp  0.1      0.440     0.360
      ...
      
      (access the data.frame's columns/rows as usual)

# print.delta_funs works as expected

    Code
      print(delta_funs_obj)
    Output
      Type of Statistic: delta_funs
      
         Source Prob Quant_corr_comp Quant_corr_incomp Delta_incomp_comp
      1     obs  0.1           0.401             0.440             0.039
      2     obs  0.2           0.427             0.464             0.037
      3     obs  0.3           0.441             0.483             0.042
      4     obs  0.4           0.463             0.508             0.045
      5     obs  0.5           0.479             0.528             0.050
      6     obs  0.6           0.503             0.544             0.042
      7     obs  0.7           0.535             0.582             0.047
      8     obs  0.8           0.577             0.618             0.040
      9     obs  0.9           0.648             0.672             0.024
      10   pred  0.1           0.325             0.352             0.027
         Avg_incomp_comp
      1            0.421
      2            0.446
      3            0.462
      4            0.485
      5            0.504
      6            0.523
      7            0.558
      8            0.598
      9            0.660
      10           0.339
      ...
      
      (access the data.frame's columns/rows as usual)

# print.fit_stats works as expected

    Code
      print(fit_stats_obj)
    Output
      Type of Statistic: fit_stats
      
        ID Log_Like Neg_Log_Like      AIC      BIC RMSE_s RMSE_ms
      1  1  399.565     -399.565 -785.129 -758.410  0.019  18.806
      2  2  377.965     -377.965 -741.929 -715.251  0.032  32.040
      3  3  472.748     -472.748 -931.496 -904.776  0.014  13.758
      
      (access the data.frame's columns/rows as usual)

# print.stats_dm_list works as expected

    Code
      print(stats_dm_list_obj)
    Output
      Element 1, contains fit_stats
      
        ID Log_Like Neg_Log_Like      AIC      BIC RMSE_s RMSE_ms
      1  1  399.565     -399.565 -785.129 -758.410  0.019  18.806
      2  2  377.965     -377.965 -741.929 -715.251  0.032  32.040
      3  3  472.748     -472.748 -931.496 -904.776  0.014  13.758
      
      
      Element 2, contains quantiles
      
         ID Source   Cond Prob Quant_corr Quant_err
      1   1    obs   comp  0.1      0.335     0.361
      2   1    obs   comp  0.2      0.368     0.388
      3   1    obs   comp  0.3      0.385     0.415
      4   1    obs   comp  0.4      0.385     0.441
      5   1    obs   comp  0.5      0.401     0.468
      6   1    obs   comp  0.6      0.418     0.468
      7   1    obs   comp  0.7      0.435     0.468
      8   1    obs   comp  0.8      0.452     0.468
      9   1    obs   comp  0.9      0.501     0.468
      10  1    obs incomp  0.1      0.368     0.361
      ...
      
      (extract the list's elements as usual, e.g., with $fit_stats)

---

    Code
      print(test)
    Output
      named list()

# summary.stats_dm works as expected

    Code
      print(summary_stats)
    Output
      Type of Statistic: stats_dm
      
      Dependent Variables:
             ID         Log_Like    Neg_Log_Like       AIC            BIC      
       Min.   :1.0   Min.   :378   Min.   :-473   Min.   :-931   Min.   :-905  
       1st Qu.:1.5   1st Qu.:389   1st Qu.:-436   1st Qu.:-858   1st Qu.:-832  
       Median :2.0   Median :400   Median :-400   Median :-785   Median :-758  
       Mean   :2.0   Mean   :417   Mean   :-417   Mean   :-820   Mean   :-793  
       3rd Qu.:2.5   3rd Qu.:436   3rd Qu.:-389   3rd Qu.:-764   3rd Qu.:-737  
       Max.   :3.0   Max.   :473   Max.   :-378   Max.   :-742   Max.   :-715  
           RMSE_s          RMSE_ms    
       Min.   :0.0138   Min.   :13.8  
       1st Qu.:0.0163   1st Qu.:16.3  
       Median :0.0188   Median :18.8  
       Mean   :0.0215   Mean   :21.5  
       3rd Qu.:0.0254   3rd Qu.:25.4  
       Max.   :0.0320   Max.   :32.0  
      
      N IDs: 3 

# summary.sum_dist works as expected

    Code
      print(summary_stats)
    Output
      Type of Statistic: sum_dist
      
      Dependent Variables:
          Source              Cond                Bin        P_corr     
       Length:20          Length:20          Min.   :1   Min.   :0.733  
       Class :character   Class :character   1st Qu.:2   1st Qu.:0.950  
       Mode  :character   Mode  :character   Median :3   Median :0.981  
                                             Mean   :3   Mean   :0.951  
                                             3rd Qu.:4   3rd Qu.:0.984  
                                             Max.   :5   Max.   :1.000  
      
      Sources: obs, pred 

# summary.basic_stats works as expected

    Code
      print(summary_stats)
    Output
      Type of Statistic: basic_stats
      
      Dependent Variables:
         Mean_corr       Mean_err       SD_corr          SD_err          P_corr    
       Min.   :0.36   Min.   :0.29   Min.   :0.053   Min.   :0.020   Min.   :0.90  
       1st Qu.:0.38   1st Qu.:0.35   1st Qu.:0.063   1st Qu.:0.046   1st Qu.:0.96  
       Median :0.41   Median :0.36   Median :0.068   Median :0.060   Median :0.97  
       Mean   :0.41   Mean   :0.37   Mean   :0.067   Mean   :0.056   Mean   :0.96  
       3rd Qu.:0.44   3rd Qu.:0.40   3rd Qu.:0.070   3rd Qu.:0.070   3rd Qu.:0.98  
       Max.   :0.45   Max.   :0.44   Max.   :0.085   Max.   :0.077   Max.   :1.00  
                      NA's   :1                      NA's   :1                     
      
      N IDs: 3 
      Sources: obs, pred 
      Conditions: comp, incomp 

# summary.cafs works as expected

    Code
      print(summary_stats)
    Output
      Type of Statistic: cafs
      
      Dependent Variables:
           P_corr    
       Min.   :0.74  
       1st Qu.:0.96  
       Median :0.98  
       Mean   :0.96  
       3rd Qu.:1.00  
       Max.   :1.00  
      
      N IDs: 3 
      Sources: obs, pred 
      Conditions: comp, incomp 
      Bins: 1, 2, 3, 4, 5 

# summary.quantiles works as expected

    Code
      print(summary_stats)
    Output
      Type of Statistic: quantiles
      
      Dependent Variables:
         Quant_corr      Quant_err    
       Min.   :0.285   Min.   :0.230  
       1st Qu.:0.368   1st Qu.:0.335  
       Median :0.401   Median :0.368  
       Mean   :0.411   Mean   :0.376  
       3rd Qu.:0.451   3rd Qu.:0.410  
       Max.   :0.638   Max.   :0.668  
                       NA's   :18     
      
      N IDs: 16 
      Sources: obs 
      Conditions: comp, incomp 
      Probs: 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 

# summary.delta_funs works as expected

    Code
      print(summary_stats)
    Output
      Type of Statistic: delta_funs
      
      Dependent Variables:
       Quant_corr_comp Quant_corr_incomp Delta_incomp_comp  Avg_incomp_comp
       Min.   :0.325   Min.   :0.352     Min.   :-0.00102   Min.   :0.339  
       1st Qu.:0.402   1st Qu.:0.433     1st Qu.: 0.02430   1st Qu.:0.419  
       Median :0.452   Median :0.481     Median : 0.02990   Median :0.467  
       Mean   :0.460   Mean   :0.490     Mean   : 0.03014   Mean   :0.475  
       3rd Qu.:0.507   3rd Qu.:0.540     3rd Qu.: 0.04125   3rd Qu.:0.520  
       Max.   :0.648   Max.   :0.672     Max.   : 0.05000   Max.   :0.660  
      
      Sources: obs, pred 
      Probs: 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 

# summary.fit_stats works as expected

    Code
      print(summary_stats)
    Output
      Type of Statistic: fit_stats
      
      Dependent Variables:
          Log_Like    Neg_Log_Like       AIC            BIC           RMSE_s      
       Min.   :378   Min.   :-473   Min.   :-931   Min.   :-905   Min.   :0.0138  
       1st Qu.:389   1st Qu.:-436   1st Qu.:-858   1st Qu.:-832   1st Qu.:0.0163  
       Median :400   Median :-400   Median :-785   Median :-758   Median :0.0188  
       Mean   :417   Mean   :-417   Mean   :-820   Mean   :-793   Mean   :0.0215  
       3rd Qu.:436   3rd Qu.:-389   3rd Qu.:-764   3rd Qu.:-737   3rd Qu.:0.0254  
       Max.   :473   Max.   :-378   Max.   :-742   Max.   :-715   Max.   :0.0320  
          RMSE_ms    
       Min.   :13.8  
       1st Qu.:16.3  
       Median :18.8  
       Mean   :21.5  
       3rd Qu.:25.4  
       Max.   :32.0  
      
      N IDs: 3 

# summary.stats_dm_list works as expected

    Code
      print(summary_list)
    Output
      Summary of Element 1: quantiles
      
      Dependent Variables:
         Quant_corr      Quant_err    
       Min.   :0.325   Min.   :0.301  
       1st Qu.:0.411   1st Qu.:0.361  
       Median :0.465   Median :0.431  
       Mean   :0.475   Mean   :0.438  
       3rd Qu.:0.530   3rd Qu.:0.491  
       Max.   :0.672   Max.   :0.698  
      
      Sources: obs, pred 
      Conditions: comp, incomp 
      Probs: 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 
      -------
      
      Summary of Element 2: cafs
      
      Dependent Variables:
           P_corr     
       Min.   :0.733  
       1st Qu.:0.950  
       Median :0.981  
       Mean   :0.951  
       3rd Qu.:0.984  
       Max.   :1.000  
      
      Sources: obs, pred 
      Conditions: comp, incomp 
      Bins: 1, 2, 3, 4, 5 
      -------

