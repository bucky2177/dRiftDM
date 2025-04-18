# print.stats_dm works as expected

    Code
      print(some_stats, some = TRUE, print_rows = 3, show_header = FALSE, show_note = FALSE)
    Output
        Source   Cond Bin P_corr
      4   pred   comp   4  0.983
      7   pred incomp   2  0.968
      9   pred incomp   4  0.991
      ...

# print.basic_stats works as expected

    Code
      print(basic_obj)
    Output
      Type of Statistic: basic_stats
      
        Source Cond Mean_corr Mean_err SD_corr SD_err P_corr
      1   pred null      0.49     0.49    0.13   0.13  0.973
      
      (access the data.frame's columns/rows as usual)

# print.cafs works as expected

    Code
      print(cafs_obj)
    Output
      Type of Statistic: cafs
      
         Source   Cond Bin P_corr
      1    pred   comp   1  0.982
      2    pred   comp   2  0.983
      3    pred   comp   3  0.980
      4    pred   comp   4  0.983
      5    pred   comp   5  0.989
      6    pred incomp   1  0.825
      7    pred incomp   2  0.968
      8    pred incomp   3  0.986
      9    pred incomp   4  0.991
      10   pred incomp   5  0.992
      
      (access the data.frame's columns/rows as usual)

# print.quantiles works as expected

    Code
      print(quantiles_obj)
    Output
      Type of Statistic: quantiles
      
         Source   Cond Prob Quant_corr Quant_err
      1    pred   comp  0.1      0.323     0.318
      2    pred   comp  0.2      0.343     0.340
      3    pred   comp  0.3      0.362     0.358
      4    pred   comp  0.4      0.382     0.376
      5    pred   comp  0.5      0.405     0.395
      6    pred   comp  0.6      0.431     0.415
      7    pred   comp  0.7      0.464     0.439
      8    pred   comp  0.8      0.506     0.472
      9    pred   comp  0.9      0.574     0.528
      10   pred incomp  0.1      0.350     0.298
      ...
      
      (access the data.frame's columns/rows as usual)

# print.delta_funs works as expected

    Code
      print(delta_funs_obj)
    Output
      Type of Statistic: delta_funs
      
        Source Prob Quant_corr_comp Quant_corr_incomp Delta_incomp_comp
      1   pred  0.1           0.323             0.350             0.027
      2   pred  0.2           0.343             0.373             0.030
      3   pred  0.3           0.362             0.392             0.030
      4   pred  0.4           0.382             0.410             0.028
      5   pred  0.5           0.405             0.429             0.024
      6   pred  0.6           0.431             0.450             0.019
      7   pred  0.7           0.464             0.477             0.013
      8   pred  0.8           0.506             0.512             0.006
      9   pred  0.9           0.574             0.573            -0.001
        Avg_incomp_comp
      1           0.336
      2           0.358
      3           0.377
      4           0.396
      5           0.417
      6           0.441
      7           0.470
      8           0.509
      9           0.573
      
      (access the data.frame's columns/rows as usual)

# print.fit_stats works as expected

    Code
      print(fit_stats_obj)
    Output
      Type of Statistic: fit_stats
      
        ID Log_Like      AIC      BIC
      1  1  274.725 -543.451 -532.339
      2  2  234.308 -462.616 -451.505
      
      (access the data.frame's columns/rows as usual)

# print.stats_dm_list works as expected

    Code
      print(stats_dm_list_obj)
    Output
      Element 1, contains fit_stats
      
        ID Log_Like      AIC      BIC
      1  1  274.725 -543.451 -532.339
      2  2  234.308 -462.616 -451.505
      
      
      Element 2, contains quantiles
      
         ID Source Cond Prob Quant_corr Quant_err
      1   1    obs null  0.1      0.370     0.360
      2   1    obs null  0.2      0.390     0.370
      3   1    obs null  0.3      0.410     0.370
      4   1    obs null  0.4      0.430     0.370
      5   1    obs null  0.5      0.450     0.440
      6   1    obs null  0.6      0.470     0.510
      7   1    obs null  0.7      0.501     0.510
      8   1    obs null  0.8      0.550     0.510
      9   1    obs null  0.9      0.637     0.600
      10  1   pred null  0.1      0.367     0.367
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
             ID         Log_Like        AIC            BIC      
       Min.   :1.0   Min.   :382   Min.   :-926   Min.   :-900  
       1st Qu.:1.5   1st Qu.:396   1st Qu.:-865   1st Qu.:-839  
       Median :2.0   Median :409   Median :-804   Median :-778  
       Mean   :2.0   Mean   :421   Mean   :-827   Mean   :-800  
       3rd Qu.:2.5   3rd Qu.:440   3rd Qu.:-777   3rd Qu.:-751  
       Max.   :3.0   Max.   :470   Max.   :-750   Max.   :-724  
      
      N IDs: 3 

# summary.sum_dist works as expected

    Code
      print(summary_stats)
    Output
      Type of Statistic: sum_dist
      
      Dependent Variables:
          Source              Cond                Bin        P_corr     
       Length:10          Length:10          Min.   :1   Min.   :0.825  
       Class :character   Class :character   1st Qu.:2   1st Qu.:0.981  
       Mode  :character   Mode  :character   Median :3   Median :0.983  
                                             Mean   :3   Mean   :0.968  
                                             3rd Qu.:4   3rd Qu.:0.988  
                                             Max.   :5   Max.   :0.992  
      
      Sources: pred 

# summary.basic_stats works as expected

    Code
      print(summary_stats)
    Output
      Type of Statistic: basic_stats
      
      Dependent Variables:
         Mean_corr       Mean_err       SD_corr         SD_err          P_corr    
       Min.   :0.48   Min.   :0.44   Min.   :0.11   Min.   :0.094   Min.   :0.98  
       1st Qu.:0.48   1st Qu.:0.46   1st Qu.:0.11   1st Qu.:0.105   1st Qu.:0.98  
       Median :0.48   Median :0.47   Median :0.11   Median :0.113   Median :0.98  
       Mean   :0.48   Mean   :0.47   Mean   :0.12   Mean   :0.113   Mean   :0.98  
       3rd Qu.:0.49   3rd Qu.:0.48   3rd Qu.:0.12   3rd Qu.:0.121   3rd Qu.:0.98  
       Max.   :0.49   Max.   :0.48   Max.   :0.13   Max.   :0.131   Max.   :0.98  
      
      N IDs: 2 
      Sources: obs, pred 
      Conditions: null 

# summary.cafs works as expected

    Code
      print(summary_stats)
    Output
      Type of Statistic: cafs
      
      Dependent Variables:
           P_corr    
       Min.   :0.96  
       1st Qu.:0.98  
       Median :0.98  
       Mean   :0.98  
       3rd Qu.:0.98  
       Max.   :1.00  
      
      N IDs: 2 
      Sources: obs, pred 
      Conditions: null 
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
       Min.   :0.323   Min.   :0.350     Min.   :-0.00072   Min.   :0.336  
       1st Qu.:0.362   1st Qu.:0.392     1st Qu.: 0.01310   1st Qu.:0.377  
       Median :0.404   Median :0.429     Median : 0.02442   Median :0.417  
       Mean   :0.421   Mean   :0.441     Mean   : 0.01966   Mean   :0.431  
       3rd Qu.:0.463   3rd Qu.:0.477     3rd Qu.: 0.02813   3rd Qu.:0.470  
       Max.   :0.574   Max.   :0.573     Max.   : 0.02999   Max.   :0.573  
      
      Sources: pred 
      Probs: 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 

# summary.fit_stats works as expected

    Code
      print(summary_stats)
    Output
      Type of Statistic: fit_stats
      
      Dependent Variables:
          Log_Like        AIC            BIC      
       Min.   :234   Min.   :-543   Min.   :-532  
       1st Qu.:244   1st Qu.:-523   1st Qu.:-512  
       Median :255   Median :-503   Median :-492  
       Mean   :255   Mean   :-503   Mean   :-492  
       3rd Qu.:265   3rd Qu.:-483   3rd Qu.:-472  
       Max.   :275   Max.   :-463   Max.   :-452  
      
      N IDs: 2 

# summary.stats_dm_list works as expected

    Code
      print(summary_list)
    Output
      Summary of Element 1: quantiles
      
      Dependent Variables:
         Quant_corr      Quant_err    
       Min.   :0.323   Min.   :0.298  
       1st Qu.:0.375   1st Qu.:0.331  
       Median :0.419   Median :0.360  
       Mean   :0.431   Mean   :0.375  
       3rd Qu.:0.473   3rd Qu.:0.410  
       Max.   :0.574   Max.   :0.528  
      
      Sources: pred 
      Conditions: comp, incomp 
      Probs: 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 
      -------
      
      Summary of Element 2: cafs
      
      Dependent Variables:
           P_corr     
       Min.   :0.825  
       1st Qu.:0.981  
       Median :0.983  
       Mean   :0.968  
       3rd Qu.:0.988  
       Max.   :0.992  
      
      Sources: pred 
      Conditions: comp, incomp 
      Bins: 1, 2, 3, 4, 5 
      -------

