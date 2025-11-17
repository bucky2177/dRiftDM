# print.coefs_dm works as expected

    Code
      print(coefs, round_digits = 2)
    Output
      Object Type: coefs_dm
      
        ID  muc    b non_dec sd_non_dec  tau    A alpha
      1  1 4.55 0.45    0.34       0.03 0.04 0.10  7.39
      2  2 4.17 0.39    0.29       0.04 0.07 0.08  7.74
      3  3 5.65 0.58    0.32       0.01 0.10 0.18  3.84
      
      (access the data.frame's columns/rows as usual)

---

    Code
      print(coefs, round_digits = 2, some = TRUE, print_rows = 3)
    Output
      Object Type: coefs_dm
      
        ID  muc    b non_dec sd_non_dec  tau    A alpha
      4  4 4.55 0.45    0.34       0.03 0.04 0.10  7.39
      7  7 4.55 0.45    0.34       0.03 0.04 0.10  7.39
      9  9 5.65 0.58    0.32       0.01 0.10 0.18  3.84
      ...
      
      (access the data.frame's columns/rows as usual)

# summary.coefs_dm works as expected

    Code
      print(summary_coefs)
    Output
      Object Type: coefs_dm
      
      Parameters:
            muc             b            non_dec        sd_non_dec    
       Min.   :4.17   Min.   :0.387   Min.   :0.292   Min.   :0.0142  
       1st Qu.:4.36   1st Qu.:0.416   1st Qu.:0.306   1st Qu.:0.0232  
       Median :4.55   Median :0.446   Median :0.319   Median :0.0322  
       Mean   :4.79   Mean   :0.472   Mean   :0.317   Mean   :0.0289  
       3rd Qu.:5.10   3rd Qu.:0.515   3rd Qu.:0.330   3rd Qu.:0.0363  
       Max.   :5.65   Max.   :0.585   Max.   :0.341   Max.   :0.0403  
            tau               A              alpha     
       Min.   :0.0350   Min.   :0.0785   Min.   :3.84  
       1st Qu.:0.0509   1st Qu.:0.0906   1st Qu.:5.61  
       Median :0.0668   Median :0.1026   Median :7.39  
       Mean   :0.0676   Mean   :0.1202   Mean   :6.32  
       3rd Qu.:0.0839   3rd Qu.:0.1411   3rd Qu.:7.56  
       Max.   :0.1010   Max.   :0.1795   Max.   :7.74  
      
      N IDs: 3 

