# print.coefs_dm works as expected

    Code
      print(coefs, round_digits = 2)
    Output
      Object Type: coefs_dm
      
        ID  muc    b non_dec sd_non_dec  tau    A alpha
      1  1 4.70 0.45    0.34       0.03 0.03 0.10  6.61
      2  2 4.70 0.39    0.30       0.04 0.10 0.09  5.04
      3  3 5.96 0.63    0.32       0.01 0.11 0.19  3.33
      
      (access the data.frame's columns/rows as usual)

---

    Code
      print(coefs, round_digits = 2, some = TRUE, print_rows = 3)
    Output
      Object Type: coefs_dm
      
        ID  muc    b non_dec sd_non_dec  tau    A alpha
      4  4 4.70 0.45    0.34       0.03 0.03 0.10  6.61
      7  7 4.70 0.45    0.34       0.03 0.03 0.10  6.61
      9  9 5.96 0.63    0.32       0.01 0.11 0.19  3.33
      ...
      
      (access the data.frame's columns/rows as usual)

# summary.coefs_dm works as expected

    Code
      print(summary_coefs)
    Output
      Object Type: coefs_dm
      
      Parameters:
            muc             b            non_dec        sd_non_dec    
       Min.   :4.70   Min.   :0.391   Min.   :0.297   Min.   :0.0120  
       1st Qu.:4.70   1st Qu.:0.418   1st Qu.:0.307   1st Qu.:0.0221  
       Median :4.70   Median :0.446   Median :0.318   Median :0.0323  
       Mean   :5.12   Mean   :0.488   Mean   :0.319   Mean   :0.0285  
       3rd Qu.:5.33   3rd Qu.:0.536   3rd Qu.:0.330   3rd Qu.:0.0368  
       Max.   :5.96   Max.   :0.626   Max.   :0.341   Max.   :0.0413  
            tau               A              alpha     
       Min.   :0.0322   Min.   :0.0893   Min.   :3.33  
       1st Qu.:0.0667   1st Qu.:0.0956   1st Qu.:4.19  
       Median :0.1012   Median :0.1019   Median :5.04  
       Mean   :0.0812   Mean   :0.1286   Mean   :4.99  
       3rd Qu.:0.1056   3rd Qu.:0.1483   3rd Qu.:5.82  
       Max.   :0.1101   Max.   :0.1947   Max.   :6.61  
      
      N IDs: 3 

