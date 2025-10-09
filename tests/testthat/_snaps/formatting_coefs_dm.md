# print.coefs_dm works as expected

    Code
      print(coefs, round_digits = 2)
    Output
      Object Type: coefs_dm
      
        ID  muc    b non_dec sd_non_dec  tau    A alpha
      1  1 4.73 0.44    0.34       0.03 0.03 0.11  6.50
      2  2 4.63 0.38    0.30       0.04 0.10 0.09  5.24
      3  3 5.79 0.59    0.32       0.01 0.09 0.18  3.30
      
      (access the data.frame's columns/rows as usual)

---

    Code
      print(coefs, round_digits = 2, some = TRUE, print_rows = 3)
    Output
      Object Type: coefs_dm
      
        ID  muc    b non_dec sd_non_dec  tau    A alpha
      4  4 4.73 0.44    0.34       0.03 0.03 0.11   6.5
      7  7 4.73 0.44    0.34       0.03 0.03 0.11   6.5
      9  9 5.79 0.59    0.32       0.01 0.09 0.18   3.3
      ...
      
      (access the data.frame's columns/rows as usual)

# summary.coefs_dm works as expected

    Code
      print(summary_coefs)
    Output
      Object Type: coefs_dm
      
      Parameters:
            muc             b            non_dec        sd_non_dec    
       Min.   :4.63   Min.   :0.384   Min.   :0.298   Min.   :0.0143  
       1st Qu.:4.68   1st Qu.:0.411   1st Qu.:0.310   1st Qu.:0.0237  
       Median :4.73   Median :0.438   Median :0.322   Median :0.0332  
       Mean   :5.05   Mean   :0.470   Mean   :0.321   Mean   :0.0298  
       3rd Qu.:5.26   3rd Qu.:0.513   3rd Qu.:0.333   3rd Qu.:0.0375  
       Max.   :5.79   Max.   :0.588   Max.   :0.344   Max.   :0.0418  
            tau               A             alpha     
       Min.   :0.0282   Min.   :0.094   Min.   :3.30  
       1st Qu.:0.0597   1st Qu.:0.101   1st Qu.:4.27  
       Median :0.0911   Median :0.107   Median :5.24  
       Mean   :0.0747   Mean   :0.127   Mean   :5.01  
       3rd Qu.:0.0980   3rd Qu.:0.143   3rd Qu.:5.87  
       Max.   :0.1048   Max.   :0.179   Max.   :6.50  
      
      N IDs: 3 

