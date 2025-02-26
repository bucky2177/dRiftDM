# print.coefs_dm works as expected

    Code
      print(coefs, round_digits = 2)
    Output
      Object Type: coefs_dm
      
        ID  muc    b non_dec
      1  1 3.35 0.59    0.31
      2  2 3.09 0.61    0.31
      
      (access the data.frame's columns/rows as usual)

# summary.coefs_dm works as expected

    Code
      print(summary_coefs)
    Output
      Object Type: coefs_dm
      
      Parameters:
            muc             b            non_dec     
       Min.   :3.09   Min.   :0.590   Min.   :0.309  
       1st Qu.:3.15   1st Qu.:0.594   1st Qu.:0.310  
       Median :3.22   Median :0.598   Median :0.311  
       Mean   :3.22   Mean   :0.598   Mean   :0.311  
       3rd Qu.:3.29   3rd Qu.:0.602   3rd Qu.:0.312  
       Max.   :3.35   Max.   :0.606   Max.   :0.313  
      
      N IDs: 2 

