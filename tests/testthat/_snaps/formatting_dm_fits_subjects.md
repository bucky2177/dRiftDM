# drift_dm summary and print format

    Code
      print(dm_fits_subjects)
    Output
      Fit procedure name: test_case_saved
      Fitted model type: ratcliff_dm_simple, drift_dm
      Time of (last) call: 2023-12-04 10:12:28
      N Subjects: 2 

---

    Code
      print(sum_obj)
    Output
      Fit procedure name: test_case_saved
      N Subjects: 2 
      
      Parameter summary:
              muc     b non_dec log_like      AIC      BIC
      means 3.234 0.613   0.306  250.636 -495.271 -484.160
      sds   0.151 0.042   0.017   29.964   59.927   59.927
      errs  0.106 0.030   0.012   21.188   42.375   42.375
      
      Parameter space:
            muc   b non_dec
      lower   1 0.3     0.1
      upper   5 0.8     0.5
      
      -------
      Fitted model type: ratcliff_dm_simple, drift_dm
      Time of (last) call: 2023-12-04 10:12:28

