# print.drift_dm_fits works as expected

    Code
      print(dm_fits_ids)
    Output
      Fit procedure name: test_case_saved
      Fitted model type: ratcliff_dm, drift_dm
      Time of (last) call: 2024-Dezember-12_13-20
      N Individuals: 2 

# summary.drift_dm_fits and print

    Code
      print(sum_obj)
    Output
      Fit procedure name: test_case_saved
      N Individuals: 2 
      
      Parameter summary: null 
                muc     b non_dec
      mean    3.220 0.598   0.311
      std_err 0.131 0.008   0.002
      
      
      Parameter space:
            muc   b non_dec
      lower   1 0.3     0.1
      upper   5 0.8     0.5
      
      -------
      Fitted model type: ratcliff_dm, drift_dm
      Time of (last) call: 2024-Dezember-12_13-20

