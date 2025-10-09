# print.fits_ids_dm (old) works as expected

    Code
      print(drift_dm_fits)
    Output
      Fit procedure name: test_case_saved
      Fitted model type: ratcliff_dm, drift_dm
      Time of (last) call: 2025-Februar-24_09-47
      N Individuals: 2

# print.fits_ids_dm (new) works as expected

    Code
      print(drift_dm_fits)
    Output
      Fit approach: separately - classical
      Fitted model type: dmc_dm, drift_dm
      Optimizer: nmkb 
      Convergence: TRUE 
      N Individuals: 3 
      Average Trial Numbers:
       168 trials comp; 168 trials incomp

# summary.fits_ids_dm (old) and print

    Code
      print(sum_obj)
    Output
      Fit Procedure Name: test_case_saved
      N Individuals: 2 
      
      Parameter Summary: null 
                muc     b non_dec
      mean    3.220 0.598   0.311
      std_err 0.131 0.008   0.002
      
      Parameter Space:
            muc   b non_dec
      lower   1 0.3     0.1
      upper   5 0.8     0.5
      
      -------
      Fitted Model Type: ratcliff_dm, drift_dm
      Time of (Last) Call: 2025-Februar-24_09-47

