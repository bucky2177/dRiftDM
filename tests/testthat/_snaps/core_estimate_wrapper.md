# estimate_model_subject and load_fits_subjects works as expected

    Code
      expect_warning(estimate_model_subjects(drift_dm_obj = a_model,
        obs_data_subject = data_both, lower = c(1), upper = c(5), fit_procedure_name = "test_case_1",
        seed = 1, fit_dir = test_path("temp_fits"), force_refit = TRUE, verbose = 1, ),
      "obs_data in drift_dm_obj will be ignored")
    Output
      Estimating model for subject: 1 
      INFO: Running differential evolution
      [33mINFO: Estimation gave muc=3.255, b=0.6, non_dec=0.3 
      	with -log_like_val of -267.6199 [0m
      Estimating model for subject: 2 
      INFO: Running differential evolution
      [33mINFO: Estimation gave muc=3.058, b=0.6, non_dec=0.3 
      	with -log_like_val of -224.0664 [0m

---

    Code
      estimate_model_subjects(drift_dm_obj = a_model, obs_data_subject = data_both,
        lower = c(1), upper = c(5), fit_procedure_name = "test_case_2", folder_name = "test_case_no_2",
        seed = 2, force_refit = TRUE, fit_dir = test_path("temp_fits"), verbose = 0)
    Output
      Estimating model for subject: 1 
      Estimating model for subject: 2 

---

    Code
      estimate_model_subjects(drift_dm_obj = a_model, obs_data_subject = data_all,
        lower = c(1), upper = c(5), fit_procedure_name = "test_case_2", folder_name = "test_case_no_2",
        seed = 2, force_refit = FALSE, fit_dir = test_path("temp_fits"), verbose = 0)
    Output
      Estimating model for subject: 3 

