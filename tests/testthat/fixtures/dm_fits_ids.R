a_model <- ratcliff_dm(t_max = 1, dt = 0.01, dx = 0.01)
subject_1 <- simulate_data(drift_dm_obj = a_model, n = 300, seed = 1)
subject_1$ID <- 1
subject_2 <- simulate_data(drift_dm_obj = a_model, n = 300, seed = 2)
subject_2$ID <- 2
data_both <- rbind(subject_1, subject_2)

estimate_model_ids(
  drift_dm_obj = a_model,
  obs_data_ids = data_both,
  lower = c(1, 0.3, 0.1), upper = c(5, 0.8, 0.5),
  fit_procedure_name = "test_case_saved",
  seed = 1,
  fit_dir = test_path("fixtures", "dm_fits_ids"),
  force_refit = TRUE,
  verbose = 1,
)
