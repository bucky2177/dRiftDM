# Create and Save Models to Avoid Createing "a_model" all the TIme --------

ssp_dm <- ssp_dm()
ssp_dm <- re_evaluate_model(ssp_dm)

dmc_dm <- dmc_dm(obs_data = dRiftDM::dmc_synth_data)
dmc_dm <- re_evaluate_model(dmc_dm)


basic_dm <- ratcliff_dm()
basic_dm <- re_evaluate_model(basic_dm)

saveRDS(ssp_dm, test_path("fixtures", "ssp.rds"))
saveRDS(dmc_dm, test_path("fixtures", "dmc.rds"))
saveRDS(basic_dm, test_path("fixtures", "ratcliff.rds"))
