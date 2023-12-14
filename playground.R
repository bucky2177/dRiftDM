
library("devtools")
library("tictoc")
load_all()
rm(list = ls())
# Hallo Vali! :)

a_model = ratcliff_dm(dt = 0.05, dx = 0.1)
data = data.frame(c(Subject = 1, ratcliff_data))
estimate_model_subjects(a_model, data, c(1, 0.3, 0.1), c(5, 0.9, 0.5), fit_procedure_name = "bla")
obj = load_fits_subjects()
