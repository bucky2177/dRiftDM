
library("devtools")
library("tictoc")
library("microbenchmark")
library(tictoc)
load_all()
rm(list = ls())

a_model = dmc_dm(dt = 0.002, dx = 0.002)
some_data = simulate_data(a_model, 100000)
a_model = set_obs_data(a_model, some_data)
plot_stats(a_model, type = "cafs", n_bins_cafs = 4,
           x_lab_cafs = "foo", y_lab_cafs = "bar",
           x_lim_cafs = c(-1, 5), y_lim_cafs = c(0.5, 1),
           line_cols_cafs = c("green", "blue"))

plot_stats(a_model, type = "quantiles",
           probs_quantiles = seq(0.3, 0.6, 0.1),
           to_plot_quantiles = "Quant_Corr",
           x_lab_quantiles = "foo", y_lab_quantiles = "bar",
           x_lim_quantiles = c(0.2, 0.8), y_lim_quantiles = c(0.1, 0.9),
           line_cols_quantiles = c("green", "blue"))


estimate_model(a_model, c(3, 0.5, 0.2, 0.01, 0.02, 0.05, 3),
               upper = c(5, 0.8, 0.4, 0.03, 0.05, 0.2, 10),
               verbose = 2, use_de_optim = F, use_nmkb = T)

microbenchmark(all = re_evaluate_model(a_model)
)



data = simulate_data(a_model, 100)
a_model = set_obs_data(a_model, data)
a_model = set_solver_settings(a_model, c("dx", "dt"), c(0.005, 0.005))


tic()
a_model = re_evaluate_model(a_model)
toc()

drift_dm_obj = a_model


microbenchmark(test1 = lapply(1:10000, function(x) a_model$prms_model),
               test2 = sapply(1:10000, function(x) a_model$prms_model ,simplify = F, USE.NAMES = T)
)

tic()
a = dmcSim()
toc()


