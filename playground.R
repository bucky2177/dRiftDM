
library("devtools")
library("tictoc")
load_all()
rm(list = ls())
# Hallo Vali! :)
# compare with kfe by Thomas
a_dmc_model = dmc_dm(t_max = 1000, dt = 5, dx = .1, sigma = 4)
a_dmc_model = set_model_prms(a_dmc_model, c(0.5, 75, 300, 30, 50, 20, 2))
a_dmc_model$comp_funs$x_fun = function(drift_dm_obj, x_vec, one_cond) {
  alpha <- drift_dm_obj$prms_model[["alpha"]]
  nx <- drift_dm_obj$prms_solve[["nx"]]
  xx <- seq(0, 1, length.out = nx + 1)
  x <- stats::dbeta(xx, alpha, alpha)
  x <- x / sum(x)
  return(x)
}
a_dmc_model$comp_funs$x_fun(a_dmc_model, c(1,2), "comp")


test = get_pdfs(a_dmc_model, "comp", "kfe")


a_dmc_model = dmc_dm(t_max = 1000, dt = 5, dx = .1, sigma = 4)
a_dmc_model = set_model_prms(a_dmc_model, c(0.5, 75, 300, 30, 50, 20, 2))
a_dmc_model$comp_funs$x_fun = function(drift_dm_obj, x_vec, one_cond) {
  alpha <- drift_dm_obj$prms_model[["alpha"]]
  nx <- drift_dm_obj$prms_solve[["nx"]]
  xx <- seq(0, 1, length.out = nx + 1)
  x <- stats::dbeta(xx, alpha, alpha) *100
  return(x)
}

a_dmc_model$comp_funs$x_fun(a_dmc_model, c(1,2), "comp")

test2 = get_pdfs(a_dmc_model, "comp", "kfe")
plot(test[[1]])
plot(test2[[1]])
