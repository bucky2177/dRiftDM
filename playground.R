
library("devtools")
library("tictoc")
library(tictoc)
load_all()
rm(list = ls())

a_model = dmc_dm(dt = 0.001, dx = 0.001, t_max = 1)
a_model = set_comp_funs(a_model, comp_funs = list(x_fun = component_shelf()$x_dirac_0))

a_model = set_solver_settings(a_model, new_solver_vals = c("solver" = "im_zero"))

tic()
a_model = re_evaluate_model(a_model)
toc()

pdf_u = a_model$pdfs$comp$pdf_u
pdf_l = a_model$pdfs$comp$pdf_l

head(pdf_u)
tail(pdf_u)
head(pdf_l)
tail(pdf_l)
plot(pdf_u, ty = "l")
points(pdf_l, ty = "l", col = "red")



a_model = set_solver_settings(a_model, new_solver_vals = c("solver" = "kfe"))

tic()
a_model = re_evaluate_model(a_model)
toc()

pdf_u = a_model$pdfs$comp$pdf_u
pdf_l = a_model$pdfs$comp$pdf_l

plot(pdf_u, ty = "l")
points(pdf_l, ty = "l", col = "red")
