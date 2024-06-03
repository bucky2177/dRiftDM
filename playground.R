
library("devtools")
library("tictoc")
library(tictoc)
load_all()
rm(list = ls())

a_model = dmc_dm(dt = 0.001, dx = 0.001)

a_model$prms_model[["alpha"]]=1.5
a_model$prms_model[["tau"]]=0.01

all_comps = component_shelf()
#a_model = set_comp_funs(a_model, comp_funs = list(x_fun = all_comps$x_dirac_0))
a_model = set_comp_funs(a_model, comp_funs = list(nt_fun = all_comps$nt_constant))
a_model = set_comp_funs(a_model, comp_funs = list(x_fun = all_comps$x_uniform))
a_model$prms_model[["range_start"]] = 0.1
tic()
a_model=re_evaluate_model(a_model)
toc()

min(unlist(a_model$pdfs))
plot(a_model$pdfs$comp$pdf_u+a_model$pdfs$comp$pdf_l,type='l',col='red')
points(a_model$pdfs$comp$pdf_l,type='l',col='orange')
