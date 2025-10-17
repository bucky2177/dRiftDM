############
# useful functions to access when testing

default_solver_prms <- function() {
  return(
    default_solver_prms = c(
      "sigma" = 1,
      "t_max" = 3,
      "dt" = .001,
      "dx" = .001,
      "nt" = 3000,
      "nx" = 2000
    )
  )
}

default_solver <- function() {
  return("kfe")
}

#############
# Create Models to Avoid Creating "a_model" all the Time --------

ssp_dummy <- ssp_dm(obs_data = dRiftDM::ssp_synth_data, dx = .001, dt = .001)
ssp_dummy <- re_evaluate_model(ssp_dummy)

dmc_dummy <- dmc_dm(obs_data = dRiftDM::dmc_synth_data, dx = .001, dt = .001)
dmc_dummy <- re_evaluate_model(dmc_dummy)

ratcliff_dummy <- ratcliff_dm(
  obs_data = dRiftDM::ratcliff_synth_data,
  dx = .001,
  dt = .001
)
ratcliff_dummy <- re_evaluate_model(ratcliff_dummy)


#############
# suppress lifecycle warning (to easily catch it, see test-core-wrapper)
suppress_lifecycle_deprecated <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (inherits(w, "lifecycle_warning_deprecated")) {
        invokeRestart("muffleWarning")
      }
    }
  )
}
