############
# useful functions to access when testing

default_solver_prms <- function() {
  return(
    default_solver_prms = c(
      "sigma" = 1, "t_max" = 3,
      "dt" = .001, "dx" = .001,
      "nt" = 3000, "nx" = 2000
    )
  )
}

default_solver <- function() {
  return("kfe")
}
