.onAttach <- function(libname, pkgname) {
  if (requireNamespace("cowsay", quietly = TRUE)) {
    packageStartupMessage(cowsay::say(
      what = "Welcome to dRiftDM",
      by = "cow", type = "string"
    ))
  }
}
