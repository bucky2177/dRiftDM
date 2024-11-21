.onAttach <- function(libname, pkgname) {
  msg <- paste(
    "Welcome to dRiftDM", utils::packageVersion("dRiftDM"),
    "\nPlease report any bugs or unexpected behavior"
  )
  if (requireNamespace("cowsay", quietly = TRUE)) {
    packageStartupMessage(cowsay::say(
      what = msg,
      by = "cow", type = "string"
    ))
  } else {
    packageStartupMessage(msg)
  }
}
