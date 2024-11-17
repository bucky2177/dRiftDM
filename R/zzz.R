.onAttach <- function(libname, pkgname) {
  msg <- paste(
    "Welcome to dRiftDM", utils::packageVersion("dRiftDM"),
    "\nThis is a first version...",
    "\nPlease report any bugs or",
    "\nunexpected behavior"
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
