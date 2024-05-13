.onAttach <- function(libname, pkgname) {
  if (requireNamespace("cowsay", quietly = TRUE)) {
    msg <- paste(
      "Welcome to dRiftDM", utils::packageVersion("dRiftDM"),
      "\nThis is a first version...",
      "\nPlease report any bugs/unexpected ",
      "\nbehavior to koob@uni-bremen.de"
    )
    packageStartupMessage(cowsay::say(
      what = msg,
      by = "cow", type = "string"
    ))
  }
}
