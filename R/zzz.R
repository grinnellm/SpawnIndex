.onAttach <- function(libname,
                      pkgname) {
  # Welcome message
  packageStartupMessage(
    "This is SpawnIndex version ", utils::packageVersion("SpawnIndex"), "."
  )
}
