.onAttach <- function(libname, pkgname) {
  # Welcome message
  packageStartupMessage(
    "This is SpawnIndex version ", utils::packageVersion("SpawnIndex"), "."
  )
  # Check for 32-bit R
  if (.Machine$sizeof.pointer != 4) {
    packageStartupMessage("Note: SpawnIndex requires 32-bit R.")
  }
}
