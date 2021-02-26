.onAttach <- function(libname, pkgname) {
  # Version
  version <- as.vector(read.dcf("DESCRIPTION")[, "Version"])
  # Welcome
  packageStartupMessage("Welcome to SpawnIndex v", version)
}

.onLoad <- function(libname, pkgname) {
  # Warning if R is not 32-bit
  if (.Machine$sizeof.pointer != 4) warning("32-bit R required", call. = FALSE)
}
