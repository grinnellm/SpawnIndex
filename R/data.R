#' Understory spawn width correction factors.
#'
#' Spawn width correction factors for Pacific Herring understory spawn surveys
#' by region and year.
#'
#' @format Tibble with 12 rows and 8 columns (one for year, and one for each
#'   regions).
#' @note Adjust spawn width for affected understory spawn surveys by multiplying
#'   the observed width by the corresponding correction fator for the affected
#'   years.
#' @docType data
#' @usage data(underWidthFac)
#' @seealso \code{\link{CalcUnderSpawn}}
#' @examples
#' data(underWidthFac)
"underWidthFac"

#' Example Pacific Herring spawn survey database.
#'
#' Example Pacific Herring spawn survey database. This is a subset of the actual
#' spawn survey database, and some additional data so that examples run.
#'
#' @format MS Access databases.
#' @docType data
#' @seealso \code{\link{LoadAreaData}} \code{\link{CalcSurfSpawn}}
#'   \code{\link{CalcUnderSpawn}} \code{\link{CalcMacroSpawn}}
#'
system.file("extdata", "HerringSpawn.mdb", package = "SpawnIndex")
