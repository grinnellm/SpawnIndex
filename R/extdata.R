#' Example Pacific Herring spawn survey database.
#'
#' Example Pacific Herring spawn survey database. This database contains a
#' subset of tables from the spawn survey database, and some additional tables
#' from other databases to ensure the examples work.
#'
#' @format MS Access databases with tables:
#' \describe{
#'   \item{Location}{Spatial information for Locations.}
#'   \item{Sections}{Spatial information for Sections.}
#'   \item{PoolStd}{Spawn width for Beds.}
#'   \item{SectionStd}{Spawn width for Sections.}
#'   \item{RegionStd}{Spawn width for Regions.}
#'   \item{tSSAllspawn}{Spawn survey information.}
#'   \item{tSSMacPlant}{Macrocystis plant information.}
#'   \item{tSSMacTrans}{Macrocystis transect information.}
#'   \item{tSSStations}{Understory quadrat information.}
#'   \item{tSSSurface}{Surface spawn information}
#'   \item{tSSVegetation}{Understory spawn algae information.}
#'   \item{tSSVegTrans}{Understory spawn transect information.}
#' }
#' @docType data
#' @name HerringSpawn
#' @seealso \code{\link{LoadAreaData}} \code{\link{LoadAllSpawn}}
#'   \code{\link{GetWidth}} \code{\link{CalcSurfSpawn}}
#'   \code{\link{CalcMacroSpawn}} \code{\link{CalcUnderSpawn}}
#' @note This is a 32-bit MS Access database, and it requires 32-bit R to access
#'   the data. This example database only includes WCVI from 2010 to 2015.
#' @examples
#' dbLoc <- system.file("extdata", "HerringSpawn.mdb", package = "SpawnIndex")
#' dbLoc
#' library(odbc)
#' library(DBI)
#' accessDB <- dbConnect(
#'   drv = odbc(),
#'   .connection_string = paste(
#'     "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=",
#'     dbLoc,
#'     sep = ""
#'   )
#' )
#' dbListTables(conn = accessDB)
#' dbDisconnect(conn = accessDB)
NULL
