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
#' @seealso \code{\link{load_area_data}} \code{\link{load_all_spawn}}
#'   \code{\link{load_width}} \code{\link{calc_surf_index}}
#'   \code{\link{calc_macro_index}} \code{\link{calc_under_index}}
#' @family raw data
#' @note This is a 32-bit MS Access database; 32-bit R is required to access the
#'   data. This example database contains West Coast of Vancouver Island (WCVI)
#'   spawn survey data from 2010 to 2015.
#' @examples
#' db_loc <- system.file("extdata", "HerringSpawn.mdb", package = "SpawnIndex")
#' library(odbc)
#' library(DBI)
#' access_db <- dbConnect(
#'   drv = odbc(),
#'   .connection_string = paste(
#'     "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=", db_loc,
#'     sep = ""
#'   )
#' )
#' dbListTables(conn = access_db)
#' dbDisconnect(conn = access_db)
NULL

#' Pacific Herring Section polygons.
#'
#' Pacific Herring Section polygons, with information to aggregate Sections by
#' Statistical Area and Region.
#'
#' @format Shapefile. Cluster of files that define the spatial boundaries of
#'   Pacific Herring Sections, Statistical Areas, and Regions as polygons.
#'   Coordinate reference system: these polygons are projected in BC Albers (NAD
#'   83).
#' @docType data
#' @name HerringSections
#' @seealso \code{\link{load_sections}}
#' @family raw data
#' @note These are not official Section polygons such as might be used to
#'   ascribe catch to a given area for management purposes; these polygons are
#'   only to be used for making nice maps. Do not use these polygons for
#'   navigation.
#' @examples
#' sections_files <- system.file("extdata", "Sections", package = "SpawnIndex")
#' library(sf)
#' st_read(dsn = sections_files, layer = "HerringSections", quiet = TRUE)
NULL
