% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extdata.R
\docType{data}
\name{HerringSpawn}
\alias{HerringSpawn}
\title{Example Pacific Herring spawn survey database.}
\format{
MS Access databases with tables:
\describe{
\item{Location}{Spatial information for Locations.}
\item{Sections}{Spatial information for Sections.}
\item{PoolStd}{Spawn width for Beds.}
\item{SectionStd}{Spawn width for Sections.}
\item{RegionStd}{Spawn width for Regions.}
\item{tSSAllspawn}{Spawn survey information.}
\item{tSSMacPlant}{Macrocystis plant information.}
\item{tSSMacTrans}{Macrocystis transect information.}
\item{tSSStations}{Understory quadrat information.}
\item{tSSSurface}{Surface spawn information}
\item{tSSVegetation}{Understory spawn algae information.}
\item{tSSVegTrans}{Understory spawn transect information.}
}
}
\description{
Example Pacific Herring spawn survey database. This database contains a
subset of tables from the spawn survey database, and some additional tables
from other databases to ensure the examples work.
}
\note{
This is a 32-bit MS Access database, and it requires 32-bit R to access
the data. This example database only includes WCVI from 2010 to 2015.
}
\examples{
db_loc <- system.file("extdata", "HerringSpawn.mdb", package = "SpawnIndex")
db_loc
library(odbc)
library(DBI)
access_db <- dbConnect(
  drv = odbc(),
  .connection_string = paste(
    "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=", db_loc,
    sep = ""
  )
)
dbListTables(conn = access_db)
dbDisconnect(conn = access_db)
}
\seealso{
\code{\link{load_area_data}} \code{\link{load_all_spawn}}
\code{\link{get_width}} \code{\link{calc_surf_index}}
\code{\link{calc_macro_index}} \code{\link{calc_under_index}}
}
\concept{raw data}
