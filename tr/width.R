require(SpawnIndex)

dbLoc <- system.file("extdata", package = "SpawnIndex")

areaLoc <- list(
  loc = dbLoc, db = "HerringSpawn.mdb",
  fns = list(sections = "Sections", locations = "Location")
)

areas <- LoadAreaData(reg = "WCVI", where = areaLoc)

whereLoc <- list(
  loc = dbLoc, db = "HerringSpawn.mdb",
  fns = list(
    regionStd = "RegionStd", sectionStd = "SectionStd", poolStd = "PoolStd",
    surface = "tSSSurface", allSpawn = "tSSAllspawn"
  )
)

GetWidth <- function(where, sec) {

}

GetWidth(where = whereLoc, sec == 23)
