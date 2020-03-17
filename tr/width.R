require(SpawnIndex)

# Region of interest
region <- "WCVI"

# Database location
dbLoc <- system.file("extdata", package = "SpawnIndex")

# Tables etc for area info
areaLoc <- list(
  loc = dbLoc, db = "HerringSpawn.mdb",
  fns = list(sections = "Sections", locations = "Location")
)

# Load area data
areas <- LoadAreaData(reg = region, where = areaLoc)

# Tables etc for median widths
widthLoc <- list(
  loc = dbLoc, db = "HerringSpawn.mdb",
  fns = list(
    regionStd = "RegionStd", sectionStd = "SectionStd", poolStd = "PoolStd"
  )
)

# Median widths
medWidth <- GetWidth(where = widthLoc, a = areas)
