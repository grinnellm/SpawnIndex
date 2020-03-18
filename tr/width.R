require(SpawnIndex)
require(tidyverse)
require(RODBC)

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
barWidth <- GetWidth(where = widthLoc, a = areas)

# Tables etc for surface data
surfLoc <- list(
  loc = dbLoc, db = "HerringSpawn.mdb",
  fns = list(surface = "tSSSurface", allSpawn = "tSSAllspawn")
)

# Get surface widths
GetSurfWidth <- function(where, a, widths) {
  # Establish connection with access
  accessDB <- odbcConnectAccess(access.file = file.path(
    where$loc,
    where$db
  ))
  # Get a small subset of area data
  areasSm <- a %>%
    select(Region, StatArea, Section, LocationCode, Pool) %>%
    distinct() %>%
    as_tibble()
  # Load all spawn
  spawn <- sqlFetch(channel = accessDB, sqtable = where$fns$allSpawn) %>%
    rename(
      LocationCode = Loc_Code, SpawnNumber = Spawn_Number, WidthObs = Width
    ) %>%
    filter(LocationCode %in% a$LocationCode) %>%
    select(Year, LocationCode, SpawnNumber, WidthObs) %>%
    as_tibble()
  # Bind the tables
  res <- spawn %>%
    left_join(y = areasSm, by = "LocationCode") %>%
    # Merge widths incrementally: region
    left_join(
      y = widths %>% select(Region, WidthReg) %>% distinct(),
      by = "Region"
    ) %>%
    # Merge widths incrementally: section
    left_join(
      y = widths %>% select(Region, Section, WidthSec) %>% distinct(),
      by = c("Region", "Section")
    ) %>%
    # Merge widths incrementally: pool
    left_join(
      y = widths %>% select(Region, Section, Pool, WidthPool) %>% distinct(),
      by = c("Region", "Section", "Pool")
    ) %>%
    unite(col = Group, Section, Pool, sep = "-") %>%
    select(
      Year, Region, StatArea, Group, LocationCode, SpawnNumber, WidthReg,
      WidthSec, WidthPool, WidthObs
    ) %>%
    arrange(Year, Region, StatArea, Group, LocationCode, SpawnNumber)
  # Close the connection
  RODBC::odbcClose(accessDB)
  # Return the table
  return(res)
} # End GetSurfWidth function

# Load surface widths
surfWidth <- GetSurfWidth(where = surfLoc, a = areas, widths = barWidth)

# Plot observed width with lines for median pool, section, and region width
poolPlot <- ggplot(data = surfWidth, mapping = aes(y = Group, group = Group)) +
  labs(x = "Width", y = "Section-Pool") +
  geom_point(
    mapping = aes(x = WidthObs), alpha = 0.5, na.rm = TRUE, size = 4
  ) +
  geom_vline(
    mapping = aes(xintercept = WidthReg), colour = "red", na.rm = TRUE
  ) +
  geom_point(
    mapping = aes(x = WidthSec), colour = "blue", na.rm = TRUE, size = 3
  ) +
  geom_point(
    mapping = aes(x = WidthPool), colour = "green", na.rm = TRUE, size = 3
  ) +
  expand_limits(x = 0) +
  theme_bw()
print(poolPlot)
