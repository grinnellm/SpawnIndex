# Packages
require(SpawnIndex)
require(tidyverse)
require(RODBC)
require(scales)

# Region of interest
region <- "HG"

# Figure path
figPath <- file.path("tr", "cache")

# Figure width
figWidth <- 6.5

# Make the directory if required
if (!dir.exists(figPath)) dir.create(path = figPath)

# Database location
# dbLoc <- system.file("extdata", package = "SpawnIndex")
dbLoc <- file.path("..", "Data", "Local")
# dbName <- "HerringSpawn.mdb"
dbName <- "HSA_Program_v6.2.mdb"

# Tables etc for area info
areaLoc <- list(
  loc = dbLoc, db = dbName,
  fns = list(sections = "Sections", locations = "Location")
)

# Load area data
areas <- LoadAreaData(reg = region, where = areaLoc, quiet = TRUE)

# Tables etc for median widths
widthLoc <- list(
  loc = dbLoc, db = dbName,
  fns = list(
    regionStd = "RegionStd", sectionStd = "SectionStd", poolStd = "PoolStd"
  )
)

# Median widths
barWidth <- GetWidth(where = widthLoc, a = areas)

# Tables etc for surface data
surfLoc <- list(loc = dbLoc, db = dbName, fns = list(allSpawn = "tSSAllspawn"))

# Get surface widths
GetSurfWidth <- function(where, a, widths) {
  # Establish connection with access
  accessDB <- odbcConnectAccess(access.file = file.path(where$loc, where$db))
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
    # unite(col = Group, Section, Pool, sep = "-") %>%
    mutate(Pool = as.character(Pool), Survey = "Surface") %>%
    select(
      Survey, Year, Region, StatArea, Section, Pool, LocationCode, SpawnNumber,
      WidthReg, WidthSec, WidthPool, WidthObs
    ) %>%
    arrange(Year, Region, StatArea, Section, Pool, LocationCode, SpawnNumber)
  # Close the connection
  RODBC::odbcClose(accessDB)
  # Return the table
  return(res)
} # End GetSurfWidth function

# Load surface widths
surfWidth <- GetSurfWidth(where = surfLoc, a = areas, widths = barWidth)

# Plot observed width with lines for median pool, section, and region width
poolPlot <- ggplot(data = surfWidth, mapping = aes(y = Pool)) +
  geom_boxplot(
    mapping = aes(x = WidthObs), outlier.alpha = 0.5, outlier.size = 1,
    na.rm = TRUE
  ) +
  geom_vline(
    mapping = aes(xintercept = WidthReg), colour = "red", na.rm = TRUE,
    alpha = 0.5
  ) +
  geom_vline(
    mapping = aes(xintercept = WidthSec), colour = "blue", na.rm = TRUE,
    alpha = 0.5
  ) +
  geom_point(
    mapping = aes(x = WidthPool), colour = "green", na.rm = TRUE, alpha = 0.5
  ) +
  labs(x = "Width (m)") +
  expand_limits(x = 0) +
  scale_x_continuous(labels = comma) +
  facet_grid(Section ~ ., scales = "free_y", space = "free_y") +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0)) +
  ggsave(
    filename = file.path(figPath, "PoolWidth.png"), width = figWidth,
    height = figWidth * 1.33
  )
