##### Housekeeping #####
graphics.off()
rm(list = ls())

# Packages
require(SpawnIndex)
require(tidyverse)
require(RODBC)
require(scales)

##### Controls #####

# Region of interest
region <- "WCVI"

# Years to consider
yrRange <- 1951:2019

# Figure path
figPath <- file.path("tr", "cache")

# Figure width
figWidth <- 6.5

# Section subset
sectionSub <- 250:259

##### Paths #####

# Make the directory if required
if (!dir.exists(figPath)) dir.create(path = figPath)

# Database location
dbLoc <- system.file("extdata", package = "SpawnIndex")
# dbLoc <- file.path("..", "Data", "Local")

# Database name
dbName <- "HerringSpawn.mdb"
# dbName <- "HSA_Program_v6.2.mdb"

# Tables etc for area info
areaLoc <- list(
  loc = dbLoc, db = dbName,
  fns = list(sections = "Sections", locations = "Location")
)

# Tables etc for median widths
widthLoc <- list(
  loc = dbLoc, db = dbName,
  fns = list(
    regionStd = "RegionStd", sectionStd = "SectionStd", poolStd = "PoolStd"
  )
)

# Tables etc for surface data
surfLoc <- list(loc = dbLoc, db = dbName, fns = list(allSpawn = "tSSAllspawn"))

# Tables etc for dive data (Macrocystis and understory)
diveLoc <- list(loc = dbLoc, db = dbName, fns = list(algTrans = "tSSVegTrans"))

##### Data #####

# Load area data
areas <- LoadAreaData(
  reg = region, secSub = sectionSub, where = areaLoc, quiet = TRUE
)

# Median widths
barWidth <- GetWidth(where = widthLoc, a = areas)

# Understory spawn width correction factors
data(underWidthFac)

##### Analysis #####

# Get surface widths
GetSurfWidth <- function(where,
                         a,
                         yrs = yrRange,
                         widths) {
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
    filter(Year %in% yrs, LocationCode %in% a$LocationCode) %>%
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
    arrange(
      Year, Region, StatArea, Section, Pool, LocationCode, SpawnNumber, WidthObs
    )
  # Close the connection
  odbcClose(accessDB)
  # Return the table
  return(res)
} # End GetSurfWidth function

# Load surface widths
surfWidth <- GetSurfWidth(
  where = surfLoc, a = areas, widths = barWidth
)

# Get dive widths
GetDiveWidth <- function(where,
                         a,
                         yrs = yrRange,
                         tau = underWidthFac) {
  # Establish connection with access
  accessDB <- odbcConnectAccess(access.file = file.path(where$loc, where$db))
  # Get a small subset of area data
  areasSm <- a %>%
    dplyr::select(Region, StatArea, Section, Pool, LocationCode) %>%
    dplyr::distinct() %>%
    tibble::as_tibble()
  # Transect data
  algTrans <- sqlFetch(channel = accessDB, sqtable = where$fns$algTrans) %>%
    rename(
      LocationCode = Loc_Code, SpawnNumber = Spawn_Number,
      QuadratSize = Quadrat_Size, WidthObs = Width_Recorded
    ) %>%
    filter(Year %in% yrs, LocationCode %in% a$LocationCode) %>%
    select(
      Year, LocationCode, SpawnNumber, Transect, WidthObs
    ) %>%
    left_join(y = areasSm, by = "LocationCode") %>%
    as_tibble()
  # Correction factors for region(s) by year (to fix lead line shrinkage issue)
  widthFacs <- tau %>%
    gather(key = Region, value = WidthFac, -Year)
  # Merge the width factors and correct transect widths
  algTrans <- algTrans %>%
    left_join(y = widthFacs, by = c("Year", "Region")) %>%
    replace_na(replace = list(WidthFac = 1.0)) %>%
    mutate(
      Width = WidthObs * WidthFac, Survey = "Dive", Pool = as.character(Pool)
    ) %>%
    select(
      Survey, Year, Region, StatArea, Section, Pool, LocationCode, SpawnNumber,
      Width
    ) %>%
    rename(WidthObs = Width) %>%
    # group_by(Region) %>%
    # mutate(WidthReg = median(WidthObs)) %>%
    # group_by(Section) %>%
    # mutate(WidthReg = median(WidthObs)) %>%
    # group_by(Section, Pool) %>%
    # mutate(WidthReg = median(WidthObs)) %>%
    # ungroup() %>%
    arrange(
      Year, Region, StatArea, Section, Pool, LocationCode, SpawnNumber, WidthObs
    )
  # Resuts
  res <- algTrans
  # Close the connection
  odbcClose(accessDB)
  # Return results
  return(res)
} # End GetDiveWidth function

diveWidth <- GetDiveWidth(where = diveLoc, a = areas)

# Combine surface and dive widths
allWidth <- bind_rows(surfWidth, diveWidth)

##### Figures #####

# Plot observed width with lines for median pool, section, and region width
poolPlot <- ggplot(data = allWidth, mapping = aes(y = Pool)) +
  geom_boxplot(
    mapping = aes(x = WidthObs, fill=Survey), outlier.alpha = 0.5, outlier.size = 1,
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
  # scale_fill_viridis_d(option = "viridis") +
  facet_grid(Section ~ ., scales = "free_y", space = "free_y") +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0), legend.position = "top") +
  ggsave(
    filename = file.path(figPath, "PoolWidth.png"), width = figWidth,
    height = figWidth * 1.33
  )
# print(poolPlot)
