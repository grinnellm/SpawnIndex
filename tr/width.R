##### Housekeeping #####
graphics.off()
rm(list = ls())

# Packages
require(SpawnIndex)
require(tidyverse)
require(odbc)
require(DBI)
require(scales)

##### Controls #####

# Region of interest: major (HG, PRD, CC, SoG, WCVI); minor (A27, A2W)
region <- "SoG"

# Years to consider
yrRange <- 1951:2019

# Figure path
figPath <- file.path("tr", "cache")

# Figure width
figWidth <- 6.5

# Section subset
sectionSub <- NA

##### Paths #####

# Make the directory if required
if (!dir.exists(figPath)) dir.create(path = figPath)

# Database location
# db_loc <- system.file("extdata", package = "SpawnIndex")
db_loc <- file.path("..", "Data", "Local")

# Database name
# dbName <- "HerringSpawn.mdb"
dbName <- "HSA_Program_v6.2.mdb"

# Tables etc for area info
areaLoc <- list(
  loc = db_loc, db = dbName,
  fns = list(sections = "Sections", locations = "Location")
)

# Tables etc for median widths
width_loc <- list(
  loc = db_loc, db = dbName,
  fns = list(
    region_std = "RegionStd", section_std = "SectionStd", pool_std = "PoolStd"
  )
)

# Tables etc for surface data
surf_loc <- list(loc = db_loc, db = dbName, fns = list(all_spawn = "tSSAllspawn"))

# Tables etc for dive data (Macrocystis and understory)
diveLoc <- list(loc = db_loc, db = dbName, fns = list(alg_trans = "tSSVegTrans"))

##### Data #####

# Load area data
areas <- load_area_data(
  reg = region, sec_sub = sectionSub, where = areaLoc, quiet = TRUE
)

# Median widths
width_bar <- get_width(where = width_loc, a = areas)

# Understory spawn width correction factors
data(under_width_fac)

##### Analysis #####

# Get surface widths
GetSurfWidth <- function(where,
                         a,
                         yrs = yrRange,
                         widths) {
  # Establish connection with access
  access_db <- odbcDriverConnect(
    paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
          file.path(where$loc, where$db),
          sep = ""
    )
  )
  # Get a small subset of area data
  areas_sm <- a %>%
    select(Region, StatArea, Section, LocationCode, Pool) %>%
    distinct() %>%
    as_tibble()
  # Load all spawn
  spawn <- sqlFetch(channel = access_db, sqtable = where$fns$all_spawn) %>%
    rename(
      LocationCode = Loc_Code, SpawnNumber = Spawn_Number, WidthObs = Width
    ) %>%
    filter(Year %in% yrs, LocationCode %in% a$LocationCode) %>%
    select(Year, LocationCode, SpawnNumber, WidthObs) %>%
    as_tibble()
  # Bind the tables
  res <- spawn %>%
    left_join(y = areas_sm, by = "LocationCode") %>%
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
    mutate(Pool = as.character(Pool), Survey = "Surface") %>%
    select(
      Survey, Year, Region, StatArea, Section, Pool, LocationCode, SpawnNumber,
      WidthReg, WidthSec, WidthPool, WidthObs
    ) %>%
    arrange(
      Year, Region, StatArea, Section, Pool, LocationCode, SpawnNumber, WidthObs
    )
  # Close the connection
  odbcClose(access_db)
  # Return the table
  res
} # End GetSurfWidth function

# Load surface widths
surfWidth <- GetSurfWidth(where = surf_loc, a = areas, widths = width_bar)

# Get dive widths
GetDiveWidth <- function(where,
                         a,
                         yrs = yrRange,
                         tau = under_width_fac) {
  # Establish connection with access
  access_db <- odbcDriverConnect(
    paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
          file.path(where$loc, where$db),
          sep = ""
    )
  )
  # Get a small subset of area data
  areas_sm <- a %>%
    select(Region, StatArea, Section, Pool, LocationCode) %>%
    distinct() %>%
    as_tibble()
  # Transect data
  alg_trans <- sqlFetch(channel = access_db, sqtable = where$fns$alg_trans) %>%
    rename(
      LocationCode = Loc_Code, SpawnNumber = Spawn_Number,
      QuadratSize = Quadrat_Size, WidthObs = Width_Recorded
    ) %>%
    filter(Year %in% yrs, LocationCode %in% a$LocationCode) %>%
    select(
      Year, LocationCode, SpawnNumber, Transect, WidthObs
    ) %>%
    left_join(y = areas_sm, by = "LocationCode") %>%
    as_tibble()
  # Correction factors for region(s) by year (to fix lead line shrinkage issue)
  width_facs <- tau %>%
    gather(key = Region, value = WidthFac, -Year)
  # Merge the width factors and correct transect widths
  alg_trans <- alg_trans %>%
    left_join(y = width_facs, by = c("Year", "Region")) %>%
    replace_na(replace = list(WidthFac = 1.0)) %>%
    mutate(
      Width = WidthObs * WidthFac, Survey = "Dive", Pool = as.character(Pool)
    ) %>%
    select(
      Survey, Year, Region, StatArea, Section, Pool, LocationCode, SpawnNumber,
      Width
    ) %>%
    rename(WidthObs = Width) %>%
    group_by(Region) %>%
    mutate(WidthReg = median(WidthObs, na.rm = TRUE)) %>%
    group_by(StatArea) %>%
    mutate(WidthStat = median(WidthObs, na.rm = TRUE)) %>%
    group_by(Section) %>%
    mutate(WidthSec = median(WidthObs, na.rm = TRUE)) %>%
    group_by(LocationCode) %>%
    mutate(WidthLoc = median(WidthObs, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(
      Year, Region, StatArea, Section, Pool, LocationCode, SpawnNumber, WidthObs
    )
  # Resuts
  res <- alg_trans
  # Close the connection
  odbcClose(access_db)
  # Return results
  res
} # End GetDiveWidth function

# Get dive widths
diveWidth <- GetDiveWidth(where = diveLoc, a = areas)

# Get median dive widths
width_bar2 <- diveWidth %>%
  select(
    Region, StatArea, Section, LocationCode, WidthReg, WidthStat,
    WidthSec, WidthLoc
  ) %>%
  distinct() %>%
  arrange(Region, StatArea, Section, LocationCode)

# Combine surface and dive widths
allWidth <- bind_rows(surfWidth, diveWidth)

# Get surface widths (second option)
GetSurfWidth2 <- function(where,
                          a,
                          yrs = yrRange,
                          widths) {
  # Establish connection with access
  access_db <- odbcDriverConnect(
    paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
          file.path(where$loc, where$db),
          sep = ""
    )
  )
  # Get a small subset of area data
  areas_sm <- a %>%
    select(Region, StatArea, Section, LocationCode) %>%
    distinct() %>%
    as_tibble()
  # Load all spawn
  spawn <- sqlFetch(channel = access_db, sqtable = where$fns$all_spawn) %>%
    rename(
      LocationCode = Loc_Code, SpawnNumber = Spawn_Number, WidthObs = Width
    ) %>%
    filter(Year %in% yrs, LocationCode %in% a$LocationCode) %>%
    select(Year, LocationCode, SpawnNumber, WidthObs) %>%
    as_tibble()
  # Bind the tables
  res <- spawn %>%
    left_join(y = areas_sm, by = "LocationCode") %>%
    # Merge widths incrementally: region
    left_join(
      y = widths %>% select(Region, WidthReg) %>% distinct(),
      by = "Region"
    ) %>%
    # Merge widths incrementally: statistical area
    left_join(
      y = widths %>% select(StatArea, WidthStat) %>% distinct(),
      by = "StatArea"
    ) %>%
    # Merge widths incrementally: section
    left_join(
      y = widths %>% select(Section, WidthSec) %>% distinct(),
      by = "Section"
    ) %>%
    # Merge widths incrementally: location
    left_join(
      y = widths %>% select(LocationCode, WidthLoc) %>% distinct(),
      by = "LocationCode"
    ) %>%
    mutate(Survey = "Surface") %>% # Pool = as.character(Pool),
    select(
      Survey, Year, Region, StatArea, Section, LocationCode, SpawnNumber,
      WidthReg, WidthStat, WidthSec, WidthLoc, WidthObs
    ) %>%
    arrange(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber, WidthObs
    )
  # Close the connection
  odbcClose(access_db)
  # Return the table
  res
} # End GetSurfWidths2 function

# Load surface widths
surfWidth2 <- GetSurfWidth2(where = surf_loc, a = areas, widths = width_bar2)

##### Figures #####

# Plot observed width with lines for median pool, section, and region width
poolPlot <- ggplot(data = allWidth, mapping = aes(y = Pool)) +
  geom_vline(
    mapping = aes(xintercept = WidthReg, linetype = Survey),
    colour = "seagreen", na.rm = TRUE, size = 1
  ) +
  geom_vline(
    mapping = aes(xintercept = WidthStat, linetype = Survey),
    colour = "sienna", na.rm = TRUE, size = 1
  ) +
  geom_vline(
    mapping = aes(xintercept = WidthSec, linetype = Survey),
    colour = "slateblue", na.rm = TRUE, size = 1
  ) +
  geom_boxplot(
    mapping = aes(x = WidthObs, fill = Survey),
    outlier.alpha = 0.5, outlier.size = 1.5, na.rm = TRUE
  ) +
  geom_point(
    mapping = aes(x = WidthPool),
    shape = 8, na.rm = TRUE, size = 2
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

##### Tables #####

# Determine where widths come from
propWidth <- surfWidth %>%
  mutate(
    Width = ifelse(is.na(WidthPool),
                   ifelse(is.na(WidthSec), "Region", "Section"),
                   "Pool"
    ),
    Width = factor(Width, levels = c("Pool", "Section", "Region"))
  )

# Proportions by method
pt <- as.data.frame(table(propWidth$Width) / nrow(propWidth) * 100)

# Make a nice table
df <- matrix(data = pt$Freq, dimnames = list(NULL, pt$Var1), nrow = 1) %>%
  as_tibble() %>%
  mutate(SAR = region) %>%
  select(SAR, pt$Var1)

# Determine where widths come from (2)
propWidth2 <- surfWidth2 %>%
  mutate(
    Width = ifelse(is.na(WidthLoc),
                   ifelse(is.na(WidthSec),
                          ifelse(is.na(WidthStat), "Region", "StatArea"),
                          "Section"
                   ),
                   "Location"
    ),
    Width = factor(Width,
                   levels = c("Location", "Section", "StatArea", "Region")
    )
  )

# Proportions by method
pt2 <- as.data.frame(table(propWidth2$Width) / nrow(propWidth) * 100)

# Make a nice table
df2 <- matrix(data = pt2$Freq, dimnames = list(NULL, pt2$Var1), nrow = 1) %>%
  as_tibble() %>%
  mutate(SAR = region) %>%
  select(SAR, pt2$Var1)

# Write to file
if( file.exists("Old.csv") ) {
  write_csv(x=df, path="Old.csv", append=TRUE)
  write_csv(x=df2, path="New.csv", append=TRUE)
} else {
  write_csv(x=df, path="Old.csv", append=FALSE)
  write_csv(x=df2, path="New.csv", append=FALSE)
}
