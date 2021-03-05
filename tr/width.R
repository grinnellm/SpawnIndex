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
yr_range <- pars$years$assess:2019

# Figure path
fig_path <- file.path("tr", "cache")

# Figure width
fig_width <- 6.5

# Section subset
section_sub <- NA

##### Paths #####

# Make the directory if required
if (!dir.exists(fig_path)) dir.create(path = fig_path)

# Database location
# db_loc <- system.file("extdata", package = "SpawnIndex")
db_loc <- file.path("..", "Data", "Local")

# Database name
# db_name <- "HerringSpawn.mdb"
db_name <- "HSA_Program_v6.2.mdb"

# Tables etc for area info
area_loc <- list(
  loc = db_loc, db = db_name,
  fns = list(sections = "Sections", locations = "Location")
)

# Tables etc for median widths
width_loc <- list(
  loc = db_loc, db = db_name,
  fns = list(
    region_std = "RegionStd", section_std = "SectionStd", pool_std = "PoolStd"
  )
)

# Tables etc for surface data
surf_loc <- list(
  loc = db_loc, db = db_name,
  fns = list(all_spawn = "tSSAllspawn")
)

# Tables etc for dive data (Macrocystis and understory)
dive_loc <- list(
  loc = db_loc, db = db_name,
  fns = list(alg_trans = "tSSVegTrans")
)

##### Data #####

# Load area data
areas <- load_area_data(
  reg = region, sec_sub = section_sub, where = area_loc, quiet = TRUE
)

# Median widths
width_bar <- get_width(where = width_loc, areas = areas)

# Understory spawn width correction factors
data(under_width_facs)

##### Analysis #####

# Get surface widths
get_surf_width <- function(where,
                           areas,
                           yrs = yr_range,
                           widths) {
  # Establish connection with access
  access_db <- odbcDriverConnect(
    paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
      file.path(where$loc, where$db),
      sep = ""
    )
  )
  # Get a small subset of area data
  areas_sm <- areas %>%
    select(Region, StatArea, Section, LocationCode, Pool) %>%
    distinct() %>%
    as_tibble()
  # Load all spawn
  spawn <- sqlFetch(channel = access_db, sqtable = where$fns$all_spawn) %>%
    rename(
      LocationCode = Loc_Code, SpawnNumber = Spawn_Number, WidthObs = Width
    ) %>%
    filter(Year %in% yrs, LocationCode %in% areas_sm$LocationCode) %>%
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
} # End get_surf_width function

# Load surface widths
surf_width <- get_surf_width(where = surf_loc, a = areas, widths = width_bar)

# Get dive widths
get_dive_width <- function(where,
                           areas,
                           yrs = yr_range,
                           tau = under_width_facs) {
  # Establish connection with access
  access_db <- odbcDriverConnect(
    paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
      file.path(where$loc, where$db),
      sep = ""
    )
  )
  # Get a small subset of area data
  areas_sm <- areas %>%
    select(Region, StatArea, Section, Pool, LocationCode) %>%
    distinct() %>%
    as_tibble()
  # Transect data
  alg_trans <- sqlFetch(channel = access_db, sqtable = where$fns$alg_trans) %>%
    rename(
      LocationCode = Loc_Code, SpawnNumber = Spawn_Number,
      QuadratSize = Quadrat_Size, WidthObs = Width_Recorded
    ) %>%
    filter(Year %in% yrs, LocationCode %in% areas_sm$LocationCode) %>%
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
} # End get_dive_width function

# Get dive widths
dive_width <- get_dive_width(where = dive_loc, areas = areas)

# Get median dive widths
width_bar2 <- dive_width %>%
  select(
    Region, StatArea, Section, LocationCode, WidthReg, WidthStat,
    WidthSec, WidthLoc
  ) %>%
  distinct() %>%
  arrange(Region, StatArea, Section, LocationCode)

# Combine surface and dive widths
all_width <- bind_rows(surf_width, dive_width)

# Get surface widths (second option)
get_surf_width_2 <- function(where,
                             areas,
                             yrs = yr_range,
                             widths) {
  # Establish connection with access
  access_db <- odbcDriverConnect(
    paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
      file.path(where$loc, where$db),
      sep = ""
    )
  )
  # Get a small subset of area data
  areas_sm <- areas %>%
    select(Region, StatArea, Section, LocationCode) %>%
    distinct() %>%
    as_tibble()
  # Load all spawn
  spawn <- sqlFetch(channel = access_db, sqtable = where$fns$all_spawn) %>%
    rename(
      LocationCode = Loc_Code, SpawnNumber = Spawn_Number, WidthObs = Width
    ) %>%
    filter(Year %in% yrs, LocationCode %in% areas_sm$LocationCode) %>%
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
} # End get_surf_widths2 function

# Load surface widths
surf_width2 <- get_surf_width_2(
  where = surf_loc, areas = areas, widths = width_bar2
)

##### Figures #####

# Plot observed width with lines for median pool, section, and region width
pool_plot <- ggplot(data = all_width, mapping = aes(y = Pool)) +
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
    filename = file.path(fig_path, "PoolWidth.png"), width = fig_width,
    height = fig_width * 1.33
  )
# print(pool_plot)

##### Tables #####

# Determine where widths come from
prop_width <- surf_width %>%
  mutate(
    Width = ifelse(is.na(WidthPool),
      ifelse(is.na(WidthSec), "Region", "Section"),
      "Pool"
    ),
    Width = factor(Width, levels = c("Pool", "Section", "Region"))
  )

# Proportions by method
pt <- as.data.frame(table(prop_width$Width) / nrow(prop_width) * 100)

# Make a nice table
df <- matrix(data = pt$Freq, dimnames = list(NULL, pt$Var1), nrow = 1) %>%
  as_tibble() %>%
  mutate(SAR = region) %>%
  select(SAR, pt$Var1)

# Determine where widths come from (2)
prop_width2 <- surf_width2 %>%
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
pt2 <- as.data.frame(table(prop_width2$Width) / nrow(prop_width) * 100)

# Make a nice table
df2 <- matrix(data = pt2$Freq, dimnames = list(NULL, pt2$Var1), nrow = 1) %>%
  as_tibble() %>%
  mutate(SAR = region) %>%
  select(SAR, pt2$Var1)

# Write to file
if (file.exists("Old.csv")) {
  write_csv(x = df, path = "Old.csv", append = TRUE)
  write_csv(x = df2, path = "New.csv", append = TRUE)
} else {
  write_csv(x = df, path = "Old.csv", append = FALSE)
  write_csv(x = df2, path = "New.csv", append = FALSE)
}
