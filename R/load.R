#' Load Pacific Herring areas.
#'
#' Load Pacific Herring areas. Herring areas are kept in two files: the section
#' file has coarse area information, and location file has finer details. This
#' function merges these two files, and drops unnecessary rows and columns. In
#' addition, 'groups' are created for certain regions based on section numbers.
#' The output is a data frame with both coarse- and fine-scale area information
#' for the region(s) in question. There is an option to subset the sections if
#' desired.
#'
#' @param reg Character. Region of interest (i.e., HG, PRD, CC, SoG, WCVI, A27,
#'   or A2W).
#' @param sec_sub Numeric vector or NULL Subset of Sections to include in the
#'   analysis, or NULL to include all the Sections in the region.
#' @param where List. Location of the Pacific Herring "locations" database (see
#'   examples).
#' @param in_crs Character. Input coordinate reference system;
#'   \href{https://spatialreference.org/}{use EPSG codes if desired}.
#' @param out_crs Character. Output coordinate reference system;
#'   \href{https://spatialreference.org/}{use EPSG codes if desired}.
#' @param groups Tibble or NULL. Optional table to add a "Group" column to the
#'   results, say to aggregate data by combinations of Sections. Must have a
#'   column named "Group", and one or more of "StatArea", "Section",
#'   "LocationCode". Set to NULL to ignore (and Group column will be NA).
#' @template param-quiet
#' @importFrom readr read_csv cols
#' @importFrom dplyr filter select mutate full_join %>% transmute right_join
#' @importFrom tidyr unite
#' @importFrom odbc dbConnect odbc dbDisconnect
#' @importFrom DBI dbReadTable
#' @importFrom tibble as_tibble is_tibble
#' @importFrom sp SpatialPoints spTransform CRS
#' @importFrom Rdpack reprompt
#' @return Tibble. Table of geographic information for Pacific Herring: SAR,
#'   Region, Region name, Statistical Area, Group, Section, Location code,
#'   Location name, Pool, Eastings, Northings, Longitude, and Latitude.
#' @references \insertAllCited
#' @seealso \code{\link{HerringSpawn}}
#' @family load functions
#' @note This function requires 32-bit R to load data from the 32-bit MS Access
#'   database.
#' @export
#' @examples
#' db_loc <- system.file("extdata", package = "SpawnIndex")
#' area_loc <- list(
#'   loc = db_loc, db = "HerringSpawn.mdb",
#'   fns = list(sections = "Sections", locations = "Location")
#' )
#' areas <- load_area_data(reg = "WCVI", where = area_loc)
#' areas
#' secs <- c(231:233, 241, 245)
#' grps <- tibble::tibble(
#'   Section = c(231, 232, 233, 241),
#'   Group = c("Alberni Int", "Barkley", "Barkley", "Tofino Int")
#' )
#' areas_sec_grp <- load_area_data(
#'   reg = "WCVI", where = area_loc, groups = grps, sec_sub = secs
#' )
#' dplyr::distinct(dplyr::select(
#'   areas_sec_grp, Region, StatArea, Group, Section
#' ))
load_area_data <- function(reg,
                           sec_sub = NULL,
                           where,
                           in_crs = "+init=epsg:4326",
                           out_crs = "+init=epsg:3005",
                           groups = NULL,
                           quiet = FALSE) {
  # Check where: character
  if (!is.character(reg)) stop("`reg` must be character.", call. = FALSE)
  # Check sec_sub: numeric or null
  if (!is.numeric(sec_sub) & !is.null(sec_sub)) {
    stop("`sec_sub` must be numeric or NULL.", call. = FALSE)
  }
  # Get where names
  where_names <- c("loc", "db", "fns.sections", "fns.locations")
  # Check where: list
  if (!is.list(where)) stop("Argument `where` must be a list.", call. = FALSE)
  # Check where: names
  if (any(names(unlist(where)) != where_names)) {
    stop("Argument `where` needs names:", where_names, call. = FALSE)
  }
  # Check where: contents
  if (typeof(unlist(where)) != "character") {
    stop("Argument `where` must contain characters", call. = FALSE)
  }
  # Check in_crs and out_crs: character
  if (!is.character(in_crs) & !is.character(out_crs)) {
    stop("`in_crs` and `out_crs` must be characters.")
  }
  # Check groups: tibble or NULL
  if (is_tibble(groups)) {
    # Check group names
    if (!"Group" %in% names(groups)) {
      stop("`groups` needs column named 'Group`", call. = FALSE)
    }
    if (!any(c("StatArea", "Section", "LocationCode") %in% names(groups))) {
      stop("`groups` needs column named `StatArea`, `Section`, and/or
           LocationCode", call. = FALSE)
    }
  } else {
    # If not tibble, must be NULL
    if (!is.null(groups)) stop("`groups` must be tibble or NULL.")
  }
  # Cross-walk table for SAR to region and region name
  regions <- read_csv(
    file =
      "SAR, Region, RegionName, Major
          1, HG, Haida Gwaii, TRUE
          2, PRD, Prince Rupert District, TRUE
          3, CC, Central Coast, TRUE
          4, SoG, Strait of Georgia, TRUE
          5, WCVI, West Coast of Vancouver Island, TRUE
          6, A27, Area 27, FALSE
          7, A2W, Area 2 West, FALSE
          8, JS, Johnstone Strait, FALSE",
    col_types = cols("i", "c", "c", "l")
  )
  # If region isn't JS, remove it
  if (!reg %in% c("JS", "All")) {
    regions <- filter(.data = regions, SAR != 8)
  }
  # Return region names
  region_names <- regions %>%
    select(RegionName, Region, Major) %>%
    mutate(Region = paste("(", Region, ")", sep = "")) %>%
    unite(RegionName, Region, col = "Region", sep = " ")
  # Make a nice list
  all_region_names <- list(
    major = region_names$Region[region_names$Major],
    minor = region_names$Region[!region_names$Major]
  )
  # Possible regions by type
  all_regions <- list(
    major = as.character(regions$Region[regions$Major]),
    minor = as.character(regions$Region[!regions$Major])
  )
  # Error if region is incorrect
  if (!(reg %in% c(unlist(all_regions), "All"))) {
    stop(
      "Possible regions are: ", paste_nicely(unlist(all_regions)), ".",
      call. = FALSE
    )
  }
  # Establish connection with access
  access_db <- dbConnect(
    drv = odbc(),
    .connection_string = paste(
      "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=",
      file.path(where$loc, where$db),
      sep = ""
    )
  )
  # TODO: Sections 132 and 135 are also SoG sections -- how to resolve?
  # Manual fix: Johnstone Strait herring sections
  js_sections <- c(111, 112, 121:127, 131:136)
  # If the region is Johnstone Strait
  if (reg == "JS") {
    # Message
    if (!quiet) {
      cat("Note overlap between JS and SoG: Sections 132 and 135\n")
    }
    # Access the sections worksheet and wrangle
    sections <- dbReadTable(conn = access_db, name = where$fns$sections)
    # Error if data was not fetched
    if (class(sections) != "data.frame") {
      stop("No data available in MS Access connection.", call. = FALSE)
    }
    # Check sections: names
    if (!all(c("SAR", "Section") %in% names(sections))) {
      stop("Sections table is missing columns", call. = FALSE)
    }
    # Wrangle the sections worksheet
    sections <- sections %>%
      filter(Section %in% js_sections) %>%
      mutate(SAR = 8) %>%
      full_join(y = regions, by = "SAR") %>%
      filter(Region %in% reg) %>%
      select(SAR, Region, RegionName, Section) %>%
      mutate(Section = as.integer(Section)) %>%
      distinct() %>%
      as_tibble()
  } else { # End if Johnstone Strait, otherwise
    # Wrangle the sections worksheet
    sections <- dbReadTable(conn = access_db, name = where$fns$sections)
    # Error if data was not fetched
    if (class(sections) != "data.frame") {
      stop("No data available in MS Access connection.", call. = FALSE)
    }
    # Check sections: names
    if (!all(c("SAR", "Section") %in% names(sections))) {
      stop("Sections table is missing columns", call. = FALSE)
    }
    # Wrangle the sections table
    sections <- sections %>%
      full_join(y = regions, by = "SAR") %>%
      select(SAR, Region, RegionName, Section) %>%
      mutate(Section = as.integer(Section)) %>%
      distinct() %>%
      as_tibble()
    # If we only want a specific region
    if (reg != "All") {
      # Remove areas outside SARs, and other regions
      sections <- sections %>%
        filter(SAR != -1, Region == reg)
    } # End if we only want a specific region
  } # End if the region is not Johnstone Strait
  # Access the locations worksheet
  loc <- dbReadTable(conn = access_db, name = where$fns$locations)
  # Error if data was not fetched
  if (class(loc) != "data.frame") {
    stop("No data available in MS Access connection.", call. = FALSE)
  }
  # Check loc: names
  if (!all(c(
    "Loc_Code", "Location", "StatArea", "Section", "Bed", "Location_Latitude",
    "Location_Longitude"
  ) %in% names(loc))) {
    stop("Locations table is missing columns", call. = FALSE)
  }
  # Wrangle the locations table
  loc_dat <- as_tibble(loc) %>%
    select(
      Loc_Code, Location, StatArea, Section, Bed, Location_Latitude,
      Location_Longitude
    ) %>%
    mutate(Location = as.character(Location)) %>%
    rename(
      LocationCode = Loc_Code, LocationName = Location,
      Latitude = Location_Latitude, Longitude = Location_Longitude, Pool = Bed
    ) %>%
    replace_na(replace = list(Longitude = 0, Latitude = 0)) %>%
    select(
      LocationCode, LocationName, Pool, Section, StatArea, Longitude,
      Latitude
    ) %>%
    arrange(LocationCode) %>%
    distinct()
  # Grab the spatial info (X and Y)
  loc_sp <- loc_dat %>%
    transmute(X = Longitude, Y = Latitude)
  # Put X and Y into a spatial points object
  loc_pts <- SpatialPoints(coords = loc_sp, proj4string = CRS(in_crs))
  # Convert X and Y from WGS to Albers
  loc_pts_alb <- spTransform(x = loc_pts, CRSobj = CRS(out_crs))
  # Extract spatial info
  df_alb <- as_tibble(loc_pts_alb)
  # Extract relevant location data
  locations <- loc_dat %>%
    cbind(df_alb) %>%
    mutate(
      Eastings = ifelse(is.na(Longitude), Longitude, X),
      Northings = ifelse(is.na(Latitude), Latitude, Y),
      Section = as.integer(Section)
    ) %>%
    select(
      StatArea, Section, LocationCode, LocationName, Pool, Eastings,
      Northings, Latitude, Longitude
    ) %>%
    filter(Section %in% sections$Section) %>%
    distinct() %>%
    as_tibble()
  # If groups is NULL
  if (is.null(groups)) {
    # Set groups to NA
    locations <- locations %>%
      mutate(Group = NA)
  } else { # End if NULL, otherwise
    # Determine matching columns
    grp_cols <- which(names(groups) %in% names(locations))
    # Set groups
    locations <- locations %>%
      left_join(y = groups, by = names(groups)[grp_cols])
  } # End if not NULL
  # If any groups are NA, check if *some* are missing (i.e., incomplete)
  if (any(is.na(locations$Group))) {
    # Get distinct rows
    grp_u <- locations %>%
      select(StatArea, Section, Group) %>%
      distinct() %>%
      arrange(StatArea, Section)
    # Get distinct rows with no missing groups
    grp_u_na <- grp_u %>%
      filter(is.na(Group))
    # Check if none or all have groups
    none_or_all <- nrow(grp_u) == nrow(grp_u_na)
    # Message re some sections(s) missing group info
    if (!none_or_all & !quiet) {
      cat("Incomplete `Group' info for Section(s): ",
        paste_nicely(unique(grp_u_na$Section)), "\n",
        sep = ""
      )
    }
  } # End if any groups are NA
  # Extract required data
  res <- locations %>%
    right_join(y = sections, by = "Section") %>%
    filter(!is.na(StatArea), !is.na(Section)) %>%
    select(
      SAR, Region, RegionName, StatArea, Group, Section, LocationCode,
      LocationName, Pool, Eastings, Northings, Longitude, Latitude
    ) %>%
    mutate(Section = as.integer(Section), Pool = as.integer(Pool)) %>%
    #      mutate( StatArea=formatC(StatArea, width=2, format="d", flag="0"),
    #          Section=formatC(Section, width=3, format="d", flag="0") ) %>%
    arrange(Region, StatArea, Group, Section, LocationCode) %>%
    distinct() %>%
    droplevels()
  # If not all sections are included
  if (!is.null(sec_sub)) {
    # Grab a subset of sections
    res <- res %>%
      filter(Section %in% sec_sub) %>%
      droplevels()
    # Message
    if (!quiet) {
      cat("Sections: ", paste_nicely(sec_sub), "\n", sep = "")
    }
  } # End if subsetting sections
  # Close the connection
  dbDisconnect(conn = access_db)
  # Check output: tibble rows
  check_tibble(dat = list(res = res), quiet = quiet)
  # Check output: names
  if (!all(c(
    "SAR", "Region", "RegionName", "StatArea", "Group", "Section",
    "LocationCode", "LocationName", "Pool", "Eastings", "Northings",
    "Longitude", "Latitude"
  ) %in% names(res))) {
    stop("`res` is missing columns", call. = FALSE)
  }
  # Return herring areas
  res
} # End load_area_data function

#' Get the all spawn table.
#'
#' Get the all spawn table.
#'
#' @param where List. Location of the Pacific Herring understory spawn database
#'   (see examples).
#' @template param-areas
#' @template param-yrs
#' @param ft2m Numeric. Conversion factor for feet to metres; default is 0.3048.
#'   Message if not 0.3048.
#' @template param-quiet
#' @importFrom odbc dbConnect odbc dbDisconnect
#' @importFrom DBI dbReadTable
#' @importFrom dplyr select rename full_join filter mutate %>% arrange ungroup
#' @importFrom tibble as_tibble
#' @importFrom stringr str_to_title
#' @importFrom lubridate as_date
#' @importFrom gfiscamutils max_na
#' @importFrom Rdpack reprompt
#' @return Tibble. Contains additional spawn survey data including start and end
#'   dates, as well as spawn length, width, and depth. Other information in this
#'   tibble comes from \code{a}: Region, Statistical Area, Section, and Location
#'   code.
#' @references \insertAllCited
#' @seealso \code{\link{HerringSpawn}} \code{\link{load_area_data}}
#' @family load functions
#' @export
#' @examples
#' db_loc <- system.file("extdata", package = "SpawnIndex")
#' area_loc <- list(
#'   loc = db_loc, db = "HerringSpawn.mdb",
#'   fns = list(sections = "Sections", locations = "Location")
#' )
#' areas <- load_area_data(reg = "WCVI", where = area_loc)
#' all_spawn_loc <- list(
#'   loc = db_loc, db = "HerringSpawn.mdb",
#'   fns = list(all_spawn = "tSSAllspawn", stations = "tSSStations")
#' )
#' all_spawn <- load_all_spawn(
#'   where = all_spawn_loc, areas = areas, yrs = 2010:2015
#' )
#' all_spawn
load_all_spawn <- function(where,
                           areas,
                           yrs,
                           ft2m = 0.3048,
                           quiet = FALSE) {
  # Get where names
  where_names <- c("loc", "db", "fns.all_spawn", "fns.stations")
  # Check where: list
  if (!is.list(where)) stop("Argument `where` must be a list.", call. = FALSE)
  # Check where: names
  if (any(names(unlist(where)) != where_names)) {
    stop("Argument `where` needs names:", where_names, call. = FALSE)
  }
  # Check where: contents
  if (typeof(unlist(where)) != "character") {
    stop("Argument `where` must contain characters", call. = FALSE)
  }
  # Check input: tibble rows
  check_tibble(dat = list(areas = areas), quiet = quiet)
  # Check a: names
  if (!all(c(
    "Region", "StatArea", "Group", "Section", "LocationCode", "LocationName",
    "Eastings", "Northings", "Longitude", "Latitude"
  ) %in% names(areas))) {
    stop("`areas` is missing columns", call. = FALSE)
  }
  # Check input: NA and numeric
  check_numeric(
    dat = list(yrs = yrs, ft2m = ft2m),
    quiet = quiet
  )
  # Check yrs: range
  if (any(yrs < pars$years$assess) & !quiet) {
    message("`yrs` < ", pars$years$assess, ".")
  }
  # Check ft2m: range
  if (!all.equal(ft2m, 0.3048, 0.00001) & !quiet) {
    message("`ft2m` is not 0.3048.")
  }
  # Get a small subset of area data
  areas_sm <- areas %>%
    select(
      Region, StatArea, Group, Section, LocationCode, LocationName, Eastings,
      Northings, Longitude, Latitude
    ) %>%
    distinct() %>%
    as_tibble()
  # Establish connection with access
  access_db <- dbConnect(
    drv = odbc(),
    .connection_string = paste(
      "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=",
      file.path(where$loc, where$db),
      sep = ""
    )
  )
  # Extract relevant spawn data
  spawn <- dbReadTable(conn = access_db, name = where$fns$all_spawn) %>%
    rename(LocationCode = Loc_Code, SpawnNumber = Spawn_Number) %>%
    mutate(
      Start = as_date(Start), End = as_date(End),
      Method = str_to_title(Method)
    ) %>%
    filter(Year %in% yrs, LocationCode %in% areas_sm$LocationCode) %>%
    select(
      Year, LocationCode, SpawnNumber, Start, End, Length, Width, Method
    ) %>%
    as_tibble()
  # Extrac relevant stations data
  stations <- dbReadTable(conn = access_db, name = where$fns$stations) %>%
    rename(LocationCode = Loc_Code, SpawnNumber = Spawn_Number) %>%
    filter(LocationCode %in% areas_sm$LocationCode) %>%
    mutate(DepthM = Depth * ft2m * -1) %>%
    group_by(Year, LocationCode, SpawnNumber) %>%
    summarise(Depth = max_na(DepthM)) %>%
    ungroup() %>%
    arrange(Year, LocationCode, SpawnNumber)
  # Combine spawn and station data
  spawn_station <- full_join(
    x = spawn, y = stations,
    by = c("Year", "LocationCode", "SpawnNumber")
  )
  # Combine spawn and station data with area data
  res <- spawn_station %>%
    left_join(y = areas_sm, by = c("LocationCode")) %>%
    select(
      Year, Region, StatArea, Group, Section, LocationCode, LocationName,
      SpawnNumber, Eastings, Northings, Longitude, Latitude, Start, End,
      Length, Width, Depth, Method
    ) %>%
    arrange(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber, Start
    )
  # Close the connection
  dbDisconnect(conn = access_db)
  # Check output: tibble rows
  check_tibble(dat = list(res = res), quiet = quiet)
  # Check output: names
  if (!all(c(
    "Year", "Region", "StatArea", "Group", "Section", "LocationCode",
    "LocationName", "SpawnNumber", "Eastings", "Northings", "Longitude",
    "Latitude", "Start", "End", "Length", "Width", "Depth", "Method"
  ) %in% names(res))) {
    stop("`res` is missing columns", call. = FALSE)
  }
  # Return the table
  res
} # End load_all_spawn function

#' Load median spawn width.
#'
#' Load median spawn width in metres (m) for Pacific Herring surface spawn index
#' calculations. Observed width is not preferred for surface spawn surveys
#' because surveyors tend to underestimate spawn width
#' \insertCite{HayKronlund1987}{SpawnIndex}. Instead, the preferred with comes
#' from underwater surveys \insertCite{GrinnellEtalYYYY}{SpawnIndex}.
#'
#' @param where List. Location of the Pacific Herring surface spawn database
#'   (see examples).
#' @template param-areas
#' @template param-quiet
#' @importFrom odbc dbConnect odbc dbDisconnect
#' @importFrom DBI dbReadTable
#' @importFrom dplyr select distinct rename left_join filter %>%
#' @importFrom tibble as_tibble
#' @importFrom Rdpack reprompt
#' @return List with three tables: median region (\code{WidthReg}), section
#'   (\code{WidthSec}), and pool (\code{WidthPool}) widths in metres (m) for the
#'   areas in \code{a}.
#' @references \insertAllCited
#' @seealso \code{\link{HerringSpawn}} \code{\link{load_area_data}}
#'   \code{\link{calc_surf_index}}
#' @family load functions
#' @export
#' @examples
#' db_loc <- system.file("extdata", package = "SpawnIndex")
#' area_loc <- list(
#'   loc = db_loc, db = "HerringSpawn.mdb",
#'   fns = list(sections = "Sections", locations = "Location")
#' )
#' areas <- load_area_data(reg = "WCVI", where = area_loc)
#' width_loc <- list(
#'   loc = db_loc, db = "HerringSpawn.mdb",
#'   fns = list(
#'     region_std = "RegionStd", section_std = "SectionStd",
#'     pool_std = "PoolStd"
#'   )
#' )
#' width_bar <- get_width(where = width_loc, areas = areas)
#' width_bar
get_width <- function(where,
                      areas,
                      quiet = FALSE) {
  # Get where names
  where_names <- c(
    "loc", "db", "fns.region_std", "fns.section_std", "fns.pool_std"
  )
  # Check where: list
  if (!is.list(where)) stop("Argument `where` must be a list.", call. = FALSE)
  # Check where: names
  if (any(names(unlist(where)) != where_names)) {
    stop("Argument `where` needs names:", where_names, call. = FALSE)
  }
  # Check where: contents
  if (typeof(unlist(where)) != "character") {
    stop("Argument `where` must contain characters", call. = FALSE)
  }
  # Check input: tibble rows
  check_tibble(dat = list(areas = areas), quiet = quiet)
  # Check areas: names
  if (!all(c("SAR", "Region", "StatArea", "Section", "LocationCode", "Pool")
  %in% names(areas))) {
    stop("`areas` is missing columns", call. = FALSE)
  }
  # Get area info
  areas_sm <- areas %>%
    select(SAR, Region, StatArea, Section, LocationCode, Pool) %>%
    distinct() %>%
    as_tibble()
  # Establish connection with access
  access_db <- dbConnect(
    drv = odbc(),
    .connection_string = paste(
      "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=",
      file.path(where$loc, where$db),
      sep = ""
    )
  )
  # Access the region worksheet and wrangle
  reg_std <- dbReadTable(conn = access_db, name = where$fns$region_std) %>%
    rename(SAR = REGION, WidthReg = WIDMED) %>%
    left_join(y = areas_sm, by = "SAR") %>%
    filter(SAR %in% areas_sm$SAR) %>%
    select(Region, WidthReg) %>%
    distinct() %>%
    as_tibble()
  # Access the section worksheet and wrangle
  sec_std <- dbReadTable(conn = access_db, name = where$fns$section_std) %>%
    rename(Section = SECTION, WidthSec = WIDMED) %>%
    mutate(Section = as.integer(Section)) %>%
    left_join(y = areas_sm, by = "Section") %>%
    filter(Section %in% areas_sm$Section) %>%
    select(Region, Section, WidthSec) %>%
    distinct() %>%
    as_tibble()
  # Access the pool worksheet and wrangle
  pool_std <- dbReadTable(conn = access_db, name = where$fns$pool_std) %>%
    rename(Section = SECTION, Pool = BED, WidthPool = WIDMED) %>%
    mutate(Section = as.integer(Section), Pool = as.integer(Pool)) %>%
    left_join(y = areas_sm, by = c("Section", "Pool")) %>%
    filter(Section %in% areas_sm$Section) %>%
    select(Region, Section, Pool, WidthPool) %>%
    distinct() %>%
    as_tibble()
  # Merge the tables to a list
  res <- list(region = reg_std, section = sec_std, pool = pool_std)
  # Close the connection
  dbDisconnect(conn = access_db)
  # Check output: tibble rows
  check_tibble(
    dat = list(
      region = res$region, section = res$section, pool = res$pool
    ),
    quiet = quiet
  )
  # Check output: region names
  if (!all(c("Region", "WidthReg") %in% names(res$region))) {
    stop("`res$region` is missing columns", call. = FALSE)
  }
  # Check output: section names
  if (!all(c("Region", "Section", "WidthSec") %in% names(res$section))) {
    stop("`res$section` is missing columns", call. = FALSE)
  }
  # Check output: pool names
  if (!all(c("Region", "Section", "Pool", "WidthPool") %in% names(res$pool))) {
    stop("`res$pool` is missing columns", call. = FALSE)
  }
  # Table to return
  res
} # End get_width function
