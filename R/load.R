#' Load Pacific Herring areas.
#'
#' Load Pacific Herring areas. Herring areas are kept in two files: the section
#' file has coarse area information and the location file has finer details.
#' This function merges these two files and drops unnecessary rows and columns.
#' In addition, 'groups' are created for certain regions based on sections.
#' The output is a data frame with both coarse- and fine-scale area information
#' for the region(s) in question. There is an option to subset the sections if
#' desired.
#'
#' @param reg Character. Region of interest (see \code{\link{regions}}).
#' @param sec_sub Numeric vector or NULL. Subset of Sections to include, or NULL
#'   to include all the Sections in the region.
#' @template param-where
#' @param groups Tibble or NULL. Optional table to add a "Group" column to the
#'   results, say to aggregate data by combinations of Sections. Must have
#'   columns named "Group" and "Section". Set to NULL to ignore (and Group
#'   column will be NA).
#' @param region_table Tibble. Cross-walk table for regions and region names;
#'   from \code{\link{regions}}.
#' @param crs Character. Coordinate reference system for geometry.
#' @template param-quiet
#' @importFrom readr read_csv cols
#' @importFrom dplyr filter select mutate full_join %>% transmute right_join
#'   distinct
#' @importFrom tidyr unite
#' @importFrom odbc dbConnect odbc dbDisconnect
#' @importFrom DBI dbReadTable
#' @importFrom tibble as_tibble is_tibble tibble
#' @importFrom sf st_as_sf st_transform
#' @importFrom Rdpack reprompt
#' @return Simple features (points) with geographic information for Pacific
#'   Herring: SAR, Region, Region name, Statistical Area, Group, Section,
#'   Location code, Location name, and Pool.
#' @seealso \code{\link{HerringSpawn}} \code{\link{regions}}
#' @family load functions
#' @note This function requires 32-bit R to load data from the 32-bit MS Access
#'   database.
#' @export
#' @examples
#' data(regions)
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
                           groups = NULL,
                           region_table = regions,
                           crs = "+proj=longlat +datum=WGS84",
                           quiet = FALSE) {
  # Check reg: character
  if (!is.character(reg)) stop("`reg` must be character.", call. = FALSE)
  # Check sec_sub: numeric or null
  if (!is.numeric(sec_sub) && !is.null(sec_sub)) {
    stop("`sec_sub` must be numeric or NULL.", call. = FALSE)
  }
  # Check where
  check_where(
    dat = where, dat_names = c("loc", "db", "fns.sections", "fns.locations")
  )
  # Check groups: tibble or NULL
  if (is_tibble(groups)) {
    if (!all(c("Group", "Section") %in% names(groups))) {
      stop("Groups table is missing columns", call. = FALSE)
    }
  } else {
    # If not tibble, must be NULL
    if (!is.null(groups)) stop("`groups` must be tibble or NULL.")
  }
  # Error if region is incorrect
  if (!(reg %in% c(region_table$Region, "All"))) {
    stop(
      "Possible regions are: ", paste_nicely(region_table$Region), ".",
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
  # Access the sections worksheet
  sections <- dbReadTable(conn = access_db, name = where$fns$sections)
  # Error if data was not fetched
  if (class(sections) != "data.frame") {
    stop("No data available in MS Access connection.", call. = FALSE)
  }
  # Check sections: names
  if (!all(c("SAR", "Section") %in% names(sections))) {
    stop("Sections table is missing columns", call. = FALSE)
  }
  # Get required columns from table
  sections <- sections %>%
    as_tibble() %>%
    select(SAR, Section) %>%
    distinct()
  # Get region type
  reg_type <- region_table$Type[which(region_table$Region == reg)]
  # Fix if region is All
  if (reg == "All" && length(reg_type) == 0) reg_type <- ""
  # If the region is special (A10)
  if (reg_type == "Special" & reg == "A10") {
    # If not quiet
    if (!quiet) {
      # Message
      cat("Note that", reg, "is a special SAR, not an official SAR.\n")
    } # End if not quiet
    # Area 10 herring sections
    if (reg == "A10") {
      # Special sections
      special_sections <- c(101:103)
      # If not quiet
      if (!quiet) {
        # Message
        cat("\tSections 101, 102, and 103 are in CC.\n")
      } # End if not quiet
    } # End if A10
    # Get the region number from the table
    reg_num <- region_table$SAR[which(region_table$Region == reg)]
    # Make a new sections worksheet
    sections <- tibble(
      SAR = reg_num,
      Section = formatC(special_sections, width=3, format="d", flag="0")
    )
  } # End if special
  # Subset regions table
  region_table <- region_table %>%
    filter(SAR %in% sections$SAR)
  # Wrangle the sections table
  sections <- sections %>%
    full_join(y = region_table, by = "SAR") %>%
    select(SAR, Region, RegionName, Section) %>%
    distinct()
  # If we only want a specific region
  if (reg != "All") {
    # Remove areas outside SARs, and other regions
    sections <- sections %>%
      filter(SAR != -1, Region == reg)
  } # End if we only want a specific region
  # If not all sections are included
  if (!is.null(sec_sub)) {
    # If not quiet
    if (!quiet) {
      # Message if not all sections are included
      cat("Sections: ", paste_nicely(sec_sub), "\n", sep = "")
    } # End if not quiet
    # Grab a subset of sections
    sections <- sections %>%
      filter(Section %in% sec_sub)
  } # End if subsetting sections
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
  locations <- as_tibble(loc) %>%
    select(
      Loc_Code, Location, StatArea, Section, Bed, Location_Latitude,
      Location_Longitude
    ) %>%
    mutate(
      Location = as.character(Location),
      Bed = as.integer(Bed),
      StatArea = formatC(StatArea, width=2, format="d", flag="0"),
      Section = formatC(Section, width=3, format="d", flag="0")
      ) %>%
    rename(
      LocationCode = Loc_Code, LocationName = Location, Pool = Bed,
      Latitude = Location_Latitude, Longitude = Location_Longitude, Pool = Bed
    ) %>%
    replace_na(replace = list(Longitude = 0, Latitude = 0)) %>%
    left_join(sections, by = "Section") %>%
    filter(Section %in% sections$Section) %>%
    distinct() %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = crs)
  # If groups is NULL
  if (is.null(groups)) {
    # Set groups to NA
    locations <- locations %>%
      mutate(Group = NA)
  } else { # End if NULL, otherwise
    # Format groups
    groups <- groups %>%
      mutate(Section = formatC(Section, width=3, format="d", flag="0"))
    # Set groups
    locations <- locations %>%
      left_join(y = groups, by = "Section")
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
    if (!none_or_all && !quiet) {
      cat("Incomplete `Group' info for Section(s): ",
        paste_nicely(unique(grp_u_na$Section)), "\n",
        sep = ""
      )
    }
  } # End if any groups are NA
  # Extract required data
  res <- locations %>%
    filter(!is.na(StatArea), !is.na(Section)) %>%
    select(
      SAR, Region, RegionName, StatArea, Group, Section, LocationCode,
      LocationName, Pool, geometry
    ) %>%
    arrange(Region, StatArea, Group, Section, LocationCode) %>%
    distinct() %>%
    st_as_sf()
  # Close the connection
  dbDisconnect(conn = access_db)
  # Check output: tibble rows
  check_tibble(dat = list(res = res), quiet = quiet)
  # Check output: names
  if (!all(c(
    "SAR", "Region", "RegionName", "StatArea", "Group", "Section",
    "LocationCode", "LocationName", "Pool", "geometry"
  ) %in% names(res))) {
    stop("`res` is missing columns", call. = FALSE)
  }
  # Return herring areas
  res
} # End load_area_data function

#' Load the all spawn table.
#'
#' Load the all spawn table, which has additional spawn survey data.
#'
#' @template param-where
#' @template param-areas
#' @template param-years
#' @param ft2m Numeric. Conversion factor for feet to metres; default is 0.3048.
#'   Message if not 0.3048.
#' @template param-quiet
#' @importFrom odbc dbConnect odbc dbDisconnect
#' @importFrom DBI dbReadTable
#' @importFrom dplyr select rename full_join filter mutate %>% arrange ungroup
#'   distinct
#' @importFrom tibble as_tibble
#' @importFrom stringr str_to_title
#' @importFrom lubridate as_date
#' @importFrom Rdpack reprompt
#' @return Tibble. Contains additional spawn survey data including start and end
#'   dates, as well as spawn length, width, and depth. Other information in this
#'   tibble comes from \code{areas}: Region, Statistical Area, Section, and
#'   Location code.
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
#'   where = all_spawn_loc, areas = areas, years = 2010:2015
#' )
#' all_spawn
load_all_spawn <- function(where,
                           areas,
                           years,
                           ft2m = 0.3048,
                           quiet = FALSE) {
  # Check where
  check_where(
    dat = where, dat_names = c("loc", "db", "fns.all_spawn", "fns.stations")
  )
  # Check input: tibble rows
  check_tibble(dat = list(areas = areas), quiet = quiet)
  # Area names
  area_names <- c(
    "SAR", "Region", "RegionName", "StatArea", "Group", "Section",
    "LocationCode", "LocationName", "Pool", "geometry"
  )
  # Check areas: names
  if (!all(area_names %in% names(areas))) {
    stop("`areas` is missing columns", call. = FALSE)
  }
  # Check input: NA and numeric
  check_numeric(
    dat = list(years = years, ft2m = ft2m),
    quiet = quiet
  )
  # Check years: range
  if (any(years < pars$years$assess) && !quiet) {
    message("`years` < ", pars$years$assess, ".")
  }
  # Check ft2m: range
  if (!all.equal(ft2m, 0.3048, 0.00001) && !quiet) {
    message("`ft2m` is not 0.3048.")
  }
  # Get a small subset of area data
  areas_sm <- areas %>%
    select(
      Region, StatArea, Group, Section, LocationCode, LocationName, geometry
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
    filter(Year %in% years, LocationCode %in% areas_sm$LocationCode) %>%
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
      SpawnNumber, geometry, Start, End, Length, Width, Depth, Method
    ) %>%
    arrange(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber, Start
    )
  # Check for zeros in length (should probably be NA)
  if (any(na.omit(res$Length) == 0)) {
    warning("Zero(s) in Length", call. = FALSE)
  }
  # Check for zeros in width (should probably be NA)
  if (any(na.omit(res$Width) == 0)) {
    warning("Zero(s) in Width", call. = FALSE)
  }
  # Close the connection
  dbDisconnect(conn = access_db)
  # Check output: tibble rows
  check_tibble(dat = list(res = res), quiet = quiet)
  # Check output: names
  if (!all(c(
    "Year", "Region", "StatArea", "Group", "Section", "LocationCode",
    "LocationName", "SpawnNumber", "Start", "End", "Length", "Width", "Depth",
    "Method"
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
#' from underwater surveys \insertCite{GrinnellEtal2022}{SpawnIndex}.
#'
#' @template param-where
#' @template param-areas
#' @template param-quiet
#' @importFrom odbc dbConnect odbc dbDisconnect
#' @importFrom DBI dbReadTable
#' @importFrom dplyr select distinct rename left_join filter %>%
#' @importFrom tibble as_tibble
#' @importFrom Rdpack reprompt
#' @return List with three tables: median region (\code{WidthReg}), section
#'   (\code{WidthSec}), and pool (\code{WidthPool}) widths in metres (m) for the
#'   areas in \code{areas}.
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
#' width_bar <- load_width(where = width_loc, areas = areas)
#' width_bar
load_width <- function(where,
                       areas,
                       quiet = FALSE) {
  # Check where
  check_where(dat = where, dat_names = c(
    "loc", "db", "fns.region_std", "fns.section_std", "fns.pool_std"
  ))
  # Check input: tibble rows
  check_tibble(dat = list(areas = areas), quiet = quiet)
  # Area names
  area_names <- c(
    "SAR", "Region", "RegionName", "StatArea", "Group", "Section",
    "LocationCode", "LocationName", "Pool", "geometry"
  )
  # Check areas: names
  if (!all(area_names %in% names(areas))) {
    stop("`areas` is missing columns", call. = FALSE)
  }
  # Get area information
  areas_sm <- areas %>%
    select(SAR, Region, StatArea, Section, LocationCode, Pool) %>%
    arrange(SAR, Region, StatArea, Section, LocationCode, Pool) %>%
    as_tibble() %>%
    select(-geometry) %>%
    distinct()
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
    mutate(Section = formatC(Section, width=3, format="d", flag="0")) %>%
    left_join(y = areas_sm, by = "Section") %>%
    filter(Section %in% areas_sm$Section) %>%
    select(Region, Section, WidthSec) %>%
    distinct() %>%
    as_tibble()
  # Access the pool worksheet and wrangle
  pool_std <- dbReadTable(conn = access_db, name = where$fns$pool_std) %>%
    rename(Section = SECTION, Pool = BED, WidthPool = WIDMED) %>%
    mutate(
      Section = formatC(Section, width=3, format="d", flag="0"),
      Pool = as.integer(Pool)
    ) %>%
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
    dat = list(region = res$region, section = res$section, pool = res$pool),
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
} # End load_width function

#' Load and wrangle Pacific Herring Section shapefiles.
#'
#' Load and wrangle Pacific Herring Section shapefiles, and aggregate to
#' Statistical Areas and Region(s).
#'
#' @param sections Simple feature collection of polygons; from
#'   \code{\link{sections}}.
#' @template param-areas
#' @template param-quiet
#' @importFrom sf st_read st_bbox st_buffer st_transform st_crs
#' @importFrom dplyr filter select mutate left_join %>% transmute right_join
#'   distinct arrange
#' @return List of simple features (polygons) showing Section, Group,
#'   Statistical Area, and Region boundaries.
#' @references \insertAllCited
#' @seealso \code{\link{sections}}
#' @family load functions
#' @export
#' @examples
#' db_loc <- system.file("extdata", package = "SpawnIndex")
#' area_loc <- list(
#'   loc = db_loc, db = "HerringSpawn.mdb",
#'   fns = list(sections = "Sections", locations = "Location")
#' )
#' data(regions)
#' areas <- load_area_data(reg = "WCVI", where = area_loc)
#' data(sections)
#' polys <- load_sections(sections = sections, areas = areas)
load_sections <- function(sections,
                          areas,
                          quiet = FALSE) {
  # Check input: tibble rows
  check_tibble(dat = list(areas = areas), quiet = quiet)
  # Area names
  area_names <- c(
    "SAR", "Region", "RegionName", "StatArea", "Group", "Section",
    "LocationCode", "LocationName", "Pool", "geometry"
  )
  # Check areas: names
  if (!all(area_names %in% names(areas))) {
    stop("`areas` is missing columns", call. = FALSE)
  }
  # Check cRS (same for areas and polys)
  if (st_crs(areas) != st_crs(sections)){
    stop("CRS must be the same: areas and sections", call. = FALSE)
  }
  # Get area information
  areas_sm <- areas %>%
    select(SAR, Region, StatArea, Group, Section) %>%
    arrange(SAR, Region, StatArea, Group, Section) %>%
    as_tibble() %>%
    select(-geometry) %>%
    distinct()
  # Subset sections to those in areas
  sections <- sections %>%
    filter(Section %in% areas_sm$Section) %>%
    left_join(y = areas_sm, by = "Section") %>%
    select(SAR, Region, StatArea, Group, Section)
  # Dissolve to statistical area
  stat_areas <- sections %>%
    group_by(StatArea) %>%
    summarise() %>%
    ungroup()
  # Dissolve to groups
  groups <- sections %>%
    group_by(Group) %>%
    summarise() %>%
    ungroup()
  # Dissolve to region
  regions <- sections %>%
    group_by(SAR, Region) %>%
    summarise() %>%
    ungroup()
  # Return the spatial objects etc
  return(list(
    sections = sections, stat_areas = stat_areas, groups = groups,
    regions = regions
  ))
} # End load_sections function
