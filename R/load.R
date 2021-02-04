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
#' @param secSub Numeric vector or NULL Subset of Sections to include in the
#'   analysis, or NA to include all the Sections in the region.
#' @param where List. Location of the Pacific Herring "locations" database (see
#'   examples).
#' @param inCRS Character. Input coordinate reference system;
#'   \href{https://spatialreference.org/}{use EPSG codes if desired}.
#' @param outCRS Character. Output coordinate reference system;
#'   \href{https://spatialreference.org/}{use EPSG codes if desired}.
#' @param groups Tibble or NULL. Optional table to add a "Group" column to the
#'   results, say to aggregate data by combinations of Sections. Must have a
#'   column named "Group", and one or more of "StatArea", "Section",
#'   "LocationCode". Set to NULL to ignore (and Group column will be NA).
#' @param quiet Logical. Set to TRUE to prevent messages.
#' @importFrom readr read_csv cols
#' @importFrom dplyr filter select mutate full_join %>% transmute right_join
#' @importFrom tidyr unite
#' @importFrom odbc dbConnect odbc dbDisconnect
#' @importFrom DBI dbReadTable
#' @importFrom tibble as_tibble
#' @importFrom sp SpatialPoints spTransform CRS
#' @return Tibble. Table of geographic information for Pacific Herring: SAR,
#'   Region, Region name, Statistical Area, Group, Section, Location code,
#'   Location name, Pool, Eastings, Northings, Longitude, and Latitude.
#' @seealso \code{\link{HerringSpawn}}
#' @note This function requires 32-bit R to load data from the 32-bit MS Access
#'   database.
#' @export
#' @examples
#' dbLoc <- system.file("extdata", package = "SpawnIndex")
#' areaLoc <- list(
#'   loc = dbLoc, db = "HerringSpawn.mdb",
#'   fns = list(sections = "Sections", locations = "Location")
#' )
#' areas <- LoadAreaData(reg = "WCVI", where = areaLoc)
#' areas
#' secs <- c(231:233, 241, 245)
#' grps <- tibble::tibble(
#'   Section = c(231, 232, 233, 241),
#'   Group = c("Alberni Int", "Barkley", "Barkley", "Tofino Int")
#' )
#' areas2 <- LoadAreaData(
#'   reg = "WCVI", where = areaLoc, groups = grps,
#'   secSub = secs
#' )
#' dplyr::distinct(dplyr::select(areas2, Region, StatArea, Group, Section))
LoadAreaData <- function(reg,
                         secSub = NULL,
                         where,
                         inCRS = "+init=epsg:4326",
                         outCRS = "+init=epsg:3005",
                         groups = NULL,
                         quiet = FALSE) {
  # Warning if R is not 32-bit
  if (.Machine$sizeof.pointer != 4) warning("32-bit R required")
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
  regionNames <- regions %>%
    select(RegionName, Region, Major) %>%
    mutate(Region = paste("(", Region, ")", sep = "")) %>%
    unite(RegionName, Region, col = "Region", sep = " ")
  # Make a nice list
  allRegionNames <- list(
    major = regionNames$Region[regionNames$Major],
    minor = regionNames$Region[!regionNames$Major]
  )
  # Possible regions by type (return to the main level)
  allRegions <- list(
    major = as.character(regions$Region[regions$Major]),
    minor = as.character(regions$Region[!regions$Major])
  )
  # Error if region is incorrect
  if (!(reg %in% c(unlist(allRegions), "All"))) {
    stop("Possible regions are: ", paste(unlist(allRegions), collapse = ", "),
      call. = FALSE
    )
  }
  # Establish connection with access
  accessDB <- dbConnect(
    drv = odbc(),
    .connection_string = paste(
      "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=",
      file.path(where$loc, where$db),
      sep = ""
    )
  )
  # TODO: Sections 132 and 135 are also SoG sections -- how to resolve?
  # Manual fix: Johnstone Strait herring sections
  jsSections <- c(111, 112, 121:127, 131:136)
  # If the region is Johnstone Strait
  if (reg == "JS") {
    # Message
    if (!quiet) {
      cat("Note overlap between JS and SoG: Sections 132 and 135\n")
    }
    # Access the sections worksheet and wrangle
    sections <- dbReadTable(conn = accessDB, name = where$fns$sections)
    # Error if data was not fetched
    if (class(sections) != "data.frame") {
      stop("No data available in MS Access connection")
    }
    # Wrangle the sections worksheet
    sections <- sections %>%
      filter(Section %in% jsSections) %>%
      mutate(SAR = 8) %>%
      full_join(y = regions, by = "SAR") %>%
      filter(Region %in% reg) %>%
      select(SAR, Region, RegionName, Section) %>%
      mutate(Section = as.integer(Section)) %>%
      distinct() %>%
      as_tibble()
  } else { # End if Johnstone Strait, otherwise
    # Wrangle the sections worksheet
    sections <- dbReadTable(conn = accessDB, name = where$fns$sections)
    # Error if data was not fetched
    if (class(sections) != "data.frame") {
      stop("No data available in MS Access connection")
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
  loc <- dbReadTable(conn = accessDB, name = where$fns$locations)
  # Error if data was not fetched
  if (class(loc) != "data.frame") {
    stop("No data available in MS Access connection")
  }
  # Wrangle the locations table
  locDat <- as_tibble(loc) %>%
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
  locSP <- locDat %>%
    transmute(X = Longitude, Y = Latitude)
  # Put X and Y into a spatial points object
  locPts <- SpatialPoints(coords = locSP, proj4string = CRS(inCRS))
  # Convert X and Y from WGS to Albers
  locPtsAlb <- spTransform(x = locPts, CRSobj = CRS(outCRS))
  # Extract spatial info
  dfAlb <- as_tibble(locPtsAlb)
  # Extract relevant location data
  locations <- locDat %>%
    cbind(dfAlb) %>%
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
    locations$Group <- NA
  } else { # End if NULL, otherwise
    # Check for column names
    if (!"Group" %in% names(groups)) {
      stop("Groups table needs a column named 'Group'")
    }
    if (!any("StatArea" %in% names(groups) | "Section" %in% names(groups) |
      "LocationCode" %in% names(groups))) {
      stop("Groups table needs a column named 'StatArea', and/or 'Section',
      and/or 'LocationCode'")
    }
    # Determine matching columns
    grpCols <- which(names(groups) %in% names(locations))
    # Set groups
    locations <- locations %>%
      left_join(y = groups, by = names(groups)[grpCols])
  } # End if not NULL
  # If any groups are NA, check if *some* are missing (i.e., incomplete)
  if (any(is.na(locations$Group))) {
    # Get distinct rows
    grpU <- locations %>%
      select(StatArea, Section, Group) %>%
      distinct() %>%
      arrange(StatArea, Section)
    # Get distinct rows with no missing groups
    grpUNA <- grpU %>%
      filter(is.na(Group))
    # Check if none or all have groups
    noneOrAll <- nrow(grpU) == nrow(grpUNA)
    # Message re some sections(s) missing group info
    if (!noneOrAll & !quiet) {
      cat("Incomplete `Group' info for Section(s): ",
        paste(unique(grpUNA$Section), collapse = ", "), "\n",
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
  if (!is.null(secSub)) {
    # Grab a subset of sections
    res <- res %>%
      filter(Section %in% secSub) %>%
      droplevels()
    # Message
    if (!quiet) {
      cat("Sections: ", paste(secSub, collapse = ", "), "\n", sep = "")
    }
  } # End if subsetting sections
  # Close the connection
  dbDisconnect(conn = accessDB)
  # Error if there is no data
  if (nrow(res) == 0) stop("No locations; check inputs")
  # Return herring areas
  return(res)
} # End LoadAreaData function

#' Get the all spawn table.
#'
#' Get the all spawn table.
#'
#' @param where List. Location of the Pacific Herring understory spawn database
#'   (see examples).
#' @param a Tibble. Table of geographic information indicating the subset of
#'   spawn survey observations to include in calculations; from
#'   \code{\link{LoadAreaData}}.
#' @param yrs Numeric vector. Years(s) to include in the calculations, usually
#'   staring in 1951.
#' @param ft2m Numeric. Conversion factor for feet to metres.
#' @importFrom odbc dbConnect odbc dbDisconnect
#' @importFrom DBI dbReadTable
#' @importFrom dplyr select rename full_join filter mutate %>% arrange ungroup
#' @importFrom tibble as_tibble
#' @importFrom stringr str_to_title
#' @importFrom lubridate as_date
#' @importFrom gfiscamutils MaxNA
#' @importFrom Rdpack reprompt
#' @return Tibble. Contains additional spawn survey data including start and end
#'   dates, as well as spawn length, width, and depth. Other information in this
#'   tibble comes from \code{a}: Region, Statistical Area, Section, and Location
#'   code.
#' @seealso \code{\link{HerringSpawn}} \code{\link{LoadAreaData}}
#' @export
#' @examples
#' dbLoc <- system.file("extdata", package = "SpawnIndex")
#' areaLoc <- list(
#'   loc = dbLoc, db = "HerringSpawn.mdb",
#'   fns = list(sections = "Sections", locations = "Location")
#' )
#' areas <- LoadAreaData(reg = "WCVI", where = areaLoc)
#' allSpawnLoc <- list(
#'   loc = dbLoc, db = "HerringSpawn.mdb",
#'   fns = list(allSpawn = "tSSAllspawn", stations = "tSSStations")
#' )
#' allSpawn <- LoadAllSpawn(where = allSpawnLoc, a = areas, yrs = 2010:2015)
#' allSpawn
LoadAllSpawn <- function(where, a, yrs, ft2m = 0.3048) {
  # Establish connection with access
  accessDB <- dbConnect(
    drv = odbc(),
    .connection_string = paste(
      "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=",
      file.path(where$loc, where$db),
      sep = ""
    )
  )
  # Extract relevant spawn data
  spawn <- dbReadTable(conn = accessDB, name = where$fns$allSpawn) %>%
    rename(LocationCode = Loc_Code, SpawnNumber = Spawn_Number) %>%
    mutate(
      Start = as_date(Start), End = as_date(End),
      Method = str_to_title(Method)
    ) %>%
    filter(Year %in% yrs, LocationCode %in% a$LocationCode) %>%
    select(
      Year, LocationCode, SpawnNumber, Start, End, Length, Width, Method
    ) %>%
    as_tibble()
  # Extrac relevant stations data
  stations <- dbReadTable(conn = accessDB, name = where$fns$stations) %>%
    rename(LocationCode = Loc_Code, SpawnNumber = Spawn_Number) %>%
    filter(LocationCode %in% areas$LocationCode) %>%
    mutate(DepthM = Depth * ft2m * -1) %>%
    group_by(Year, LocationCode, SpawnNumber) %>%
    summarise(Depth = MaxNA(DepthM)) %>%
    ungroup() %>%
    arrange(Year, LocationCode, SpawnNumber)
  # Combine spawn and station data
  spawnStation <- full_join(
    x = spawn, y = stations,
    by = c("Year", "LocationCode", "SpawnNumber")
  )
  # Get a small subset of area data
  areasSm <- a %>%
    select(
      Region, StatArea, Group, Section, LocationCode, LocationName, Eastings,
      Northings, Longitude, Latitude
    ) %>%
    distinct() %>%
    as_tibble()
  # Combine spawn and station data with area data
  res <- spawnStation %>%
    left_join(y = areasSm, by = c("LocationCode")) %>%
    select(
      Year, Region, StatArea, Group, Section, LocationCode, LocationName,
      SpawnNumber, Eastings, Northings, Longitude, Latitude, Start, End,
      Length, Width, Depth, Method
    ) %>%
    arrange(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber, Start
    )
  # Close the connection
  dbDisconnect(conn = accessDB)
  # Return the table
  return(res)
} # End LoadAllSpawn function

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
#' @param a Tibble. Table of geographic information indicating the subset of
#'   spawn survey observations to include in calculations; from
#'   \code{\link{LoadAreaData}}.
#' @importFrom odbc dbConnect odbc dbDisconnect
#' @importFrom DBI dbReadTable
#' @importFrom dplyr select distinct rename left_join filter %>%
#' @importFrom tibble as_tibble
#' @importFrom Rdpack reprompt
#' @return List with three tables: median region (\code{WidthReg}), section
#'   (\code{WidthSec}), and pool (\code{WidthPool}) widths in metres (m) for the
#'   areas in \code{a}.
#' @references \insertAllCited
#' @seealso \code{\link{HerringSpawn}} \code{\link{LoadAreaData}}
#'   \code{\link{CalcSurfSpawn}}
#' @export
#' @examples
#' dbLoc <- system.file("extdata", package = "SpawnIndex")
#' areaLoc <- list(
#'   loc = dbLoc, db = "HerringSpawn.mdb",
#'   fns = list(sections = "Sections", locations = "Location")
#' )
#' areas <- LoadAreaData(reg = "WCVI", where = areaLoc)
#' widthLoc <- list(
#'   loc = dbLoc, db = "HerringSpawn.mdb",
#'   fns = list(
#'     regionStd = "RegionStd", sectionStd = "SectionStd", poolStd = "PoolStd"
#'   )
#' )
#' barWidth <- GetWidth(where = widthLoc, a = areas)
#' barWidth
GetWidth <- function(where, a = areas) {
  # Get area info
  aSm <- a %>%
    select(SAR, Region, StatArea, Section, LocationCode, Pool) %>%
    distinct() %>%
    as_tibble()
  # Establish connection with access
  accessDB <- dbConnect(
    drv = odbc(),
    .connection_string = paste(
      "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=",
      file.path(where$loc, where$db),
      sep = ""
    )
  )
  # Access the region worksheet and wrangle
  regStd <- dbReadTable(conn = accessDB, name = where$fns$regionStd) %>%
    rename(SAR = REGION, WidthReg = WIDMED) %>%
    left_join(y = aSm, by = "SAR") %>%
    filter(SAR %in% aSm$SAR) %>%
    select(Region, WidthReg) %>%
    distinct() %>%
    as_tibble()
  # Access the section worksheet and wrangle
  secStd <- dbReadTable(conn = accessDB, name = where$fns$sectionStd) %>%
    rename(Section = SECTION, WidthSec = WIDMED) %>%
    mutate(Section = as.integer(Section)) %>%
    left_join(y = aSm, by = "Section") %>%
    filter(Section %in% aSm$Section) %>%
    select(Region, Section, WidthSec) %>%
    distinct() %>%
    as_tibble()
  # Access the pool worksheet and wrangle
  poolStd <- dbReadTable(conn = accessDB, name = where$fns$poolStd) %>%
    rename(Section = SECTION, Pool = BED, WidthPool = WIDMED) %>%
    mutate(Section = as.integer(Section), Pool = as.integer(Pool)) %>%
    left_join(y = aSm, by = c("Section", "Pool")) %>%
    filter(Section %in% aSm$Section) %>%
    select(Region, Section, Pool, WidthPool) %>%
    distinct() %>%
    as_tibble()
  # Merge the tables to a list
  res <- list(region = regStd, section = secStd, pool = poolStd)
  # Close the connection
  dbDisconnect(conn = accessDB)
  # Table to return
  return(res)
} # End GetWidth function
