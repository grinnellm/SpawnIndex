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
#' @param secSub Numeric vector or NA. Subset of Sections to include in the
#'   analysis, or NA to include all the Sections in the region.
#' @param where List. Location of the Pacfic Herring "locations" database (see
#'   examples).
#' @param inCRS Chracter. Input coordinate reference system;
#'   \href{https://spatialreference.org/}{use EPSG codes if desired}.
#' @param outCRS Chracter. Output coordinate reference system;
#'   \href{https://spatialreference.org/}{use EPSG codes if desired}.
#' @param quiet Logical. Set to TRUE to prevent messages.
#' @importFrom readr read_csv cols
#' @importFrom dplyr filter select mutate full_join %>%
#' @importFrom tidyr unite
#' @importFrom RODBC odbcConnectAccess sqlFetch odbcClose
#' @importFrom tibble as_tibble
#' @importFrom sp SpatialPoints spTransform CRS
#' @return Tibble. Table of geographic information for Pacific Herring: SAR,
#'   Region, Region name, Statistical Area, Group, Section, Location code,
#'   Location name, Pool, Eastings, Northings, Longitude, and Latitute.
#' @note This function requires 32-bit R to load data from the 32-bit MS Access
#'   database.
#' @export
#' @examples
#' dbLoc <- system.file("extdata", package = "SpawnIndex")
#' areaLoc <- list(
#'   loc = dbLoc, db = "HerringSpawn.mdb",
#'   fns = list(sections = "Sections", locations = "Location")
#' )
#' LoadAreaData(reg = "WCVI", where = areaLoc)
LoadAreaData <- function(reg,
                         secSub = NA,
                         where,
                         inCRS = "+init=epsg:4326",
                         outCRS = "+init=epsg:3005",
                         quiet = FALSE) {
  # Cross-walk table for SAR to region and region name
  regions <- readr::read_csv(
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
    col_types = readr::cols("i", "c", "c", "l")
  )
  # If region isn't JS, remove it
  if (!reg %in% c("JS", "All")) {
    regions <- dplyr::filter(.data = regions, SAR != 8)
  }
  # Return the regions table to the main environment
  regions <- regions
  # Return region names
  regionNames <- regions %>%
    dplyr::select(RegionName, Region, Major) %>%
    dplyr::mutate(Region = paste("(", Region, ")", sep = "")) %>%
    tidyr::unite(RegionName, Region, col = "Region", sep = " ")
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
  accessDB <- RODBC::odbcConnectAccess(access.file = file.path(
    where$loc,
    where$db
  ))
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
    sections <- RODBC::sqlFetch(
      channel = accessDB,
      sqtable = where$fns$sections
    )
    # Error if data was not fetched
    if (class(sections) != "data.frame") {
      stop("No data available in MS Access connection")
    }
    sections <- sections %>%
      dplyr::filter(Section %in% jsSections) %>%
      dplyr::mutate(SAR = 8) %>%
      dplyr::full_join(y = regions, by = "SAR") %>%
      dplyr::filter(Region %in% reg) %>%
      dplyr::select(SAR, Region, RegionName, Section) %>%
      dplyr::distinct() %>%
      tibble::as_tibble()
  } else { # End if Johnstone Strait, otherwise
    # Access the sections worksheet and wrangle
    sections <- RODBC::sqlFetch(
      channel = accessDB,
      sqtable = where$fns$sections
    )
    # Error if data was not fetched
    if (class(sections) != "data.frame") {
      stop("No data available in MS Access connection")
    }
    sections <- sections %>%
      dplyr::full_join(y = regions, by = "SAR") %>%
      dplyr::select(SAR, Region, RegionName, Section) %>%
      dplyr::distinct() %>%
      tibble::as_tibble()
    # If we only want a specific region
    if (reg != "All") {
      # Remove areas outside SARs, and other regions
      sections <- sections %>%
        dplyr::filter(SAR != -1, Region == reg)
    } # End if we only want a specific region
  } # End if the region is not Johnstone Strait
  # Access the locations worksheet
  loc <- RODBC::sqlFetch(channel = accessDB, sqtable = where$fns$locations)
  # Error if data was not fetched
  if (class(loc) != "data.frame") {
    stop("No data available in MS Access connection")
  }
  # Wrangle the locations table
  locDat <- tibble::as_tibble(loc) %>%
    dplyr::select(
      Loc_Code, Location, StatArea, Section, Bed, Location_Latitude,
      Location_Longitude
    ) %>%
    dplyr::mutate(Location = as.character(Location)) %>%
    dplyr::rename(
      LocationCode = Loc_Code, LocationName = Location, StatArea = StatArea,
      Section = Section, Latitude = Location_Latitude,
      Longitude = Location_Longitude, Pool = Bed
    ) %>%
    tidyr::replace_na(replace = list(Longitude = 0, Latitude = 0)) %>%
    dplyr::select(
      LocationCode, LocationName, Pool, Section, StatArea, Longitude,
      Latitude
    ) %>%
    dplyr::arrange(LocationCode) %>%
    dplyr::distinct()
  # Grab the spatial info (X and Y)
  locSP <- locDat %>%
    dplyr::transmute(X = Longitude, Y = Latitude)
  # Put X and Y into a spatial points object
  locPts <- sp::SpatialPoints(coords = locSP, proj4string = sp::CRS(inCRS))
  # Convert X and Y from WGS to Albers
  locPtsAlb <- sp::spTransform(x = locPts, CRSobj = sp::CRS(outCRS))
  # Extract spatial info
  dfAlb <- tibble::as_tibble(locPtsAlb)
  # Extract relevant location data
  locations <- locDat %>%
    cbind(dfAlb) %>%
    dplyr::mutate(
      Eastings = ifelse(is.na(Longitude), Longitude, X),
      Northings = ifelse(is.na(Latitude), Latitude, Y)
    ) %>%
    dplyr::select(
      StatArea, Section, LocationCode, LocationName, Pool, Eastings,
      Northings, Latitude, Longitude
    ) %>%
    dplyr::filter(Section %in% sections$Section) %>%
    dplyr::distinct() %>%
    tibble::as_tibble()
  # Intialize an additional column for groups: NA
  locations$Group <- NA
  # Manually determine groups: Haida Gwaii
  # locations$Group[locations$Section %in% c(6)] <- "Louscoone"
  locations$Group[locations$Section %in% c(6)] <- "006"
  locations$Group[locations$Section %in% c(11)] <- "Massett"
  locations$Group[locations$Section %in% c(12)] <- "Naden"
  # locations$Group[locations$Section %in% c(21, 25)] <- "Juan Perez/Skincuttle"
  locations$Group[locations$Section %in% c(21, 25)] <- "021&025"
  locations$Group[locations$Section %in% c(22)] <- "E Skidegate"
  # locations$Group[locations$Section %in% c(23)] <- "Cumshewa"
  # locations$Group[locations$Section %in% c(24)] <- "Selwyn"
  locations$Group[locations$Section %in% c(23, 24)] <- "023&024"
  # Manually determine groups: Prince Rupert District
  locations$Group[locations$Section %in% c(31:33, 40:42)] <- "Big Bay"
  locations$Group[locations$Section %in% c(43, 50:53)] <- "Kitkatla"
  # Manually determine groups: Central Coast
  locations$Group[locations$Section %in% c(67, 70:78)] <- "06&07"
  locations$Group[locations$Section %in% c(85, 86)] <- "08"
  # Manually determine groups: Strait of Georgia
  locations$Group[locations$Section %in% c(132, 135, 141)] <- "Lazo"
  locations$Group[locations$Section %in% c(140, 142, 143, 170:172)] <- "14&17"
  locations$Group[locations$Section %in% c(150:152, 160:165, 280, 291, 292)] <-
    "ESoG"
  locations$Group[locations$Section %in% c(173, 180:182, 190:193)] <- "SDodd"
  # Manually determine groups: West Coast Vancouver Island
  locations$Group[locations$Section %in% c(231)] <- "Alberni Inlet"
  locations$Group[locations$Section %in% c(232, 233)] <- "Barkley"
  #  locations$Group[locations$Section %in% c(230, 239)] <- "SA 23 Unkn"
  locations$Group[locations$Section %in% c(241)] <- "Tofino Inlet"
  locations$Group[locations$Section %in% c(242)] <- "Hesquiat"
  locations$Group[locations$Section %in% c(243)] <- "Hootla Kootla"
  locations$Group[locations$Section %in% c(244)] <- "Ahousaht"
  locations$Group[locations$Section %in% c(245)] <- "Vargas Island"
  #  locations$Group[locations$Section %in% c(240, 249)] <- "SA 24 Unkn"
  locations$Group[locations$Section %in% c(251, 252)] <- "Nootka"
  locations$Group[locations$Section %in% c(253)] <- "Nuchatlitz/Ehattesaht"
  #  locations$Group[locations$Section %in% c(250, 259)] <- "SA 25 Unkn"
  # Manually determine groups: Area 2 West
  locations$Group[locations$Section %in% c(0)] <- "No group"
  locations$Group[locations$Section %in% c(2)] <- "Pt Louis/Chanal"
  locations$Group[locations$Section %in% c(3, 4)] <- "Engelfield/Rennell"
  locations$Group[locations$Section %in% c(1, 5)] <- "Southwest"
  # Manually determine groups: Area 27
  locations$Group[locations$Section %in% c(271:274)] <- "No Group"
  locations$Group[locations$Section %in% c(270)] <- "No Group"
  # Set groups to NA if using all the data
  if (reg == "All") locations$Group <- NA
  # If any groups are NA, check if *some* are missing (i.e., incomplete)
  if (any(is.na(locations$Group))) {
    # Get distinct rows
    grpU <- locations %>%
      dplyr::select(StatArea, Section, Group) %>%
      dplyr::distinct() %>%
      dplyr::arrange(StatArea, Section)
    # Get distinct rows with no missing groups
    grpUNA <- grpU %>%
      dplyr::filter(is.na(Group))
    # Check if none or all have groups
    noneOrAll <- nrow(grpU) == nrow(grpUNA)
    # Message re some sections(s) missing group info
    if (!noneOrAll & !quiet) {
      cat("Incomplete `Group' info for Section(s): ",
        paste(grpUNA$Section, collapse = ", "), "\n",
        sep = ""
      )
    }
  } # End if any groups are NA
  # Extract required data
  res <- locations %>%
    dplyr::right_join(y = sections, by = "Section") %>%
    dplyr::filter(!is.na(StatArea), !is.na(Section)) %>%
    dplyr::select(
      SAR, Region, RegionName, StatArea, Group, Section, LocationCode,
      LocationName, Pool, Eastings, Northings, Longitude, Latitude
    ) %>%
    #      mutate( StatArea=formatC(StatArea, width=2, format="d", flag="0"),
    #          Section=formatC(Section, width=3, format="d", flag="0") ) %>%
    dplyr::arrange(Region, StatArea, Group, Section, LocationCode) %>%
    dplyr::distinct() %>%
    droplevels()
  # If not all sections are included
  if (!all(is.na(secSub))) {
    # Grab a subset of sections
    res <- res %>%
      dplyr::filter(Section %in% secSub) %>%
      droplevels()
    # Message
    if (!quiet) {
      cat("Sections: ", paste(secSub, collapse = ", "), "\n", sep = "")
    }
  } # End if subsetting areas
  # Close the connection
  RODBC::odbcClose(accessDB)
  # Error if there is no data
  if(nrow(res)==0) stop("No locations; check inputs")
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
#'   spawn survey observations to inlude in calculations. Returned from
#'   \code{\link{LoadAreaData}}.
#' @param yrs Numeric vector. Years(s) to include in the calculations, usually
#'   staring in 1951.
#' @param ft2m Numeric. Conversion factor for feet to metres.
#' @importFrom RODBC odbcConnectAccess sqlFetch odbcClose
#' @importFrom dplyr select rename full_join filter mutate %>%
#' @importFrom tibble as_tibble
#' @importFrom stringr str_to_title
#' @importFrom lubridate as_date
#' @importFrom gfiscamutils MaxNA
#' @importFrom Rdpack reprompt
#' @return Tibble. Contains additional spawn survey data including start and end
#'   dates, as well as spawn length, width, and depth. Other information in this
#'   tibble comes from `a`: Region, Statistical Area, Section, and Location
#'   code.
#' @seealso \code{\link{LoadAreaData}}
#' @export
#' @examples
#' dbLoc <- system.file("extdata", package = "SpawnIndex")
#' areaLoc <- list(
#'   loc = dbLoc, db = "HerringSpawn.mdb",
#'   fns = list(sections = "Sections", locations = "Location")
#' )
#' areas <- LoadAreaData(reg = "WCVI", where = areaLoc)
#' allSpawnLoc <- list(
#'   loc = file.path(dbLoc), db = "HerringSpawn.mdb",
#'   fns = list(allSpawn = "tSSAllspawn", stations = "tSSStations")
#' )
#' allSpawn <- LoadAllSpawn(
#'   where = allSpawnLoc, a = areas, yrs = 2010:2015
#' )
#' allSpawn
LoadAllSpawn <- function(where, a, yrs, ft2m = 0.3048) {
  # Establish connection with access
  accessDB <- RODBC::odbcConnectAccess(access.file = file.path(
    where$loc,
    where$db
  ))
  # Extract relevant spawn data
  spawn <- RODBC::sqlFetch(channel = accessDB, sqtable = where$fns$allSpawn) %>%
    dplyr::rename(LocationCode = Loc_Code, SpawnNumber = Spawn_Number) %>%
    dplyr::mutate(
      Start = lubridate::as_date(Start), End = lubridate::as_date(End),
      Method = stringr::str_to_title(Method)
    ) %>%
    dplyr::filter(Year %in% yrs, LocationCode %in% a$LocationCode) %>%
    dplyr::select(
      Year, LocationCode, SpawnNumber, Start, End, Length, Width, Method
    ) %>%
    tibble::as_tibble()
  # Extrac relevant stations data
  stations <- sqlFetch(channel = accessDB, sqtable = where$fns$stations) %>%
    dplyr::rename(LocationCode = Loc_Code, SpawnNumber = Spawn_Number) %>%
    dplyr::filter(LocationCode %in% areas$LocationCode) %>%
    dplyr::mutate(DepthM = Depth * ft2m * -1) %>%
    dplyr::group_by(Year, LocationCode, SpawnNumber) %>%
    dplyr::summarise(Depth = gfiscamutils::MaxNA(DepthM)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(Year, LocationCode, SpawnNumber)
  # Combine spawn and station data
  spawnStation <- dplyr::full_join(
    x = spawn, y = stations,
    by = c("Year", "LocationCode", "SpawnNumber")
  )
  # Get a small subset of area data
  areasSm <- a %>%
    dplyr::select(
      Region, StatArea, Group, Section, LocationCode, LocationName, Eastings,
      Northings, Longitude, Latitude
    ) %>%
    dplyr::distinct() %>%
    tibble::as_tibble()
  # Combine spawn and station data with area data
  res <- spawnStation %>%
    dplyr::left_join(y = areasSm, by = c("LocationCode")) %>%
    dplyr::select(
      Year, Region, StatArea, Group, Section, LocationCode, LocationName,
      SpawnNumber, Eastings, Northings, Longitude, Latitude, Start, End,
      Length, Width, Depth, Method
    ) %>%
    dplyr::arrange(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber, Start
    )
  # Close the connection
  RODBC::odbcClose(accessDB)
  # Return the table
  return(res)
} # End LoadAllSpawn function

#' Load median spawn width.
#'
#' Load median spawn width in metres (m) for Pacific Herring surface spawn index
#' calculations.
#'
#' @param where List. Location of the Pacific Herring surface spawn database
#'   (see examples).
#' @param a Tibble. Table of geographic information indicating the subset of
#'   spawn survey observations to inlude in calculations. Returned from
#'   \code{\link{LoadAreaData}}.
#' @importFrom RODBC odbcConnectAccess sqlFetch odbcClose
#' @importFrom dplyr select distinct rename left_join filter %>%
#' @importFrom tibble as_tibble
#' @importFrom Rdpack reprompt
#' @return Table with median region (WidthReg), section (WidthSec), and pool
#'   (WidthPool) widths in metres (m) for the areas in \code{a}.
#' @seealso \code{\link{CalcSurfSpawn}} \code{\link{LoadAreaData}}
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
    dplyr::select(SAR, Region, StatArea, Section, LocationCode, Pool) %>%
    dplyr::distinct() %>%
    tibble::as_tibble()
  # Establish connection with access
  accessDB <- RODBC::odbcConnectAccess(access.file = file.path(
    where$loc,
    where$db
  ))
  # Access the region worksheet and wrangle
  regStd <- RODBC::sqlFetch(channel = accessDB, sqtable = where$fns$regionStd) %>%
    dplyr::rename(SAR = REGION, WidthReg = WIDMED) %>%
    dplyr::left_join(y = aSm, by = "SAR") %>%
    dplyr::filter(SAR %in% aSm$SAR) %>%
    dplyr::select(Region, WidthReg) %>%
    dplyr::distinct() %>%
    tibble::as_tibble()
  # Access the section worksheet and wrangle
  secStd <- RODBC::sqlFetch(
    channel = accessDB,
    sqtable = where$fns$sectionStd
  ) %>%
    dplyr::rename(Section = SECTION, WidthSec = WIDMED) %>%
    dplyr::left_join(y = aSm, by = "Section") %>%
    dplyr::filter(Section %in% aSm$Section) %>%
    dplyr::select(Region, Section, WidthSec) %>%
    dplyr::distinct() %>%
    tibble::as_tibble()
  # Access the pool worksheet and wrangle
  poolStd <- RODBC::sqlFetch(
    channel = accessDB, sqtable = where$fns$poolStd
  ) %>%
    dplyr::rename(Section = SECTION, Pool = BED, WidthPool = WIDMED) %>%
    dplyr::left_join(y = aSm, by = c("Section", "Pool")) %>%
    dplyr::filter(Section %in% aSm$Section) %>%
    dplyr::select(Region, Section, Pool, WidthPool) %>%
    dplyr::distinct() %>%
    tibble::as_tibble()
  # Merge the tables
  res <- regStd %>%
    dplyr::left_join(y = secStd, by = "Region") %>%
    dplyr::left_join(y = poolStd, by = c("Region", "Section")) %>%
    dplyr::arrange(Region, Section, Pool) %>%
    dplyr::distinct()
  # Close the connection
  RODBC::odbcClose(accessDB)
  # Table to return
  return(res)
} # End GetWidth function
