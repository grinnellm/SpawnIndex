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
#' @importFrom readr read_csv cols
#' @importFrom dplyr filter select mutate full_join %>%
#' @importFrom tidyr unite
#' @importFrom RODBC odbcConnectAccess sqlFetch odbcClose
#' @importFrom tibble as_tibble
#' @importFrom sp SpatialPoints spTransform CRS
#' @return Tibble. Table of geographic information for Pacific Herring: SAR,
#'   Region, Region name, Statistical Area, Group, Section, Location code,
#'   Location name, Bed, Eastings, Northings, Longitude, and Latitute.
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
                         outCRS = "+init=epsg:3005") {
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
    cat("Note overlap between JS and SoG: Sections 132 and 135\n")
    # Access the sections worksheet and wrangle
    sections <- RODBC::sqlFetch(channel = accessDB,
                                sqtable = where$fns$sections)
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
    sections <- RODBC::sqlFetch(channel = accessDB,
                                sqtable = where$fns$sections)
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
      Longitude = Location_Longitude
    ) %>%
    tidyr::replace_na(replace = list(Longitude = 0, Latitude = 0)) %>%
    dplyr::select(
      LocationCode, LocationName, Bed, Section, StatArea, Longitude,
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
      StatArea, Section, LocationCode, LocationName, Bed, Eastings,
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
    if (!noneOrAll) {
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
      LocationName, Bed, Eastings, Northings, Longitude, Latitude
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
    cat("Sections: ", paste(secSub, collapse = ", "), "\n", sep = "")
  } # End if subsetting areas
  # Close the connection
  RODBC::odbcClose(accessDB)
  # Return herring areas
  return(res)
} # End LoadAreaData function
