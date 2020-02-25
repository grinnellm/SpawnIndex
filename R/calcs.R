#' Convert number of eggs to spawning biomass.
#'
#' Calculate the conversion factor for Pacific Herring, to convert the number of
#' eggs to the spawn index (i.e., biomass) in tonnes.
#'
#' @param fecundity Numeric. The number of eggs per kilogram of female spawners
#'   \insertCite{Hay1985,HayBrett1988}{SpawnIndex}.
#' @param pFemale Numeric. The proportion of spawners that are female.
#' @return Numeric. The conversion factor for eggs to spawn index in tonnes
#'   (i.e., biomass). Divide the number of eggs by the conversion factor to get
#'   biomass.
#' @references \insertAllCited{}
#' @export
#' @examples
#' CalcEggConversion()
CalcEggConversion <- function(fecundity = 200000,
                              pFemale = 0.5) {
  # Eggs per tonne: eggs/kilogram female * proportion female * kilograms/tonne
  eggsPerTonne <- fecundity * pFemale * 1000
  # Return the conversion factor
  return(eggsPerTonne)
} # End CalcEggConversion function

#' Calculate spawning biomass from spawn-on-kelp (SOK) harvest.
#'
#' Calculate spawning biomass in tonnes from spawn-on-kelp (SOK) harvest in
#' kilograms.
#'
#' @param SOK Numeric. Weight of spawn-on-kelp (SOK) harvest in kilograms.
#' @param eggKelpProp Numeric. Proportion of SOK product that is eggs, not kelp
#'   \insertCite{ShieldsEtal1985}{SpawnIndex}.
#' @param eggBrineProp Numeric. Pproportion of SOK product that is eggs after
#'   brining \insertCite{WhyteEnglar1977}{SpawnIndex}.
#' @param eggWt Numeric. Average weight in kilograms of a fertilized egg
#'   \insertCite{HayMiller1982}{SpawnIndex}.
#' @param f Numeric. Egg conversion factor (eggs to biomass) from
#'   \code{\link{CalcEggConversion}}.
#' @return Numeric. Spawning biomass in tonnes.
#' @references \insertAllCited{}
#' @seealso \code{\link{CalcEggConversion}}
#' @export
#' @examples
#' CalcBiomassSOK(SOK = 100, f = CalcEggConversion())
CalcBiomassSOK <- function(SOK,
                           eggKelpProp = 0.88,
                           eggBrineProp = 1 / 1.13,
                           eggWt = 2.38 * 10^-6,
                           f) {
  # Spawnin biomass in tonnes: (kg SOK * proportion eggs * proportion eggs) /
  # (kg per egg * eggs per tonne )
  SB <- (SOK * eggKelpProp * eggBrineProp) / (eggWt * f)
  # Return the spawning biomass
  return(SB)
} # End CalcBiomassSOK

#' Calculate the surface spawn index.
#'
#' Calculate the Pacific Herring surface spawn index in tonnes.
#'
#' @param where List. Location of the Pacific Herring surface spawn database
#'   (see examples).
#' @param a Tibble. Table of geographic information indicating the subset of
#'   spawn survey observations to inlude in calculations. Returned from
#'   \code{\link{LoadAreaData}}.
#' @param yrs Numeric vector. Years(s) to include in the calculations, usually
#'   staring in 1951.
#' @param intYrs Numeric vector. Years where intensity categores are used to
#'   determine egg layers (default yrs[yrs < 1979]).
#' @param rsYrs Numeric vector. Years where intensity needs to be re-scaled from
#'   5 to 9 categories (default intYrs[intYrs < 1951]).
#' @param f Numeric. Egg conversion factor (eggs to biomass) from
#'   \code{\link{CalcEggConversion}}.
#' @importFrom RODBC odbcConnectAccess sqlFetch odbcClose
#' @importFrom dplyr select distinct rename left_join filter
#' @importFrom tibble as_tibble
#' @importFrom stringr str_to_title
#' @importFrom gfiscamutils MeanNA SumNA
#' @importFrom tidyr replace_na
#' @return List. The element `SI` is a tibble with surface spawn index
#'   (`SurfSI`) in tonnes by spawn number and year. The spawn number is the
#'   finest spatial scale at which we calculate the spawn index. Other
#'   information in this tibble comes from `a`: Region, Statistical Area,
#'   Section, and Location code.
#' @references \insertRef{SchweigertEtal1997}{SpawnIndex}
#' @note The 'spawn index' is a relative index of spawning biomass.
#' @seealso \code{\link{LoadAreaData}} \code{\link{CalcEggConversion}}
#' @export
#' @examples
#' dbLoc <- system.file("extdata", package = "SpawnIndex")
#' areaLoc <- list(
#'   loc = dbLoc, db = "HerringSpawn.mdb",
#'   fns = list(sections = "Sections", locations = "Location")
#' )
#' areas <- LoadAreaData(reg = "WCVI", where = areaLoc)
#' surfLoc <- list(
#'   loc = dbLoc, db = "HerringSpawn.mdb",
#'   fns = list(
#'     regionStd = "RegionStd", sectionStd = "SectionStd", poolStd = "PoolStd",
#'     surface = "tSSSurface", intensity = "Intensity",
#'     allSpawn = "tSSAllspawn"
#'   )
#' )
#' surfSpawn <- CalcSurfSpawn(
#'   where = surfLoc, a = areas, yrs = 2010:2015,
#'   f = CalcEggConversion()
#' )
#' surfSpawn$SI
CalcSurfSpawn <- function(where,
                          a,
                          yrs,
                          intYrs = yrs[yrs < 1979],
                          rsYrs = intYrs[intYrs < 1951],
                          f) {
  # Establish connection with access
  accessDB <- RODBC::odbcConnectAccess(access.file = file.path(
    where$loc,
    where$db
  ))
  # Get a small subset of area data
  areasSm <- a %>%
    dplyr::select(SAR, Region, StatArea, Section, LocationCode, Bed) %>%
    dplyr::distinct() %>%
    tibble::as_tibble()
  # Access the region worksheet and wrangle
  regStd <- RODBC::sqlFetch(channel = accessDB, sqtable = where$fns$regionStd) %>%
    dplyr::rename(SAR = REGION, WidthReg = WIDMED) %>%
    dplyr::left_join(y = areasSm, by = "SAR") %>%
    dplyr::filter(SAR %in% areasSm$SAR) %>%
    dplyr::select(SAR, Region, WidthReg) %>%
    dplyr::distinct() %>%
    tibble::as_tibble()
  # Access the section worksheet and wrangle
  secStd <- RODBC::sqlFetch(
    channel = accessDB,
    sqtable = where$fns$sectionStd
  ) %>%
    dplyr::rename(Section = SECTION, WidthSec = WIDMED) %>%
    dplyr::filter(Section %in% a$Section) %>%
    dplyr::select(Section, WidthSec) %>%
    dplyr::distinct() %>%
    tibble::as_tibble()
  # Access the bed worksheet and wrangle
  bedStd <- RODBC::sqlFetch(channel = accessDB, sqtable = where$fns$poolStd) %>%
    dplyr::rename(Section = SECTION, Bed = BED, WidthBed = WIDMED) %>%
    dplyr::filter(Section %in% a$Section) %>%
    dplyr::select(Section, Bed, WidthBed) %>%
    dplyr::distinct() %>%
    tibble::as_tibble()
  # Load intensity categories and egg layers (Humphreys and Haegele 1976, Hay
  # and Kronlund 1987)
  intensity <<- RODBC::sqlFetch(
    channel = accessDB,
    sqtable = where$fns$intensity
  ) %>%
    dplyr::rename(Layers = AvgLayersFromIntensity) %>%
    tibble::as_tibble()
  # Load all spawn
  spawn <- RODBC::sqlFetch(channel = accessDB, sqtable = where$fns$allSpawn) %>%
    dplyr::rename(
      LocationCode = Loc_Code, SpawnNumber = Spawn_Number,
      WidthObs = Width
    ) %>%
    dplyr::mutate(Method = stringr::str_to_title(Method)) %>%
    dplyr::filter(Year %in% yrs, LocationCode %in% a$LocationCode) %>%
    dplyr::select(
      Year, LocationCode, SpawnNumber, Length, WidthObs,
      Method
    ) %>%
    tibble::as_tibble()
  # Extract relevant surface data
  surface <- RODBC::sqlFetch(channel = accessDB, sqtable = where$fns$surface) %>%
    dplyr::rename(LocationCode = Loc_Code, SpawnNumber = Spawn_Number) %>%
    dplyr::filter(Year %in% yrs, LocationCode %in% a$LocationCode) %>%
    dplyr::left_join(y = areasSm, by = "LocationCode") %>%
    dplyr::left_join(y = spawn, by = c("Year", "LocationCode", "SpawnNumber")) %>%
    tidyr::replace_na(replace = list(
      Lay_Grass = 0, Grass_Percent = 0,
      Lay_Rockweed = 0, Rockweed_Percent = 0,
      Lay_Kelp = 0, Kelp_Percent = 0,
      Lay_Brown_Algae = 0, Brown_Algae_Percent = 0,
      Lay_Leafy_Red = 0, Leafy_Red_Percent = 0,
      Lay_Stringy_Red = 0, Stringy_Red_Percent = 0,
      Lay_Rock = 0, Rock_Percent = 0, Lay_Other = 0,
      Other_Percent = 0
    )) %>%
    # Substrate i: EggLyrs_i
    dplyr::mutate(
      Grass = Lay_Grass * Grass_Percent / 100,
      Rockweed = Lay_Rockweed * Rockweed_Percent / 100,
      Kelp = Lay_Kelp * Kelp_Percent / 100,
      BrownAlgae = Lay_Brown_Algae * Brown_Algae_Percent / 100,
      LeafyRed = Lay_Leafy_Red * Leafy_Red_Percent / 100,
      StringyRed = Lay_Stringy_Red * Stringy_Red_Percent / 100,
      Rock = Lay_Rock * Rock_Percent / 100,
      Other = Lay_Other * Other_Percent / 100
    ) %>%
    tibble::as_tibble()
  # Grab the percent cover data
  pCover <- surface %>%
    dplyr::select(dplyr::ends_with("Percent"))
  # Error if any percents are greater than 100
  if (any(pCover > 100, na.rm = TRUE)) {
    stop("Percent cover > 100 in surface spawn data", call. = FALSE)
  }
  # Continue with calculating egg layers
  surface <- surface %>%
    # Sample j: EggLyrs_j
    dplyr::mutate(
      EggLyrs = Grass + Rockweed + Kelp + BrownAlgae + LeafyRed +
        StringyRed + Rock + Other,
      Intensity = ifelse(Year %in% rsYrs & Intensity > 0,
        Intensity * 2 - 1, Intensity
      )
    ) %>%
    dplyr::filter(Method %in% c("Surface", "Dive")) %>%
    dplyr::select(
      Year, Region, StatArea, Section, LocationCode, Bed,
      SpawnNumber, Length, WidthObs, Intensity, EggLyrs
    )
  # Fill-in missing egg layers manually
  surface <- surface %>%
    dplyr::mutate(
      # SoG (1 record): update Intensity from 0 to 1 (surveyed but not reported)
      Intensity = ifelse(Year == 1962 & StatArea == 14 & Section == 142 &
        LocationCode == 820 & Intensity == 0, 1, Intensity)
    )
  # Calculate egg density based on intensity or direct measurements
  eggs <- surface %>%
    dplyr::left_join(y = intensity, by = "Intensity") %>%
    dplyr::mutate(
      EggLyrs = ifelse(Year %in% intYrs, Layers, EggLyrs),
      # Egg density in thousands (eggs * 10^3 / m^2; Schweigert et al.
      # 1997). Yes, thousands: the report is wrong (J. Schweigert,
      # personal communication, 21 February 2017)
      # Sampe j: EggDens_j
      EggDens = EggLyrs * 212.218 + 14.698
    )
  # These are the 'original' manual updates that were in the Microsoft Access
  # database: some overwrite good data with no documented reason and have been
  # omitted, others have been omitted because the spawn survey was incomplete.
  # However, they (and others) are still present in the Microsoft Access
  # database, causing discrepancies for HG 1979, as well as WCVI 1982 and 1984.
  # They should get removed from the Microsoft Access database (especially
  # updates 1, 4, and 5 which cause errros). Update 2 is still relevant;
  # updates 3 and 6 no longer have an effect.
  # 1. HG (15 records): Year 1979, SA 2, Intensity 4 (update EggLyrs to
  #    2.1496 using intensity table; 14 records overwrite good data)
  # 2. SoG (1 record): Year 1962, SA 14, Intensity 0 (update EggLyrs to
  #    0.5529 using intensity 1: spawn was surveyed but not reported)
  # 3. WCVI (4 records): Year 1981, SA 24, EggLyrs 0 (update EggLyrs to
  #    0.5529 using intensity table)
  # 4. WCVI (7 records): Year 1982, SA 23, Intensity 3 (update EggLyrs to
  #    1.3360 using intensity table; 7 records overwrite good data)
  # 5. WCVI (41 records): Year 1984, SA 24, Intensity 0 (update EggLyrs to
  #    2.33 -- not sure why/how; 41 records overwrite good data)
  # 6. A27 (14 records): Year 1982, SA 27, EggLyrs 0 (update EggLyrs to
  #    2.98 using a historical average)
  # Get the number of records with no egg layer info
  noLayers <- eggs %>% dplyr::filter(EggLyrs == 0) # %>%
  # left_join( y=select(areas, LocationCode, LocationName),
  #   by="LocationCode" ) %>%
  # select( Year, Region, StatArea, Section, LocationCode, LocationName,
  #   SpawnNumber ) %>%
  # arrange( Year, Region, StatArea, Section, LocationCode, SpawnNumber ) %>%
  # write_csv( path=file.path(regName, "NoEggs.csv") )
  # Error if there are missing values
  if (nrow(noLayers) > 0) {
    stop("Missing egg layers for ", nrow(noLayers),
      " record(s):", print(noLayers),
      sep = ""
    )
  }
  # Output egg layer info
  eggLyrs <- eggs %>%
    dplyr::group_by(
      Year, Region, StatArea, Section, LocationCode,
      SpawnNumber
    ) %>%
    dplyr::summarise(SurfLyrs = gfiscamutils::MeanNA(EggLyrs)) %>%
    dplyr::ungroup()
  # Calculate egg density per spawn number/bed
  eggsSpawn <- eggs %>%
    dplyr::group_by(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber,
      Bed
    ) %>%
    # Spawn s: bar(EggDens_s)
    dplyr::summarise(EggDens = gfiscamutils::MeanNA(EggDens)) %>%
    dplyr::ungroup()
  # Calculate annual fish biomass by spawn number/bed
  biomassSpawn <- eggsSpawn %>%
    dplyr::left_join(y = spawn, by = c("Year", "LocationCode", "SpawnNumber")) %>%
    dplyr::left_join(y = regStd, by = "Region") %>%
    dplyr::left_join(y = secStd, by = "Section") %>%
    dplyr::left_join(y = bedStd, by = c("Section", "Bed")) %>%
    # Width is set to bed, section, region, or observed width (in that order)
    dplyr::mutate(
      Width = WidthBed,
      Width = ifelse(is.na(Width), WidthSec, Width),
      Width = ifelse(is.na(Width), WidthReg, Width),
      Width = ifelse(is.na(Width), WidthObs, Width),
      # Biomass in tonnes, based on Hay (1985), and Hay and Brett (1988)
      SurfSI = EggDens * Length * Width * 1000 / f
    ) %>%
    # Group to account for 'bed' level (want 'spawn' level)
    dplyr::group_by(
      Year, Region, StatArea, Section, LocationCode,
      SpawnNumber
    ) %>%
    # Spawn s: SurfSI_s
    dplyr::summarise(SurfSI = gfiscamutils::SumNA(SurfSI)) %>%
    dplyr::ungroup() %>%
    dplyr::full_join(y = eggLyrs, by = c(
      "Year", "Region", "StatArea", "Section",
      "LocationCode", "SpawnNumber"
    ))
  # Calculate annual SI by spawn number
  SI <- biomassSpawn %>%
    dplyr::select(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber,
      SurfSI
    )
  # Close the connection
  RODBC::odbcClose(accessDB)
  # Return the data
  return(list(
    surface = surface, eggs = eggs, eggsSpawn = eggsSpawn,
    biomassSpawn = biomassSpawn, SI = SI
  ))
} # End CalcSurfSpawn function

#' Calculate the Macrocystis spawn index
#'
#' Calculate the Pacific Herring Macrocystis spawn index in tonnes. Note that
#' the 'spawn index' is a relative index of spawning biomass.
#'
#' @param where List. Location of the Pacific Herring surface spawn database
#'   (see examples).
#' @param a Tibble. Table of geographic information indicating the subset of
#'   spawn survey observations to inlude in calculations. Returned from
#'   \code{\link{LoadAreaData}}.
#' @param yrs Numeric vector. Years(s) to include in the calculations, usually
#'   staring in 1951.
#' @param tSwath Numeric. Transect swath (i.e., width) in metres.
#' @param f Numeric. Egg conversion factor (eggs to biomass) from
#'   \code{\link{CalcEggConversion}}.
#' @importFrom RODBC odbcConnectAccess sqlFetch odbcClose
#' @importFrom dplyr select distinct rename left_join filter
#' @importFrom tibble as_tibble
#' @importFrom stringr str_to_title
#' @importFrom gfiscamutils MeanNA SumNA UniqueNA
#' @importFrom tidyr replace_na
#' @return List. The element `SI` is a tibble with Macrocystis spawn index
#'   (`MacroSI`) in tonnes by spawn number and year. The spawn number is the
#'   finest spatial scale at which we calculate the spawn index. Other
#'   information in this tibble comes from `a`: Region, Statistical Area,
#'   Section, and Location code.
#' @references \insertRef{HaegeleSchweigert1990}{SpawnIndex}
#' @note The 'spawn index' is a relative index of spawning biomass.
#' @seealso \code{\link{LoadAreaData}} \code{\link{CalcEggConversion}}
#' @export
#' @examples
#' dbLoc <- system.file("extdata", package = "SpawnIndex")
#' areaLoc <- list(
#'   loc = dbLoc, db = "HerringSpawn.mdb",
#'   fns = list(sections = "Sections", locations = "Location")
#' )
#' areas <- LoadAreaData(reg = "WCVI", where = areaLoc)
#' macroLoc <- list(
#'   loc = file.path(dbLoc), db = "HerringSpawn.mdb",
#'   fns = list(
#'     allSpawn = "tSSAllspawn", plants = "tSSMacPlant",
#'     transects = "tSSMacTrans"
#'   )
#' )
#' macroSpawn <- CalcMacroSpawn(
#'   where = macroLoc, a = areas, yrs = 2010:2015,
#'   f = CalcEggConversion()
#' )
#' macroSpawn$SI
CalcMacroSpawn <- function(where, a, yrs, tSwath = 2, f) {
  # Establish connection with access
  accessDB <- RODBC::odbcConnectAccess(access.file = file.path(
    where$loc,
    where$db
  ))
  # Load all spawn
  spawn <- RODBC::sqlFetch(channel = accessDB, sqtable = where$fns$allSpawn) %>%
    dplyr::rename(
      LocationCode = Loc_Code, SpawnNumber = Spawn_Number,
      LengthMacro = Length_Macrocystis
    ) %>%
    dplyr::mutate(Method = stringr::str_to_title(Method)) %>%
    dplyr::filter(Year %in% yrs, LocationCode %in% a$LocationCode) %>%
    dplyr::select(Year, LocationCode, SpawnNumber, LengthMacro, Length, Method) %>%
    tibble::as_tibble()
  # Get plant-level data
  plants <- RODBC::sqlFetch(channel = accessDB, sqtable = where$fns$plants) %>%
    dplyr::rename(LocationCode = Loc_Code, SpawnNumber = Spawn_Number) %>%
    dplyr::filter(
      Year %in% yrs, LocationCode %in% a$LocationCode,
      !is.na(Mature)
    ) %>%
    dplyr::select(Year, LocationCode, SpawnNumber, Transect, Mature) %>%
    tibble::as_tibble()
  # Get a small subset of area data
  areasSm <- a %>%
    dplyr::select(Region, StatArea, Section, LocationCode, Bed) %>%
    dplyr::distinct() %>%
    tibble::as_tibble()
  # Get transect-level data
  transects <- RODBC::sqlFetch(
    channel = accessDB,
    sqtable = where$fns$transects
  ) %>%
    dplyr::rename(LocationCode = Loc_Code, SpawnNumber = Spawn_Number) %>%
    dplyr::filter(Year %in% yrs, LocationCode %in% a$LocationCode) %>%
    dplyr::left_join(y = areasSm, by = "LocationCode") %>%
    dplyr::select(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber,
      Transect, Height, Width, Layers
    ) %>%
    tibble::as_tibble()
  # Merge the data
  dat <- transects %>%
    dplyr::left_join(y = plants, by = c(
      "Year", "LocationCode", "SpawnNumber", "Transect"
    )) %>%
    tidyr::replace_na(replace = list(Mature = 0)) %>%
    dplyr::mutate(Swath = tSwath)
  # Calculate transect-level data
  datTrans <- dat %>%
    dplyr::group_by(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber,
      Transect
    ) %>%
    # Transect t: Width_t, Swath, Area_t, bar(Height_t), bar(EggLyrs_t),
    # and Stalks_t, P_t
    dplyr::summarise(
      # Transect metrics for all transects (not just those with mature plants)
      Width = unique(Width),
      Swath = unique(Swath),
      Area = Width * Swath,
      # Plant metrics for mature plants only
      Height = gfiscamutils::UniqueNA(Height[Mature > 0]),
      EggLyrs = gfiscamutils::UniqueNA(Layers[Mature > 0]),
      Stalks = gfiscamutils::SumNA(Mature[Mature > 0]),
      Plants = length(Mature[Mature > 0])
    ) %>%
    dplyr::ungroup()
  # Calculate spawn-level data
  biomassSpawn <- datTrans %>%
    dplyr::left_join(y = spawn, by = c("Year", "LocationCode", "SpawnNumber")) %>%
    dplyr::mutate(
      LengthMacro = ifelse(is.na(LengthMacro), Length, LengthMacro)
    ) %>%
    dplyr::filter(Method %in% c("Surface", "Dive")) %>%
    dplyr::group_by(
      Year, Region, StatArea, Section, LocationCode,
      SpawnNumber
    ) %>%
    # Spawn s: bar(Width_s), Area_s, P_s, Stalks_s, bar(Height_s),
    # bar(EggLyrs_s), bar(StalksPerPlant_s)
    dplyr::summarise(
      LengthMacro = unique(LengthMacro),
      Width = gfiscamutils::MeanNA(Width),
      Area = gfiscamutils::SumNA(Area),
      Plants = gfiscamutils::SumNA(Plants),
      Stalks = gfiscamutils::SumNA(Stalks),
      Height = gfiscamutils::MeanNA(Height),
      EggLyrs = gfiscamutils::MeanNA(EggLyrs),
      StalksPerPlant = Stalks / Plants,
      # Eggs per plant in thousands (eggs * 10^3 / plant; Haegele and
      # Schweigert 1990)
      # Spawn s: bar(EggsPerPlant_s)
      EggsPerPlant = 0.073 * EggLyrs^0.673 * Height^0.932 *
        StalksPerPlant^0.703 * 1000,
      # Eggs density in thousands (eggs * 10^3 / m^2)
      # Spawn s: bar(EggDens_s)
      EggDens = EggsPerPlant * Plants / Area,
      # Biomass in tonnes, based on Hay (1985), and Hay and Brett (1988)
      # Spawn s: MacroSI_s
      MacroSI = EggDens * LengthMacro * Width * 1000 / f
    ) %>%
    dplyr::rename(MacroLyrs = EggLyrs) %>%
    dplyr::ungroup()
  # Return the macrocystis spawn
  SI <- biomassSpawn %>%
    dplyr::select(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber,
      MacroSI
    )
  # Close the connection
  RODBC::odbcClose(accessDB)
  # Return the data
  return(list(
    dat = dat, datTrans = datTrans, biomassSpawn = biomassSpawn,
    SI = SI
  ))
} # End CalcMacroSpawn function
