#' Convert number of eggs to spawning biomass.
#'
#' Calculate the conversion factor for the number of Pacific Herring eggs to the
#' spawn index (i.e., biomass) in tonnes.
#'
#' @param omega Numeric. The number of eggs per kilogram of female spawners;
#'   from \code{\link{pars}}.
#' @param female Numeric. The proportion of spawners that are female; from
#'   \code{\link{pars}}.
#' @importFrom Rdpack reprompt
#' @return Numeric. The conversion factor for eggs to spawn index in tonnes
#'   (i.e., biomass). Divide the number of eggs by the conversion factor to get
#'   biomass.
#' @seealso \code{\link{pars}}
#' @export
#' @examples
#' data(pars)
#' CalcEggConversion()
CalcEggConversion <- function(omega = pars$conversion$omega,
                              female = pars$conversion$female) {
  # Eggs per tonne: eggs/kilogram female * proportion female * kilograms/tonne
  theta <- omega * female * 1000
  # Return the conversion factor
  return(theta)
} # End CalcEggConversion function

#' Calculate spawning biomass from spawn-on-kelp (SOK) harvest.
#'
#' Calculate spawning biomass in tonnes from spawn-on-kelp (SOK) harvest in
#' kilograms.
#'
#' @param SOK Numeric. Weight of spawn-on-kelp (SOK) harvest in kilograms.
#' @param nu Numeric. Proportion of SOK product that is kelp; from
#'   \code{\link{pars}}.
#' @param upsilon Numeric. SOK product weight increase due to brining as a
#'   proportion; from \code{\link{pars}}.
#' @param M Numeric. Average weight in kilograms of a fertilized egg; from
#'   \code{\link{pars}}.
#' @param theta Numeric. Egg conversion factor (eggs to biomass); from
#'   \code{\link{CalcEggConversion}}.
#' @importFrom Rdpack reprompt
#' @return Numeric. Spawning biomass in tonnes.
#' @seealso \code{\link{CalcEggConversion}} \code{\link{pars}}
#' @export
#' @examples
#' data(pars)
#' CalcBiomassSOK(SOK = 100)
CalcBiomassSOK <- function(SOK,
                           nu = pars$SOK$nu,
                           upsilon = pars$SOK$upsilon,
                           M = pars$SOK$M,
                           theta = CalcEggConversion()) {
  # Spawnin biomass in tonnes: (kg SOK * proportion eggs * proportion eggs) /
  # (kg per egg * eggs per tonne )
  SB <- (SOK * (1 - nu) * 1 / (1 + upsilon)) / (M * theta)
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
#'   spawn survey observations to inlude in calculations; from
#'   \code{\link{LoadAreaData}}.
#' @param widths List. List of three tables: median region, section, and pool
#'   widths in metres (m); from \code{\link{GetWidth}}.
#' @param yrs Numeric vector. Years(s) to include in the calculations, usually
#'   staring in 1951.
#' @param intense Tibble. Table of spawn intensity categories and number of egg
#'   layers; from \code{\link{intensity}}.
#' @param intYrs Numeric vector. Years where intensity categores are used to
#'   determine egg layers.
#' @param rsYrs Numeric vector. Years where intensity needs to be re-scaled from
#'   5 to 9 categories.
#' @param alpha Numeric. Regression intercept; from \code{\link{pars}}
#'   \insertCite{SchweigertEtal1997}{SpawnIndex}.
#' @param beta Numeric. Regression slope; from \code{\link{pars}}
#'   \insertCite{SchweigertEtal1997}{SpawnIndex}.
#' @param theta Numeric. Egg conversion factor (eggs to biomass); from
#'   \code{\link{CalcEggConversion}}.
#' @importFrom odbc dbConnect odbc dbDisconnect
#' @importFrom DBI dbReadTable
#' @importFrom dplyr select distinct rename left_join filter %>% ends_with
#'   ungroup mutate_at vars starts_with ends_with
#' @importFrom tibble as_tibble
#' @importFrom stringr str_to_title
#' @importFrom gfiscamutils MeanNA SumNA
#' @importFrom tidyr replace_na
#' @importFrom Rdpack reprompt
#' @return List. The element \code{SI} is a tibble with surface spawn index
#'   (\code{SurfSI}) in tonnes by spawn number and year. The spawn number is the
#'   finest spatial scale at which we calculate the spawn index. Other
#'   information in this tibble comes from \code{a}: Region, Statistical Area,
#'   Section, and Location code.
#' @references \insertAllCited
#' @note The `spawn index' is a relative index of spawning biomass.
#' @seealso \code{\link{HerringSpawn}} \code{\link{LoadAreaData}}
#'   \code{\link{GetWidth}} \code{\link{CalcEggConversion}} \code{\link{pars}}
#'   \code{\link{intensity}}
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
#' data(pars)
#' data(intensity)
#' surfLoc <- list(
#'   loc = dbLoc, db = "HerringSpawn.mdb",
#'   fns = list(surface = "tSSSurface", allSpawn = "tSSAllspawn")
#' )
#' surfSpawn <- CalcSurfSpawn(
#'   where = surfLoc, a = areas, widths = barWidth, yrs = 2010:2015
#' )
#' surfSpawn$SI
CalcSurfSpawn <- function(where,
                          a,
                          widths,
                          yrs,
                          intense = intensity,
                          intYrs = yrs[yrs < 1979],
                          rsYrs = intYrs[intYrs < 1951],
                          alpha = pars$surface$alpha,
                          beta = pars$surface$beta,
                          theta = CalcEggConversion()) {
  # Establish connection with access
  accessDB <- dbConnect(
    drv = odbc(),
    .connection_string = paste(
      "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=",
      file.path(where$loc, where$db),
      sep = ""
    )
  )
  # Get a small subset of area data
  areasSm <- a %>%
    select(SAR, Region, StatArea, Section, LocationCode, Pool) %>%
    distinct() %>%
    as_tibble()
  # Load all spawn
  spawn <- dbReadTable(conn = accessDB, name = where$fns$allSpawn) %>%
    rename(
      LocationCode = Loc_Code, SpawnNumber = Spawn_Number, WidthObs = Width
    ) %>%
    mutate(Method = str_to_title(Method)) %>%
    filter(Year %in% yrs, LocationCode %in% a$LocationCode) %>%
    select(
      Year, LocationCode, SpawnNumber, Length, WidthObs, Method
    ) %>%
    as_tibble()
  # Extract relevant surface data
  surface <- dbReadTable(conn = accessDB, name = where$fns$surface) %>%
    rename(LocationCode = Loc_Code, SpawnNumber = Spawn_Number) %>%
    filter(Year %in% yrs, LocationCode %in% a$LocationCode) %>%
    left_join(y = areasSm, by = "LocationCode") %>%
    left_join(
      y = spawn,
      by = c("Year", "LocationCode", "SpawnNumber")
    ) %>%
    replace_na(replace = list(
      Lay_Grass = 0, Grass_Percent = 0, Lay_Rockweed = 0, Rockweed_Percent = 0,
      Lay_Kelp = 0, Kelp_Percent = 0, Lay_Brown_Algae = 0,
      Brown_Algae_Percent = 0, Lay_Leafy_Red = 0, Leafy_Red_Percent = 0,
      Lay_Stringy_Red = 0, Stringy_Red_Percent = 0, Lay_Rock = 0,
      Rock_Percent = 0, Lay_Other = 0, Other_Percent = 0
    )) %>%
    mutate_at(.vars = vars(starts_with("Lay_")), .funs = as.numeric) %>%
    mutate_at(.vars = vars(ends_with("_Percent")), .funs = as.numeric) %>%
    mutate(Intensity = as.integer(Intensity)) %>%
    # Substrate i
    mutate(
      Grass = Lay_Grass * Grass_Percent / 100,
      Rockweed = Lay_Rockweed * Rockweed_Percent / 100,
      Kelp = Lay_Kelp * Kelp_Percent / 100,
      BrownAlgae = Lay_Brown_Algae * Brown_Algae_Percent / 100,
      LeafyRed = Lay_Leafy_Red * Leafy_Red_Percent / 100,
      StringyRed = Lay_Stringy_Red * Stringy_Red_Percent / 100,
      Rock = Lay_Rock * Rock_Percent / 100,
      Other = Lay_Other * Other_Percent / 100
    ) %>%
    as_tibble()
  # Grab the percent cover data
  pCover <- surface %>%
    select(ends_with("Percent"))
  # Error if any percents are greater than 100
  if (any(pCover > 100, na.rm = TRUE)) {
    stop("Percent cover > 100 in surface spawn data", call. = FALSE)
  }
  # Continue with calculating egg layers
  surface <- surface %>%
    # Sample j
    mutate(
      EggLyrs = Grass + Rockweed + Kelp + BrownAlgae + LeafyRed +
        StringyRed + Rock + Other,
      Intensity = ifelse(Year %in% rsYrs & Intensity > 0,
        Intensity * 2 - 1, Intensity
      )
    ) %>%
    filter(Method %in% c("Surface", "Dive")) %>%
    select(
      Year, Region, StatArea, Section, LocationCode, Pool, SpawnNumber, Length,
      WidthObs, Intensity, EggLyrs
    )
  # Fill-in missing egg layers manually
  surface <- surface %>%
    mutate(
      # SoG (1 record): update Intensity from 0 to 1 (surveyed but not reported)
      Intensity = ifelse(Year == 1962 & StatArea == 14 & Section == 142 &
        LocationCode == 820 & Intensity == 0, 1, Intensity)
    )
  # Calculate egg density based on intensity or direct measurements
  eggs <- surface %>%
    left_join(y = intense, by = "Intensity") %>%
    mutate(
      EggLyrs = ifelse(Year %in% intYrs, Layers, EggLyrs),
      # Egg density in thousands (eggs * 10^3 / m^2; Schweigert et al.
      # 1997). Yes, thousands: the report is wrong (J. Schweigert,
      # personal communication, 21 February 2017); sample j
      EggDens = alpha + beta * EggLyrs
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
  noLayers <- eggs %>% filter(EggLyrs == 0) # %>%
  # left_join( y=select(areas, LocationCode, LocationName),
  #   by="LocationCode" ) %>%
  # select( Year, Region, StatArea, Section, LocationCode, LocationName,
  #   SpawnNumber ) %>%
  # arrange( Year, Region, StatArea, Section, LocationCode, SpawnNumber ) %>%
  # write_csv( path=file.path(regName, "NoEggs.csv") )
  # Error if there are missing values
  if (nrow(noLayers) > 0) {
    stop("Missing egg layers for ", nrow(noLayers), " record(s):",
      print(noLayers),
      sep = ""
    )
  }
  # Output egg layer info
  eggLyrs <- eggs %>%
    group_by(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber
    ) %>%
    summarise(SurfLyrs = MeanNA(EggLyrs)) %>%
    ungroup()
  # Calculate egg density per spawn number/pool
  eggsSpawn <- eggs %>%
    group_by(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber, Pool
    ) %>%
    # Spawn s
    summarise(EggDens = MeanNA(EggDens)) %>%
    ungroup()
  # Calculate annual fish biomass by spawn number/pool
  biomassSpawn <- eggsSpawn %>%
    left_join(y = spawn, by = c("Year", "LocationCode", "SpawnNumber")) %>%
    left_join(y = widths$region, by = "Region") %>%
    left_join(y = widths$section, by = c("Region", "Section")) %>%
    left_join(y = widths$pool, by = c("Region", "Section", "Pool")) %>%
    # Width is set to pool, section, region, or observed width (in that order)
    mutate(
      Width = WidthPool,
      Width = ifelse(is.na(Width), WidthSec, Width),
      Width = ifelse(is.na(Width), WidthReg, Width),
      Width = ifelse(is.na(Width), WidthObs, Width),
      # Biomass in tonnes, based on Hay (1985), and Hay and Brett (1988)
      SurfSI = EggDens * Length * Width * 1000 / theta
    ) %>%
    # Group to account for 'pool' level (want 'spawn' level)
    group_by(Year, Region, StatArea, Section, LocationCode, SpawnNumber) %>%
    # Spawn s
    summarise(SurfSI = SumNA(SurfSI)) %>%
    ungroup() %>%
    full_join(
      y = eggLyrs,
      by = c(
        "Year", "Region", "StatArea", "Section", "LocationCode", "SpawnNumber"
      )
    )
  # Calculate annual SI by spawn number
  SI <- biomassSpawn %>%
    select(Year, Region, StatArea, Section, LocationCode, SpawnNumber, SurfSI)
  # Close the connection
  dbDisconnect(conn = accessDB)
  # Return the data
  return(list(
    surface = surface, eggs = eggs, eggsSpawn = eggsSpawn,
    biomassSpawn = biomassSpawn, SI = SI
  ))
} # End CalcSurfSpawn function

#' Calculate the Macrocystis spawn index.
#'
#' Calculate the Pacific Herring Macrocystis spawn index in tonnes.
#'
#' @param where List. Location of the Pacific Herring Macrocystis spawn database
#'   (see examples).
#' @param a Tibble. Table of geographic information indicating the subset of
#'   spawn survey observations to inlude in calculations; from
#'   \code{\link{LoadAreaData}}.
#' @param yrs Numeric vector. Years(s) to include in the calculations, usually
#'   staring in 1951.
#' @param tSwath Numeric. Transect swath (i.e., width) in metres.
#' @param xi Numeric. Regression slope; from \code{\link{pars}}
#'   \insertCite{HaegeleSchweigert1990}{SpawnIndex}.
#' @param gamma Numeric. Regression exponent on egg layers; from
#'   \code{\link{pars}} \insertCite{HaegeleSchweigert1990}{SpawnIndex}.
#' @param delta Numeric. Regression exponent on plant height; from
#'   \code{\link{pars}} \insertCite{HaegeleSchweigert1990}{SpawnIndex}.
#' @param epsilon Numeric. Regression exponent on numnber of stalks per plant;
#'   from \code{\link{pars}} \insertCite{HaegeleSchweigert1990}{SpawnIndex}.
#' @param theta Numeric. Egg conversion factor (eggs to biomass); from
#'   \code{\link{CalcEggConversion}}.
#' @importFrom odbc dbConnect odbc dbDisconnect
#' @importFrom DBI dbReadTable
#' @importFrom dplyr select distinct rename left_join filter %>% group_by
#'   summarise ungroup
#' @importFrom tibble as_tibble
#' @importFrom stringr str_to_title
#' @importFrom gfiscamutils MeanNA SumNA UniqueNA
#' @importFrom tidyr replace_na
#' @importFrom Rdpack reprompt
#' @return List. The element \code{SI} is a tibble with Macrocystis spawn index
#'   (\code{MacroSI}) in tonnes by spawn number and year. The spawn number is
#'   the finest spatial scale at which we calculate the spawn index. Other
#'   information in this tibble comes from \code{a}: Region, Statistical Area,
#'   Section, and Location code.
#' @references \insertAllCited
#' @note The `spawn index' is a relative index of spawning biomass.
#' @seealso \code{\link{HerringSpawn}} \code{\link{LoadAreaData}}
#'   \code{\link{CalcEggConversion}} \code{\link{pars}}
#' @export
#' @examples
#' dbLoc <- system.file("extdata", package = "SpawnIndex")
#' areaLoc <- list(
#'   loc = dbLoc, db = "HerringSpawn.mdb",
#'   fns = list(sections = "Sections", locations = "Location")
#' )
#' areas <- LoadAreaData(reg = "WCVI", where = areaLoc)
#' data(pars)
#' macroLoc <- list(
#'   loc = dbLoc, db = "HerringSpawn.mdb",
#'   fns = list(
#'     allSpawn = "tSSAllspawn", plants = "tSSMacPlant",
#'     transects = "tSSMacTrans"
#'   )
#' )
#' macroSpawn <- CalcMacroSpawn(where = macroLoc, a = areas, yrs = 2010:2015)
#' macroSpawn$SI
CalcMacroSpawn <- function(where,
                           a,
                           yrs,
                           tSwath = 2,
                           xi = pars$macrocystis$xi,
                           gamma = pars$macrocystis$gamma,
                           delta = pars$macrocystis$delta,
                           epsilon = pars$macrocystis$epsilon,
                           theta = CalcEggConversion()) {
  # Establish connection with access
  accessDB <- dbConnect(
    drv = odbc(),
    .connection_string = paste(
      "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=",
      file.path(where$loc, where$db),
      sep = ""
    )
  )
  # Load all spawn
  spawn <- dbReadTable(conn = accessDB, name = where$fns$allSpawn) %>%
    rename(
      LocationCode = Loc_Code, SpawnNumber = Spawn_Number,
      LengthMacro = Length_Macrocystis
    ) %>%
    mutate(Method = str_to_title(Method)) %>%
    filter(Year %in% yrs, LocationCode %in% a$LocationCode) %>%
    select(
      Year, LocationCode, SpawnNumber, LengthMacro, Length, Method
    ) %>%
    as_tibble()
  # Get plant-level data
  plants <- dbReadTable(conn = accessDB, name = where$fns$plants) %>%
    rename(LocationCode = Loc_Code, SpawnNumber = Spawn_Number) %>%
    filter(
      Year %in% yrs, LocationCode %in% a$LocationCode,
      !is.na(Mature)
    ) %>%
    select(Year, LocationCode, SpawnNumber, Transect, Mature) %>%
    as_tibble()
  # Get a small subset of area data
  areasSm <- a %>%
    select(Region, StatArea, Section, LocationCode, Pool) %>%
    distinct() %>%
    as_tibble()
  # Get transect-level data
  transects <- dbReadTable(conn = accessDB, name = where$fns$transects) %>%
    rename(LocationCode = Loc_Code, SpawnNumber = Spawn_Number) %>%
    filter(Year %in% yrs, LocationCode %in% a$LocationCode) %>%
    left_join(y = areasSm, by = "LocationCode") %>%
    select(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber, Transect,
      Height, Width, Layers
    ) %>%
    as_tibble()
  # Merge the data
  dat <- transects %>%
    left_join(y = plants, by = c(
      "Year", "LocationCode", "SpawnNumber", "Transect"
    )) %>%
    replace_na(replace = list(Mature = 0)) %>%
    mutate(Swath = tSwath)
  # Calculate transect-level data
  datTrans <- dat %>%
    group_by(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber, Transect
    ) %>%
    # Transect t
    summarise(
      # Transect metrics for all transects (not just those with mature plants)
      Width = unique(Width),
      Swath = unique(Swath),
      Area = Width * Swath,
      # Plant metrics for mature plants only
      Height = UniqueNA(Height[Mature > 0]),
      EggLyrs = UniqueNA(Layers[Mature > 0]),
      Stalks = SumNA(Mature[Mature > 0]),
      Plants = length(Mature[Mature > 0])
    ) %>%
    ungroup()
  # Calculate spawn-level data
  biomassSpawn <- datTrans %>%
    left_join(
      y = spawn,
      by = c("Year", "LocationCode", "SpawnNumber")
    ) %>%
    mutate(
      LengthMacro = ifelse(is.na(LengthMacro), Length, LengthMacro)
    ) %>%
    filter(Method %in% c("Surface", "Dive")) %>%
    group_by(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber
    ) %>%
    # Spawn s
    summarise(
      LengthMacro = unique(LengthMacro),
      Width = MeanNA(Width),
      Area = SumNA(Area),
      Plants = SumNA(Plants),
      Stalks = SumNA(Stalks),
      Height = MeanNA(Height),
      EggLyrs = MeanNA(EggLyrs),
      StalksPerPlant = Stalks / Plants,
      # Eggs per plant in thousands (eggs * 10^3 / plant; Haegele and
      # Schweigert 1990); spawn s
      EggsPerPlant = xi * EggLyrs^gamma * Height^delta *
        StalksPerPlant^epsilon * 1000,
      # Eggs density in thousands (eggs * 10^3 / m^2; spawn s
      EggDens = EggsPerPlant * Plants / Area,
      # Biomass in tonnes, based on Hay (1985), and Hay and Brett (1988); spawn
      # s
      MacroSI = EggDens * LengthMacro * Width * 1000 / theta
    ) %>%
    rename(MacroLyrs = EggLyrs) %>%
    ungroup()
  # Return the macrocystis spawn
  SI <- biomassSpawn %>%
    select(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber, MacroSI
    )
  # Close the connection
  dbDisconnect(conn = accessDB)
  # Return the data
  return(list(
    dat = dat, datTrans = datTrans, biomassSpawn = biomassSpawn, SI = SI
  ))
} # End CalcMacroSpawn function

#' Calculate the understory spawn index.
#'
#' Calculate the Pacific Herring understory spawn index in tonnes.
#'
#' @param where List. Location of the Pacific Herring understory spawn database
#'   (see examples).
#' @param a Tibble. Table of geographic information indicating the subset of
#'   spawn survey observations to inlude in calculations; from
#'   \code{\link{LoadAreaData}}.
#' @param yrs Numeric vector. Years(s) to include in the calculations, usually
#'   staring in 1951.
#' @param algCoefs Tibble. Table of algae coefficients; from
#'   \code{\link{algaeCoefs}}.
#' @param tau Tibble. Table of understory spawn width adjustment factors from
#'   \code{\link{underWidthFac}}.
#' @param varphi Numeric. Regression slope for substrate; from
#'   \code{\link{pars}} \insertCite{HaegeleEtal1979}{SpawnIndex}.
#' @param vartheta Numeric. Regression slope for algae; from \code{\link{pars}}
#'   \insertCite{Schweigert2005}{SpawnIndex}.
#' @param varrho Numeric. Regression exponent on number of egg layers; from
#'   \code{\link{pars}} \insertCite{Schweigert2005}{SpawnIndex}.
#' @param varsigma Numeric. Regression exponent on proportion of algae; from
#'   \code{\link{pars}} \insertCite{Schweigert2005}{SpawnIndex}.
#' @param theta Numeric. Egg conversion factor (eggs to biomass); from
#'   \code{\link{CalcEggConversion}}.
#' @importFrom odbc dbConnect odbc dbDisconnect
#' @importFrom DBI dbReadTable
#' @importFrom dplyr select distinct rename left_join filter %>% ungroup
#'   bind_rows
#' @importFrom tibble as_tibble
#' @importFrom stringr str_to_title str_to_upper
#' @importFrom gfiscamutils MeanNA SumNA UniqueNA WtMeanNA
#' @importFrom tidyr replace_na gather
#' @importFrom Rdpack reprompt
#' @return List. The element \code{SI} is a tibble with understory spawn index
#'   (\code{UnderSI}) in tonnes by spawn number and year. The spawn number is
#'   the finest spatial scale at which we calculate the spawn index. Other
#'   information in this tibble comes from \code{a}: Region, Statistical Area,
#'   Section, and Location code.
#' @references \insertAllCited
#' @note The `spawn index' is a relative index of spawning biomass.
#' @seealso \code{\link{HerringSpawn}} \code{\link{LoadAreaData}}
#'   \code{\link{CalcEggConversion}} \code{\link{pars}} \code{\link{algaeCoefs}}
#' @export
#' @examples
#' dbLoc <- system.file("extdata", package = "SpawnIndex")
#' areaLoc <- list(
#'   loc = dbLoc, db = "HerringSpawn.mdb",
#'   fns = list(sections = "Sections", locations = "Location")
#' )
#' areas <- LoadAreaData(reg = "WCVI", where = areaLoc)
#' underLoc <- list(
#'   loc = dbLoc, db = "HerringSpawn.mdb",
#'   fns = list(
#'     allSpawn = "tSSAllspawn", algTrans = "tSSVegTrans",
#'     stations = "tSSStations", algae = "tSSVegetation"
#'   )
#' )
#' data(underWidthFac)
#' data(pars)
#' data(algaeCoefs)
#' underSpawn <- CalcUnderSpawn(where = underLoc, a = areas, yrs = 2010:2015)
#' underSpawn$SI
CalcUnderSpawn <- function(where,
                           a,
                           yrs,
                           algCoefs = algaeCoefs,
                           tau = underWidthFac,
                           varphi = pars$understory$varphi,
                           vartheta = pars$understory$vartheta,
                           varrho = pars$under$varrho,
                           varsigma = pars$understory$varsigma,
                           theta = CalcEggConversion()) {
  # Establish connection with access
  accessDB <- dbConnect(
    drv = odbc(),
    .connection_string = paste(
      "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=",
      file.path(where$loc, where$db),
      sep = ""
    )
  )
  # Get a small subset of area data
  areasSm1 <- a %>%
    select(Region, LocationCode) %>%
    distinct() %>%
    as_tibble()
  # Load all spawn
  spawn <- dbReadTable(conn = accessDB, name = where$fns$allSpawn) %>%
    rename(
      LocationCode = Loc_Code, SpawnNumber = Spawn_Number,
      LengthAlgae = Length_Vegetation
    ) %>%
    mutate(Method = str_to_title(Method)) %>%
    filter(Year %in% yrs, LocationCode %in% a$LocationCode) %>%
    select(
      Year, LocationCode, SpawnNumber, LengthAlgae, Length, Method
    ) %>%
    as_tibble()
  # Load algae transects
  algTrans <- dbReadTable(conn = accessDB, name = where$fns$algTrans) %>%
    rename(
      LocationCode = Loc_Code, SpawnNumber = Spawn_Number,
      QuadratSize = Quadrat_Size, WidthObs = Width_Recorded
    ) %>%
    filter(Year %in% yrs, LocationCode %in% a$LocationCode) %>%
    select(
      Year, LocationCode, SpawnNumber, Transect, WidthObs, QuadratSize
    ) %>%
    left_join(y = areasSm1, by = "LocationCode") %>%
    as_tibble()
  # Correction factors for region(s) by year (to fix lead line shrinkage issue)
  widthFacs <- tau %>%
    gather(key = Region, value = WidthFac, -Year)
  # Merge the width factors and correct transect widths
  algTrans <- algTrans %>%
    left_join(y = widthFacs, by = c("Year", "Region")) %>%
    replace_na(replace = list(WidthFac = 1.0)) %>%
    mutate(Width = WidthObs * WidthFac)
  # Error if any quadrats are not 0.5 m^2
  if (any(algTrans$QuadratSize != 0.5)) {
    stop("All quadrats must be 0.5m^2", call. = FALSE)
  }
  # Load station data
  stations <- dbReadTable(conn = accessDB, name = where$fns$stations) %>%
    rename(
      LocationCode = Loc_Code, SpawnNumber = Spawn_Number,
      SubLyrs = Layers_Bottom
    ) %>%
    filter(Year %in% yrs, LocationCode %in% a$LocationCode) %>%
    mutate(SubProp = Percent_Bottom / 100) %>%
    select(
      Year, LocationCode, SpawnNumber, Transect, Station, SubLyrs, SubProp
    ) %>%
    as_tibble()
  # Get egg layer info: substrate
  eggLyrsSub <- stations %>%
    group_by(Year, LocationCode, SpawnNumber, Transect) %>%
    summarise(Layers = MeanNA(SubLyrs)) %>%
    ungroup() %>%
    mutate(Source = "Substrate")
  # Load algae
  algae <- dbReadTable(conn = accessDB, name = where$fns$algae) %>%
    rename(
      LocationCode = Loc_Code, SpawnNumber = Spawn_Number,
      AlgType = Type_Vegetation, AlgLyrs = Layers_Vegetation
    ) %>%
    filter(Year %in% yrs, LocationCode %in% a$LocationCode) %>%
    mutate(
      AlgType = str_to_upper(AlgType),
      AlgProp = Percent_Vegetation / 100,
      AlgProp = ifelse(AlgProp > 1, 1, AlgProp)
    ) %>%
    select(
      Year, LocationCode, SpawnNumber, Transect, Station, AlgType, AlgLyrs,
      AlgProp
    ) %>%
    as_tibble()
  # Get egg layer info: algae
  eggLyrsAlg <- algae %>%
    group_by(Year, LocationCode, SpawnNumber, Transect) %>%
    summarise(Layers = MeanNA(AlgLyrs)) %>%
    ungroup() %>%
    mutate(Source = "Algae")
  # Combine egg layer info
  eggLyrs <- bind_rows(eggLyrsSub, eggLyrsAlg) %>%
    group_by(Year, LocationCode, SpawnNumber, Transect) %>%
    summarise(Layers = MeanNA(Layers)) %>%
    group_by(Year, LocationCode, SpawnNumber) %>%
    summarise(UnderLyrs = MeanNA(Layers)) %>%
    ungroup()
  # If there are missing algae types
  if (any(!algae$AlgType %in% algCoefs$AlgType)) {
    # Get missing algae type(s)
    missAlg <- unique(algae$AlgType[!algae$AlgType %in%
      algCoefs$AlgType])
    # Error, and show missing type(s)
    stop("Missing algae type(s): ", paste(missAlg, collapse = ", "),
      call. = FALSE
    )
  } # End if there are missing algae types
  # Get a small subset of area data
  areasSm2 <- a %>%
    select(Region, StatArea, Section, LocationCode) %>%
    distinct() %>%
    as_tibble()
  # Error if proportion > 1
  if (any(stations$SubProp > 1, na.rm = TRUE)) {
    stop("Substrate proportion > 1 in understory spawn data", call. = FALSE)
  }
  # Calculate substrate egg density
  eggsSub <- stations %>%
    full_join(y = algTrans, by = c(
      "Year", "LocationCode", "SpawnNumber", "Transect"
    )) %>%
    left_join(y = areasSm2, by = c("Region", "LocationCode")) %>%
    # Egg density in thousands (eggs x 10^3 / m^2; Haegele et al. 1979);
    # quadrat q
    mutate(EggDensSub = varphi * SubLyrs * SubProp) %>%
    replace_na(replace = list(EggDensSub = 0)) %>%
    select(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber, Transect,
      Station, Width, EggDensSub
    )
  # Error if proportion > 1
  if (any(algae$AlgProp > 1, na.rm = TRUE)) {
    stop("Algae proportion > 1 in understory spawn data", call. = FALSE)
  }
  # Calculate substrate egg density by quadrat/station
  eggsAlg <- algae %>%
    left_join(y = algCoefs, by = "AlgType") %>%
    left_join(y = areasSm2, by = "LocationCode") %>%
    left_join(
      y = select(.data = algTrans, -Width),
      by = c("Year", "Region", "LocationCode", "SpawnNumber", "Transect")
    ) %>%
    # Egg density in thousands (eggs * 10^3 / m^2; Schweigert 2005); quadrat
    # size coefficients not required because all quadrats are 0.5m^2 (1.0512)
    # Algae a
    mutate(EggDensAlg = vartheta * AlgLyrs^varrho * AlgProp^varsigma * Coef *
      1.0512) %>%
    group_by(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber, Transect,
      Station
    ) %>%
    # Quadrat q
    summarise(EggDensAlg = SumNA(EggDensAlg)) %>%
    replace_na(replace = list(EggDensAlg = 0)) %>%
    ungroup()
  # Combine eggs
  eggs <- eggsSub %>%
    full_join(y = eggsAlg, by = c(
      "Year", "Region", "StatArea", "Section", "LocationCode", "SpawnNumber",
      "Transect", "Station"
    )) %>%
    replace_na(replace = list(
      Width = 0, EggDensSub = 0, EggDensAlg = 0
    )) %>%
    mutate(EggDensSub = ifelse(Width > 0, EggDensSub, 0))
  # Calculate total egg density by station/quadrat
  eggsStation <- eggs %>%
    # Total egg density in thousands (eggs * 10^3 / m^2); quadrat q
    mutate(EggDens = EggDensSub + EggDensAlg) %>%
    filter(!is.na(Station))
  # Widths
  widths <- eggsStation %>%
    group_by(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber, Transect
    ) %>%
    summarise(Width = unique(Width)) %>%
    group_by(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber
    ) %>%
    # Spawn s
    summarise(
      WidthBar = MeanNA(Width)
    ) %>%
    ungroup()
  # Calculate transect-level metrics
  eggsTrans <- eggsStation %>%
    filter(Width > 0) %>%
    group_by(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber, Transect
    ) %>%
    # Transect t
    summarise(
      EggDens = MeanNA(EggDens),
      Width = unique(Width)
    ) %>%
    ungroup()
  # Calculate spawn number-level metrics
  eggsSpawn <- eggsTrans %>%
    left_join(
      y = spawn,
      by = c("Year", "LocationCode", "SpawnNumber")
    ) %>%
    mutate(LengthAlgae = ifelse(is.na(LengthAlgae), Length,
      LengthAlgae
    )) %>%
    filter(Method %in% c("Surface", "Dive")) %>%
    left_join(y = widths, by = c(
      "Year", "Region", "StatArea", "Section", "LocationCode", "SpawnNumber"
    )) %>%
    group_by(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber
    ) %>%
    # Spawn s
    summarise(
      WidthBar = unique(WidthBar),
      LengthAlgae = unique(LengthAlgae),
      EggDens = WtMeanNA(EggDens, w = Width)
    ) %>%
    ungroup()
  # Calculate understory biomass by spawn number
  biomassSpawn <- eggsSpawn %>%
    mutate(
      # Biomass in tonnes, based on Hay (1985), and Hay and Brett (1988); spawn
      # s
      UnderSI = EggDens * LengthAlgae * WidthBar * 1000 / theta
    ) %>%
    left_join(y = eggLyrs, by = c("Year", "LocationCode", "SpawnNumber"))
  # Calculate understory SI by spawn number
  SI <- biomassSpawn %>%
    select(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber, UnderSI
    )
  # Close the connection
  dbDisconnect(conn = accessDB)
  # Return the data
  return(list(
    stations = stations, algae = algae, eggs = eggs, eggsStation = eggsStation,
    eggsTrans = eggsTrans, eggsSpawn = eggsSpawn, biomassSpawn = biomassSpawn,
    SI = SI
  ))
} # End CalcUnderSpawn function
