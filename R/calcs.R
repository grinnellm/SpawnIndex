#' Convert number of eggs to spawning biomass.
#'
#' Calculate the conversion factor for the number of Pacific Herring eggs to the
#' spawn index (i.e., biomass) in tonnes.
#'
#' @param omega Numeric. The number of eggs per kilogram of female spawners;
#'   from \code{\link{pars}}. Message if < 0.
#' @param female Numeric. The proportion of spawners that are female; from
#'   \code{\link{pars}}. Message if < 0 and/or > 1.
#' @param quiet Logical. Suppress messages; default is FALSE.
#' @importFrom Rdpack reprompt
#' @importFrom stats na.omit
#' @return Numeric. The conversion factor for eggs to spawn index in tonnes
#'   (i.e., biomass). Divide the number of eggs by the conversion factor to get
#'   biomass. Message if < 0.
#' @references \insertAllCited
#' @seealso \code{\link{pars}}
#' @family calculation functions
#' @export
#' @examples
#' data(pars)
#' calc_egg_conversion()
calc_egg_conversion <- function(omega = pars$conversion$omega,
                                female = pars$conversion$female,
                                quiet = FALSE) {
  # Check omega: NA
  if (any(is.na(omega)) & !quiet) message("NA(s) in `omega`")
  # Check omega: numeric
  if (!is.numeric(omega)) stop("`omega` must be numeric.", call. = FALSE)
  # Check omega: range
  if (any(na.omit(omega) < 0) & !quiet) message("`omega` < 0.")
  # Check female: NA
  if (any(is.na(female)) & !quiet) message("NA(s) in `female`")
  # Check female: numeric
  if (!is.numeric(female)) stop("`female` must be numeric.", call. = FALSE)
  # Check female: range
  if ((any(na.omit(female) < 0) | any(na.omit(female) > 1)) & !quiet) {
    message("`female` < 0 and/or > 1.")
  }
  # Eggs per tonne: eggs/kilogram female * proportion female * kilograms/tonne
  theta <- omega * female * 1000
  # Check theta: NA
  if (any(is.na(theta)) & !quiet) message("NA(s) in `theta`")
  # Check theta: numeric
  if (any(!is.numeric(theta))) stop("`theta` is not numeric.", call. = FALSE)
  # Check theta: range
  if (any(na.omit(theta) < 0) & !quiet) message("`theta` < 0.")
  # Return the conversion factor
  theta
} # End calc_egg_conversion function

#' Calculate spawning biomass from spawn-on-kelp (SOK) harvest.
#'
#' Calculate spawning biomass in tonnes from spawn-on-kelp (SOK) harvest in
#' kilograms.
#'
#' @param sok Numeric. Weight of spawn-on-kelp (SOK) harvest in kilograms.
#'   Message if < 0.
#' @param nu Numeric. Proportion of SOK product that is kelp; from
#'   \code{\link{pars}}. Message if < 0 and/or > 1.
#' @param upsilon Numeric. SOK product weight increase due to brining as a
#'   proportion; from \code{\link{pars}}. Message if < 0 and/or > 1.
#' @param w Numeric. Average weight in kilograms of a fertilized egg; from
#'   \code{\link{pars}}. Message if < 0.
#' @param theta Numeric. Egg conversion factor (eggs to biomass); from
#'   \code{\link{calc_egg_conversion}}. Message if < 0.
#' @param quiet Logical. Suppress messages; default is FALSE.
#' @importFrom Rdpack reprompt
#' @importFrom stats na.omit
#' @return Numeric. Spawning biomass in tonnes. Message if < 0.
#' @references \insertAllCited
#' @seealso \code{\link{calc_egg_conversion}} \code{\link{pars}}
#' @family calculation functions
#' @export
#' @examples
#' data(pars)
#' calc_biomass_sok(sok = 100)
calc_biomass_sok <- function(sok,
                             nu = pars$sok$nu,
                             upsilon = pars$sok$upsilon,
                             w = pars$sok$w,
                             theta = calc_egg_conversion(),
                             quiet = FALSE) {
  # Check sok: NA
  if (any(is.na(sok)) & !quiet) message("NA(s) in `sok`")
  # Check sok: numeric
  if (!is.numeric(sok)) stop("`sok` must be numeric.", call. = FALSE)
  # Check sok: range
  if (any(na.omit(sok) < 0) & !quiet) message("`sok` < 0.")
  # Check nu: NA
  if (any(is.na(nu)) & !quiet) message("NA(s) in `nu`")
  # Check nu: numeric
  if (!is.numeric(nu)) stop("`nu` must be numeric.", call. = FALSE)
  # Check nu: range
  if ((any(na.omit(nu) < 0) | any(na.omit(nu) > 1)) & !quiet) {
    message("`nu` < 0 and/or > 1.")
  }
  # Check upsilon: NA
  if (any(is.na(upsilon)) & !quiet) message("NA(s) in `upsilon`")
  # Check upsilon: numeric
  if (!is.numeric(upsilon)) stop("`upsilon` must be numeric.", call. = FALSE)
  # Check upsilon: range
  if ((any(na.omit(upsilon) < 0) | any(na.omit(upsilon) > 1)) & !quiet) {
    message("`upsilon` < 0 and/or > 1.")
  }
  # Check w: NA
  if (any(is.na(w)) & !quiet) message("NA(s) in `w`")
  # Check w: numeric
  if (!is.numeric(w)) stop("`w` must be numeric.", call. = FALSE)
  # Check w: range
  if (any(na.omit(w) < 0) & !quiet) message("`w` < 0.")
  # Check theta: NA
  if (any(is.na(theta)) & !quiet) message("NA(s) in `theta`")
  # Check theta: numeric
  if (!is.numeric(theta)) stop("`theta` must be numeric.", call. = FALSE)
  # Check theta: range
  if (any(na.omit(theta) < 0) & !quiet) message("`theta` < 0.")
  # Spawning biomass in tonnes: (kg SOK * proportion eggs * proportion eggs) /
  # (kg per egg * eggs per tonne )
  SB <- (sok * (1 - nu) * 1 / (1 + upsilon)) / (w * theta)
  # Check SB: numeric
  if (!is.numeric(SB)) stop("`SB` is not numeric.", call. = FALSE)
  # Check SB: NA
  if (any(is.na(SB)) & !quiet) message("NA(s) in spawning biomass `SB`")
  # Check SB: range
  if (any(na.omit(SB) < 0) & !quiet) message("`SB` < 0.")
  # Return the spawning biomass
  SB
} # End calc_biomass_sok

#' Calculate the surface spawn index.
#'
#' Calculate the Pacific Herring surface spawn index in tonnes.
#'
#' @param where List. Location of the Pacific Herring surface spawn database
#'   (see examples).
#' @param a Tibble. Table of geographic information indicating the subset of
#'   spawn survey observations to include in calculations; from
#'   \code{\link{load_area_data}}.
#' @param widths List. List of three tables: median region, section, and pool
#'   widths in metres (m); from \code{\link{get_width}}.
#' @param yrs Numeric vector. Years(s) to include in the calculations. Message
#'   if < `pars$years$assess`.
#' @param intense Tibble. Table of spawn intensity categories and number of egg
#'   layers; from \code{\link{intensity}}.
#' @param intense_yrs Numeric vector. Years where intensity categories are used
#'   to determine egg layers. Message if >= `pars$years$layers`.
#' @param rescale_yrs Numeric vector. Years where intensity needs to be
#'   re-scaled from 5 to 9 categories. Message if >= `pars$years$assess`.
#' @param alpha Numeric. Regression intercept; from \code{\link{pars}}
#'   \insertCite{SchweigertEtal1997}{SpawnIndex}.
#' @param beta Numeric. Regression slope; from \code{\link{pars}}
#'   \insertCite{SchweigertEtal1997}{SpawnIndex}.
#' @param theta Numeric. Egg conversion factor (eggs to biomass); from
#'   \code{\link{calc_egg_conversion}}.
#' @param quiet Logical. Suppress messages; default is FALSE.
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
#' @note The `spawn index' is a relative index of spawning biomass.
#' @references \insertAllCited
#' @seealso \code{\link{HerringSpawn}} \code{\link{load_area_data}}
#'   \code{\link{get_width}} \code{\link{calc_egg_conversion}}
#'   \code{\link{pars}} \code{\link{intensity}}
#' @family calculation functions
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
#' width_bar <- get_width(where = width_loc, a = areas)
#' data(pars)
#' data(intensity)
#' surf_loc <- list(
#'   loc = db_loc, db = "HerringSpawn.mdb",
#'   fns = list(surface = "tSSSurface", all_spawn = "tSSAllspawn")
#' )
#' surf_spawn <- calc_surf_spawn(
#'   where = surf_loc, a = areas, widths = width_bar, yrs = 2010:2015
#' )
#' surf_spawn$SI
calc_surf_spawn <- function(where,
                            a,
                            widths,
                            yrs,
                            intense = intensity,
                            intense_yrs = pars$years$survey:pars$years$layers,
                            rescale_yrs = pars$years$assess:pars$years$assess,
                            alpha = pars$surface$alpha,
                            beta = pars$surface$beta,
                            theta = calc_egg_conversion(),
                            quiet = FALSE) {
  # Get where names
  where_names <- c("loc", "db", "fns.surface", "fns.all_spawn")
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
  # Check a: tibble
  if (!is_tibble(a)) {
    stop("`a` must be a tibble.", call. = FALSE)
  }
  # Check a: names
  if (!all(c(
    "SAR", "Region", "StatArea", "Section", "LocationCode", "Pool"
  ) %in% names(a))) {
    stop("`a` is missing columns", call. = FALSE)
  }
  # Check widths: list
  if (!is.list(widths)) stop("`widths` is not a list.", call. = FALSE)
  # Check widths: region tibble
  if (!is_tibble(widths$region)) {
    stop("`widths$region` is not a tibble.", call. = FALSE)
  }
  # Check widths: region rows
  if (nrow(widths$region) == 0 & !quiet) {
    message("`widths$region` has no data.", call. = FALSE)
  }
  # Check widths: region names
  if (!all(c("Region", "WidthReg") %in% names(widths$region))) {
    stop("`widths$region` is missing columns", call. = FALSE)
  }
  # Check widths: section tibble
  if (!is_tibble(widths$section)) {
    stop("`widths$section` is not a tibble.", call. = FALSE)
  }
  # Check widths: section rows
  if (nrow(widths$section) == 0 & !quiet) {
    message("`widths$section` has no data.", call. = FALSE)
  }
  # Check widths: section names
  if (!all(c("Region", "Section", "WidthSec") %in% names(widths$section))) {
    stop("`widths$section` is missing columns", call. = FALSE)
  }
  # Check widths: pool tibble
  if (!is_tibble(widths$pool)) {
    stop("`widths$pool` is not a tibble.", call. = FALSE)
  }
  # Check widths: pool rows
  if (nrow(widths$pool) == 0 & !quiet) {
    message("`widths$pool` has no data.", call. = FALSE)
  }
  # Check widths: pool names
  if (!all(c("Region", "Section", "Pool", "WidthPool") %in%
    names(widths$pool))) {
    stop("`widths$pool` is missing columns", call. = FALSE)
  }
  # Check yrs: numeric
  if (!is.numeric(yrs)) stop("`yrs` must be numeric", call. = FALSE)
  # Check yrs: range
  if (any(yrs < pars$years$assess) & !quiet) {
    message("`yrs` < ", pars$years$assess, ".")
  }
  # Check intense: tibble
  if (!is_tibble(intense)) stop("`intense` must be a tibble.", call. = FALSE)
  # Check intense: names
  if (!all(c("Intensity", "Description", "Layers") %in% names(intense))) {
    stop("`intense` is missing columns", call. = FALSE)
  }
  # Check intense_yrs: numeric
  if (!is.numeric(intense_yrs)) {
    stop("`intense_yrs` must be numeric", call. = FALSE)
  }
  # Check intense_yrs: range
  if (any(intense_yrs >= pars$years$layers) & !quiet) {
    message("`intense_yrs` >= ", pars$years$layers, ".")
  }
  # Check rescale_yrs: numeric
  if (!is.numeric(rescale_yrs)) {
    stop("`rescale_yrs` must be numeric", call. = FALSE)
  }
  # Check rescale_yrs: range
  if (any(rescale_yrs >= pars$years$assess) & !quiet) {
    message("`rescale_yrs` >= ", pars$years$assess, ".")
  }
  # Check alpha: NA
  if (any(is.na(alpha)) & !quiet) message("NA(s) in `alpha`")
  # Check alpha: numeric
  if (!is.numeric(alpha)) stop("`alpha` must be numeric.", call. = FALSE)
  # Check beta: NA
  if (any(is.na(beta)) & !quiet) message("NA(s) in `beta`")
  # Check beta: numeric
  if (!is.numeric(beta)) stop("`beta` must be numeric.", call. = FALSE)
  # Check theta: NA
  if (any(is.na(theta)) & !quiet) message("NA(s) in `theta`")
  # Check theta: numeric
  if (!is.numeric(theta)) stop("`theta` must be numeric.", call. = FALSE)
  # Establish connection with access
  access_db <- dbConnect(
    drv = odbc(),
    .connection_string = paste(
      "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=",
      file.path(where$loc, where$db),
      sep = ""
    )
  )
  # Get a small subset of area data
  areas_sm <- a %>%
    select(SAR, Region, StatArea, Section, LocationCode, Pool) %>%
    distinct() %>%
    as_tibble()
  # Load all spawn
  spawn <- dbReadTable(conn = access_db, name = where$fns$all_spawn) %>%
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
  surface <- dbReadTable(conn = access_db, name = where$fns$surface) %>%
    rename(LocationCode = Loc_Code, SpawnNumber = Spawn_Number) %>%
    filter(Year %in% yrs, LocationCode %in% a$LocationCode) %>%
    left_join(y = areas_sm, by = "LocationCode") %>%
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
  p_cover <- surface %>%
    select(ends_with("Percent"))
  # Error if any percents are greater than 100
  if (any(p_cover > 100, na.rm = TRUE)) {
    stop("Percent cover > 100 in surface spawn data.", call. = FALSE)
  }
  # Continue with calculating egg layers
  surface <- surface %>%
    # Sample j
    mutate(
      EggLyrs = Grass + Rockweed + Kelp + BrownAlgae + LeafyRed +
        StringyRed + Rock + Other,
      Intensity = ifelse(Year %in% rescale_yrs & Intensity > 0,
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
      EggLyrs = ifelse(Year %in% intense_yrs, Layers, EggLyrs),
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
  no_layers <- eggs %>% filter(EggLyrs == 0) # %>%
  # Error if there are missing values
  if (nrow(no_layers) > 0) {
    stop("Missing egg layers for ", nrow(no_layers), " record(s):",
      print(no_layers), ".",
      sep = "", call. = FALSE
    )
  }
  # Output egg layer info
  egg_lyrs <- eggs %>%
    group_by(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber
    ) %>%
    summarise(SurfLyrs = MeanNA(EggLyrs)) %>%
    ungroup()
  # Calculate egg density per spawn number/pool
  eggs_spawn <- eggs %>%
    group_by(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber, Pool
    ) %>%
    # Spawn s
    summarise(EggDens = MeanNA(EggDens)) %>%
    ungroup()
  # Calculate annual fish biomass by spawn number/pool
  biomass_spawn <- eggs_spawn %>%
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
      y = egg_lyrs,
      by = c(
        "Year", "Region", "StatArea", "Section", "LocationCode", "SpawnNumber"
      )
    )
  # Calculate annual SI by spawn number
  SI <- biomass_spawn %>%
    select(Year, Region, StatArea, Section, LocationCode, SpawnNumber, SurfSI)
  # Close the connection
  dbDisconnect(conn = access_db)
  # Assemble into a list
  res <- list(
    surface = surface, eggs = eggs, eggs_spawn = eggs_spawn,
    biomass_spawn = biomass_spawn, SI = SI
  )
  # Check output: rows
  if (nrow(res$SI) == 0) stop("`res$SI` has no data.", call. = FALSE)
  # Check output: tibble
  if (!is_tibble(res$SI)) stop("`res$SI` is not a tibble.", call. = FALSE)
  # Check output: names
  if (!all(c(
    "Year", "Region", "StatArea", "Section", "LocationCode", "SpawnNumber",
    "SurfSI"
  ) %in% names(res$SI))) {
    stop("`res$SI` is missing columns", call. = FALSE)
  }
  # Return the list
  res
} # End calc_surf_spawn function

#' Calculate the Macrocystis spawn index.
#'
#' Calculate the Pacific Herring Macrocystis spawn index in tonnes.
#'
#' @param where List. Location of the Pacific Herring Macrocystis spawn database
#'   (see examples).
#' @param a Tibble. Table of geographic information indicating the subset of
#'   spawn survey observations to include in calculations; from
#'   \code{\link{load_area_data}}.
#' @param yrs Numeric vector. Years(s) to include in the calculations, usually
#'   staring in `pars$years$assess`.
#' @param t_swath Numeric. Transect swath (i.e., width) in metres.
#' @param xi Numeric. Regression slope; from \code{\link{pars}}
#'   \insertCite{HaegeleSchweigert1990}{SpawnIndex}.
#' @param gamma Numeric. Regression exponent on egg layers; from
#'   \code{\link{pars}} \insertCite{HaegeleSchweigert1990}{SpawnIndex}.
#' @param delta Numeric. Regression exponent on plant height; from
#'   \code{\link{pars}} \insertCite{HaegeleSchweigert1990}{SpawnIndex}.
#' @param epsilon Numeric. Regression exponent on number of stalks per plant;
#'   from \code{\link{pars}} \insertCite{HaegeleSchweigert1990}{SpawnIndex}.
#' @param theta Numeric. Egg conversion factor (eggs to biomass); from
#'   \code{\link{calc_egg_conversion}}.
#' @param quiet Logical. Suppress messages; default is FALSE.
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
#' @note The `spawn index' is a relative index of spawning biomass.
#' @references \insertAllCited
#' @seealso \code{\link{HerringSpawn}} \code{\link{load_area_data}}
#'   \code{\link{calc_egg_conversion}} \code{\link{pars}}
#' @family calculation functions
#' @export
#' @examples
#' db_loc <- system.file("extdata", package = "SpawnIndex")
#' area_loc <- list(
#'   loc = db_loc, db = "HerringSpawn.mdb",
#'   fns = list(sections = "Sections", locations = "Location")
#' )
#' areas <- load_area_data(reg = "WCVI", where = area_loc)
#' data(pars)
#' macro_loc <- list(
#'   loc = db_loc, db = "HerringSpawn.mdb",
#'   fns = list(
#'     all_spawn = "tSSAllspawn", plants = "tSSMacPlant",
#'     transects = "tSSMacTrans"
#'   )
#' )
#' macro_spawn <- calc_macro_spawn(
#'   where = macro_loc, a = areas, yrs = 2010:2015
#' )
#' macro_spawn$SI
calc_macro_spawn <- function(where,
                             a,
                             yrs,
                             t_swath = 2,
                             xi = pars$macrocystis$xi,
                             gamma = pars$macrocystis$gamma,
                             delta = pars$macrocystis$delta,
                             epsilon = pars$macrocystis$epsilon,
                             theta = calc_egg_conversion(),
                             quiet = FALSE) {

  # Get where names
  where_names <- c("loc", "db", "fns.all_spawn", "fns.plants", "fns.transects")
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
  # Check a: tibble
  if (!is_tibble(a)) {
    stop("`a` must be a tibble.", call. = FALSE)
  }
  # Check a: names
  if (!all(c(
    "SAR", "Region", "StatArea", "Section", "LocationCode", "Pool"
  ) %in% names(a))) {
    stop("`a` is missing columns", call. = FALSE)
  }
  # Check yrs: numeric
  if (!is.numeric(yrs)) stop("`yrs` must be numeric", call. = FALSE)
  # Check yrs: range
  if (any(yrs < pars$years$assess) & !quiet) {
    message("`yrs` < ", pars$years$assess, ".")
  }
  # Check t_swath: NA
  if (any(is.na(t_swath)) & !quiet) message("NA(s) in `t_swath`")
  # Check t_swath: numeric
  if (!is.numeric(t_swath)) stop("`t_swath` must be numeric.", call. = FALSE)
  # Check t_swath: value
  if (t_swath != 2 & !quiet) message("`t_swath` != 2")
  # Check xi: NA
  if (any(is.na(xi)) & !quiet) message("NA(s) in `xi`")
  # Check xi: numeric
  if (!is.numeric(xi)) stop("`xi` must be numeric.", call. = FALSE)
  # Check gamma: NA
  if (any(is.na(gamma)) & !quiet) message("NA(s) in `gamma`")
  # Check gamma: numeric
  if (!is.numeric(gamma)) stop("`gamma` must be numeric.", call. = FALSE)
  # Check delta: NA
  if (any(is.na(delta)) & !quiet) message("NA(s) in `delta`")
  # Check delta: numeric
  if (!is.numeric(delta)) stop("`delta` must be numeric.", call. = FALSE)
  # Check epsilon: NA
  if (any(is.na(epsilon)) & !quiet) message("NA(s) in `epsilon`")
  # Check epsilon: numeric
  if (!is.numeric(epsilon)) stop("`epsilon` must be numeric.", call. = FALSE)
  # Check theta: NA
  if (any(is.na(theta)) & !quiet) message("NA(s) in `theta`")
  # Check theta: numeric
  if (!is.numeric(theta)) stop("`theta` must be numeric.", call. = FALSE)
  # Establish connection with access
  access_db <- dbConnect(
    drv = odbc(),
    .connection_string = paste(
      "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=",
      file.path(where$loc, where$db),
      sep = ""
    )
  )
  # Load all spawn
  spawn <- dbReadTable(conn = access_db, name = where$fns$all_spawn) %>%
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
  plants <- dbReadTable(conn = access_db, name = where$fns$plants) %>%
    rename(LocationCode = Loc_Code, SpawnNumber = Spawn_Number) %>%
    filter(
      Year %in% yrs, LocationCode %in% a$LocationCode,
      !is.na(Mature)
    ) %>%
    select(Year, LocationCode, SpawnNumber, Transect, Mature) %>%
    as_tibble()
  # Get a small subset of area data
  areas_sm <- a %>%
    select(Region, StatArea, Section, LocationCode, Pool) %>%
    distinct() %>%
    as_tibble()
  # Get transect-level data
  transects <- dbReadTable(conn = access_db, name = where$fns$transects) %>%
    rename(LocationCode = Loc_Code, SpawnNumber = Spawn_Number) %>%
    filter(Year %in% yrs, LocationCode %in% a$LocationCode) %>%
    left_join(y = areas_sm, by = "LocationCode") %>%
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
    mutate(Swath = t_swath)
  # Calculate transect-level data
  dat_trans <- dat %>%
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
  biomass_spawn <- dat_trans %>%
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
  SI <- biomass_spawn %>%
    select(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber, MacroSI
    )
  # Close the connection
  dbDisconnect(conn = access_db)
  # Assemble into a list
  res <- list(
    dat = dat, dat_trans = dat_trans, biomass_spawn = biomass_spawn, SI = SI
  )
  # Check output: rows
  if (nrow(res$SI) == 0) stop("`res$SI` has no data.", call. = FALSE)
  # Check output: tibble
  if (!is_tibble(res$SI)) stop("`res$SI` is not a tibble.", call. = FALSE)
  # Check output: names
  if (!all(c(
    "Year", "Region", "StatArea", "Section", "LocationCode", "SpawnNumber",
    "MacroSI"
  ) %in% names(res$SI))) {
    stop("`res$SI` is missing columns", call. = FALSE)
  }
  # Return the list
  res
} # End calc_macro_spawn function

#' Calculate the understory spawn index.
#'
#' Calculate the Pacific Herring understory spawn index in tonnes.
#'
#' @param where List. Location of the Pacific Herring understory spawn database
#'   (see examples).
#' @param a Tibble. Table of geographic information indicating the subset of
#'   spawn survey observations to include in calculations; from
#'   \code{\link{load_area_data}}.
#' @param yrs Numeric vector. Years(s) to include in the calculations, usually
#'   staring in `pars$years$assess`.
#' @param alg_coefs Tibble. Table of algae coefficients; from
#'   \code{\link{algae_coefs}}.
#' @param tau Tibble. Table of understory spawn width adjustment factors from
#'   \code{\link{under_width_facs}}.
#' @param varphi Numeric. Regression slope for substrate; from
#'   \code{\link{pars}} \insertCite{HaegeleEtal1979}{SpawnIndex}.
#' @param vartheta Numeric. Regression slope for algae; from \code{\link{pars}}
#'   \insertCite{Schweigert2005}{SpawnIndex}.
#' @param varrho Numeric. Regression exponent on number of egg layers; from
#'   \code{\link{pars}} \insertCite{Schweigert2005}{SpawnIndex}.
#' @param varsigma Numeric. Regression exponent on proportion of algae; from
#'   \code{\link{pars}} \insertCite{Schweigert2005}{SpawnIndex}.
#' @param theta Numeric. Egg conversion factor (eggs to biomass); from
#'   \code{\link{calc_egg_conversion}}.
#' @param quiet Logical. Suppress messages; default is FALSE.
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
#' @note The `spawn index' is a relative index of spawning biomass.
#' @references \insertAllCited
#' @seealso \code{\link{HerringSpawn}} \code{\link{load_area_data}}
#'   \code{\link{calc_egg_conversion}} \code{\link{pars}}
#'   \code{\link{algae_coefs}}
#' @family calculation functions
#' @export
#' @examples
#' db_loc <- system.file("extdata", package = "SpawnIndex")
#' area_loc <- list(
#'   loc = db_loc, db = "HerringSpawn.mdb",
#'   fns = list(sections = "Sections", locations = "Location")
#' )
#' areas <- load_area_data(reg = "WCVI", where = area_loc)
#' under_loc <- list(
#'   loc = db_loc, db = "HerringSpawn.mdb",
#'   fns = list(
#'     all_spawn = "tSSAllspawn", alg_trans = "tSSVegTrans",
#'     stations = "tSSStations", algae = "tSSVegetation"
#'   )
#' )
#' data(under_width_facs)
#' data(pars)
#' data(algae_coefs)
#' under_spawn <- calc_under_spawn(
#'   where = under_loc, a = areas, yrs = 2010:2015
#' )
#' under_spawn$SI
calc_under_spawn <- function(where,
                             a,
                             yrs,
                             alg_coefs = algae_coefs,
                             tau = under_width_facs,
                             varphi = pars$understory$varphi,
                             vartheta = pars$understory$vartheta,
                             varrho = pars$under$varrho,
                             varsigma = pars$understory$varsigma,
                             theta = calc_egg_conversion(),
                             quiet = FALSE) {

  # Get where names
  where_names <- c(
    "loc", "db", "fns.all_spawn", "fns.alg_trans", "fns.stations", "fns.algae"
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
  # Check a: tibble
  if (!is_tibble(a)) {
    stop("`a` must be a tibble.", call. = FALSE)
  }
  # Check a: names
  if (!all(c(
    "SAR", "Region", "StatArea", "Section", "LocationCode", "Pool"
  ) %in% names(a))) {
    stop("`a` is missing columns", call. = FALSE)
  }
  # Check yrs: numeric
  if (!is.numeric(yrs)) stop("`yrs` must be numeric", call. = FALSE)
  # Check yrs: range
  if (any(yrs < pars$years$assess) & !quiet) {
    message("`yrs` < ", pars$years$assess, ".")
  }
  # Check alg_coefs: tibble
  if (!is_tibble(alg_coefs)) {
    stop("`alg_coefs` must be a tibble.", call. = FALSE)
  }
  # Check alg_coefs: names
  if (!all(c("AlgaeName", "AlgType", "Coef") %in% names(alg_coefs))) {
    stop("`alg_coefs` is missing columns", call. = FALSE)
  }
  # Check tau: tibble
  if (!is_tibble(tau)) stop("`tau` must be a tibble.", call. = FALSE)
  # Check tau: names
  if (!all(c("Year", "HG", "PRD", "CC", "SoG", "WCVI", "A27", "A2W")
  %in% names(tau))) {
    stop("`tau` is missing columns", call. = FALSE)
  }
  # Check varphi: NA
  if (any(is.na(varphi)) & !quiet) message("NA(s) in `varphi`")
  # Check varphi: numeric
  if (!is.numeric(varphi)) stop("`varphi` must be numeric.", call. = FALSE)
  # Check vartheta: NA
  if (any(is.na(vartheta)) & !quiet) message("NA(s) in `vartheta`")
  # Check vartheta: numeric
  if (!is.numeric(vartheta)) stop("`vartheta` must be numeric.", call. = FALSE)
  # Check varrho: NA
  if (any(is.na(varrho)) & !quiet) message("NA(s) in `varrho`")
  # Check varrho: numeric
  if (!is.numeric(varrho)) stop("`varrho` must be numeric.", call. = FALSE)
  # Check varsigma: NA
  if (any(is.na(varsigma)) & !quiet) message("NA(s) in `varsigma`")
  # Check varsigma: numeric
  if (!is.numeric(varsigma)) stop("`varsigma` must be numeric.", call. = FALSE)
  # Check theta: NA
  if (any(is.na(theta)) & !quiet) message("NA(s) in `theta`")
  # Check theta: numeric
  if (!is.numeric(theta)) stop("`theta` must be numeric.", call. = FALSE)
  # Establish connection with access
  access_db <- dbConnect(
    drv = odbc(),
    .connection_string = paste(
      "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=",
      file.path(where$loc, where$db),
      sep = ""
    )
  )
  # Get a small subset of area data
  areas_sm1 <- a %>%
    select(Region, LocationCode) %>%
    distinct() %>%
    as_tibble()
  # Load all spawn
  spawn <- dbReadTable(conn = access_db, name = where$fns$all_spawn) %>%
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
  alg_trans <- dbReadTable(conn = access_db, name = where$fns$alg_trans) %>%
    rename(
      LocationCode = Loc_Code, SpawnNumber = Spawn_Number,
      QuadratSize = Quadrat_Size, WidthObs = Width_Recorded
    ) %>%
    filter(Year %in% yrs, LocationCode %in% a$LocationCode) %>%
    select(
      Year, LocationCode, SpawnNumber, Transect, WidthObs, QuadratSize
    ) %>%
    left_join(y = areas_sm1, by = "LocationCode") %>%
    as_tibble()
  # Correction factors for region(s) by year (to fix lead line shrinkage issue)
  width_facs <- tau %>%
    gather(key = Region, value = WidthFac, -Year)
  # Merge the width factors and correct transect widths
  alg_trans <- alg_trans %>%
    left_join(y = width_facs, by = c("Year", "Region")) %>%
    replace_na(replace = list(WidthFac = 1.0)) %>%
    mutate(Width = WidthObs * WidthFac)
  # Error if any quadrats are not 0.5 m^2
  if (any(alg_trans$QuadratSize != 0.5)) {
    stop("All quadrats must be 0.5m^2.", call. = FALSE)
  }
  # Load station data
  stations <- dbReadTable(conn = access_db, name = where$fns$stations) %>%
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
  egg_lyrs_sub <- stations %>%
    group_by(Year, LocationCode, SpawnNumber, Transect) %>%
    summarise(Layers = MeanNA(SubLyrs)) %>%
    ungroup() %>%
    mutate(Source = "Substrate")
  # Load algae
  algae <- dbReadTable(conn = access_db, name = where$fns$algae) %>%
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
  egg_lyrs_alg <- algae %>%
    group_by(Year, LocationCode, SpawnNumber, Transect) %>%
    summarise(Layers = MeanNA(AlgLyrs)) %>%
    ungroup() %>%
    mutate(Source = "Algae")
  # Combine egg layer info
  egg_lyrs <- bind_rows(egg_lyrs_sub, egg_lyrs_alg) %>%
    group_by(Year, LocationCode, SpawnNumber, Transect) %>%
    summarise(Layers = MeanNA(Layers)) %>%
    group_by(Year, LocationCode, SpawnNumber) %>%
    summarise(UnderLyrs = MeanNA(Layers)) %>%
    ungroup()
  # If there are missing algae types
  if (any(!algae$AlgType %in% alg_coefs$AlgType)) {
    # Get missing algae type(s)
    miss_alg <- unique(algae$AlgType[!algae$AlgType %in%
      alg_coefs$AlgType])
    # Error, and show missing type(s)
    stop("Missing algae type(s): ", paste(miss_alg, collapse = ", "), ".",
      call. = FALSE
    )
  } # End if there are missing algae types
  # Get a small subset of area data
  areas_sm_2 <- a %>%
    select(Region, StatArea, Section, LocationCode) %>%
    distinct() %>%
    as_tibble()
  # Error if proportion > 1
  if (any(stations$SubProp > 1, na.rm = TRUE)) {
    stop("Substrate proportion > 1 in understory spawn data.", call. = FALSE)
  }
  # Calculate substrate egg density
  eggs_sub <- stations %>%
    full_join(y = alg_trans, by = c(
      "Year", "LocationCode", "SpawnNumber", "Transect"
    )) %>%
    left_join(y = areas_sm_2, by = c("Region", "LocationCode")) %>%
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
    stop("Algae proportion > 1 in understory spawn data.", call. = FALSE)
  }
  # Calculate substrate egg density by quadrat/station
  eggs_alg <- algae %>%
    left_join(y = alg_coefs, by = "AlgType") %>%
    left_join(y = areas_sm_2, by = "LocationCode") %>%
    left_join(
      y = select(.data = alg_trans, -Width),
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
  eggs <- eggs_sub %>%
    full_join(y = eggs_alg, by = c(
      "Year", "Region", "StatArea", "Section", "LocationCode", "SpawnNumber",
      "Transect", "Station"
    )) %>%
    replace_na(replace = list(
      Width = 0, EggDensSub = 0, EggDensAlg = 0
    )) %>%
    mutate(EggDensSub = ifelse(Width > 0, EggDensSub, 0))
  # Calculate total egg density by station/quadrat
  eggs_station <- eggs %>%
    # Total egg density in thousands (eggs * 10^3 / m^2); quadrat q
    mutate(EggDens = EggDensSub + EggDensAlg) %>%
    filter(!is.na(Station))
  # Widths
  widths <- eggs_station %>%
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
  eggs_trans <- eggs_station %>%
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
  eggs_spawn <- eggs_trans %>%
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
  biomass_spawn <- eggs_spawn %>%
    mutate(
      # Biomass in tonnes, based on Hay (1985), and Hay and Brett (1988); spawn
      # s
      UnderSI = EggDens * LengthAlgae * WidthBar * 1000 / theta
    ) %>%
    left_join(y = egg_lyrs, by = c("Year", "LocationCode", "SpawnNumber"))
  # Calculate understory SI by spawn number
  SI <- biomass_spawn %>%
    select(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber, UnderSI
    )
  # Close the connection
  dbDisconnect(conn = access_db)
  # Assemble into a list
  res <- list(
    stations = stations, algae = algae, eggs = eggs,
    eggs_station = eggs_station, eggs_trans = eggs_trans,
    eggs_spawn = eggs_spawn, biomass_spawn = biomass_spawn, SI = SI
  )
  # Check output: rows
  if (nrow(res$SI) == 0) stop("`res$SI` has no data.", call. = FALSE)
  # Check output: tibble
  if (!is_tibble(res$SI)) stop("`res$SI` is not a tibble.", call. = FALSE)
  # Check output: names
  if (!all(c(
    "Year", "Region", "StatArea", "Section", "LocationCode", "SpawnNumber",
    "UnderSI"
  ) %in% names(res$SI))) {
    stop("`res$SI` is missing columns", call. = FALSE)
  }
  # Return the list
  res
} # End calc_under_spawn function
