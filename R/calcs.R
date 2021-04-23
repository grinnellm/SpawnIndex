#' Convert number of eggs to spawning biomass.
#'
#' Calculate the conversion factor for the number of Pacific Herring eggs to
#' spawning biomass (i.e., the spawn index) in tonnes.
#'
#' @param omega Numeric. The number of eggs per kilogram of female spawners;
#'   from \code{\link{pars}}. Message if < 0.
#' @param female Numeric. The proportion of spawners that are female; from
#'   \code{\link{pars}}. Message if < 0 and/or > 1.
#' @template param-quiet
#' @importFrom Rdpack reprompt
#' @importFrom stats na.omit
#' @return Numeric. The conversion factor for eggs to spawn index in tonnes
#'   (i.e., biomass). Divide the number of eggs by the conversion factor to get
#'   biomass. Message if < 0.
#' @seealso \code{\link{pars}}
#' @family calculation functions
#' @export
#' @examples
#' data(pars)
#' eggs_to_sb()
eggs_to_sb <- function(omega = pars$conversion$omega,
                       female = pars$conversion$female,
                       quiet = FALSE) {
  # Check input: NA and numeric
  check_numeric(dat = list(omega = omega, female = female), quiet = quiet)
  # Check omega: range
  if (any(na.omit(omega) < 0) & !quiet) message("`omega` < 0.")
  # Check female: range
  if ((any(na.omit(female) < 0) | any(na.omit(female) > 1)) & !quiet) {
    message("`female` < 0 and/or > 1.")
  }
  # Eggs per tonne: eggs/kilogram female * proportion female * kilograms/tonne
  theta <- omega * female * 1000
  # Check output: NA and numeric
  check_numeric(dat = list(theta = theta), quiet = quiet)
  # Check theta: range
  if (any(na.omit(theta) < 0) & !quiet) message("`theta` < 0.")
  # Return the conversion factor
  theta
} # End eggs_to_sb function

#' Calculate the spawn index from spawn-on-kelp (SOK) harvest.
#'
#' Calculate the spawn index in tonnes from spawn-on-kelp (SOK) harvest in
#' kilograms.
#'
#' @param sok Numeric. Weight of spawn-on-kelp (SOK) harvest in kilograms.
#'   Message if < 0.
#' @param nu Numeric. Proportion of SOK product that is kelp; from
#'   \code{\link{pars}}. Message if < 0 and/or > 1.
#' @param upsilon Numeric. SOK product weight increase due to brining as a
#'   proportion; from \code{\link{pars}}. Message if < 0 and/or > 1.
#' @param egg_weight Numeric. Average weight in kilograms of a fertilized egg;
#' from \code{\link{pars}}. Message if < 0.
#' @template param-theta
#' @template param-quiet
#' @importFrom Rdpack reprompt
#' @importFrom stats na.omit
#' @return Numeric. Spawn index in tonnes. Message if < 0.
#' @seealso \code{\link{eggs_to_sb}} \code{\link{pars}}
#' @family calculation functions
#' @export
#' @examples
#' data(pars)
#' calc_sok_index(sok = 100)
calc_sok_index <- function(sok,
                           nu = pars$sok$nu,
                           upsilon = pars$sok$upsilon,
                           egg_weight = pars$sok$egg_weight,
                           theta = eggs_to_sb(),
                           quiet = FALSE) {
  # Check input: NA and numeric
  check_numeric(
    dat = list(
      sok = sok, nu = nu, upsilon = upsilon, egg_weight = egg_weight,
      theta = theta
    ),
    quiet = quiet
  )
  # Check sok: range
  if (any(na.omit(sok) < 0) & !quiet) message("`sok` < 0.")
  # Check nu: range
  if ((any(na.omit(nu) < 0) | any(na.omit(nu) > 1)) & !quiet) {
    message("`nu` < 0 and/or > 1.")
  }
  # Check upsilon: range
  if ((any(na.omit(upsilon) < 0) | any(na.omit(upsilon) > 1)) & !quiet) {
    message("`upsilon` < 0 and/or > 1.")
  }
  # Check egg_weight: range
  if (any(na.omit(egg_weight) < 0) & !quiet) message("`egg_weight` < 0.")
  # Check theta: range
  if (any(na.omit(theta) < 0) & !quiet) message("`theta` < 0.")
  # Spawning biomass in tonnes: (kg SOK * proportion eggs * proportion eggs) /
  # (kg per egg * eggs per tonne )
  sb <- (sok * (1 - nu) * 1 / (1 + upsilon)) / (egg_weight * theta)
  # Check output: NA and numeric
  check_numeric(dat = list(sb = sb), quiet = quiet)
  # Check sb: range
  if (any(na.omit(sb) < 0) & !quiet) message("`sb` < 0.")
  # Return the spawning biomass
  sb
} # End calc_sok_index

#' Calculate surface spawn egg density.
#'
#' Calculate Pacific Herring surface spawn egg density in thousands of eggs per
#' square metre (10^3 * eggs / m^2). This function implements a linear
#' regression model to estimate egg density; implemented in
#' \code{\link{calc_surf_index}} \insertCite{SchweigertEtal1997}{SpawnIndex}.
#'
#' @param alpha Numeric. Regression intercept; from \code{\link{pars}}
#'   \insertCite{SchweigertEtal1997}{SpawnIndex}.
#' @param beta Numeric. Regression slope; from \code{\link{pars}}
#'   \insertCite{SchweigertEtal1997}{SpawnIndex}.
#' @template param-egg_layers
#' @template param-quiet
#' @importFrom Rdpack reprompt
#' @importFrom stats na.omit
#' @return Numeric. Egg density in thousands of eggs per square metre. Message
#'   if <= 0.
#' @note Density is set to 0 when egg layers is 0 (otherwise it would be
#'   \code{alpha}). Also, there is an error in
#'   \insertCite{SchweigertEtal1997;textual}{SpawnIndex}; surface egg density is
#'   in thousands of eggs per square metre.
#' @references \insertAllCited
#' @seealso \code{\link{pars}} \code{\link{calc_surf_index}}
#' @family calculation functions
#' @export
#' @examples
#' data(pars)
#' dens_surf(egg_layers = 4)
dens_surf <- function(alpha = pars$surface$alpha,
                      beta = pars$surface$beta,
                      egg_layers,
                      quiet = FALSE) {
  # Check input: NA and numeric
  check_numeric(
    dat = list(alpha = alpha, beta = beta, egg_layers = egg_layers),
    quiet = quiet
  )
  # Check egg_layers: range
  if (any(na.omit(egg_layers) < 0) & !quiet) message("`egg_layers` < 0.")
  # Egg density in thousands (10^3 * eggs / m^2; Schweigert et al. 1997)
  density <- alpha + beta * egg_layers
  # Ensure density is zero (not `alpha`) when egg_layers is zero
  density[egg_layers == 0] <- 0
  # Check output: NA and numeric
  check_numeric(dat = list(density = density), quiet = quiet)
  # Check density: range
  if (any(na.omit(density) < 0) & !quiet) message("`density` < 0.")
  # Return density
  density
} # End dens_surf function

#' Calculate the surface spawn index.
#'
#' Calculate the Pacific Herring surface spawn index in tonnes
#' \insertCite{SchweigertEtal1997}{SpawnIndex}. This function primarily wrangles
#' and prepares the data; the actual calculation is done by
#' \code{\link{dens_surf}}.
#'
#' @template param-where
#' @template param-areas
#' @param widths List. List of three tables: median region, section, and pool
#'   widths in metres (m); from \code{\link{load_width}}.
#' @template param-years
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
#' @template param-theta
#' @template param-quiet
#' @importFrom odbc dbConnect odbc dbDisconnect
#' @importFrom DBI dbReadTable
#' @importFrom dplyr select distinct rename left_join filter %>% ends_with
#'   ungroup mutate_at vars starts_with ends_with
#' @importFrom tibble as_tibble
#' @importFrom stringr str_to_title
#' @importFrom gfiscamutils mean_na sum_na
#' @importFrom tidyr replace_na
#' @importFrom Rdpack reprompt
#' @return List. The element \code{si} is a tibble with surface spawn index
#'   (\code{SurfSI}) in tonnes by spawn number and year. The spawn number is the
#'   finest spatial scale at which we calculate the spawn index. Other
#'   information in this tibble comes from \code{a}: Region, Statistical Area,
#'   Section, and Location code.
#' @note The `spawn index' is a relative index of spawning biomass.
#' @references \insertAllCited
#' @seealso \code{\link{HerringSpawn}} \code{\link{load_area_data}}
#'   \code{\link{load_width}} \code{\link{eggs_to_sb}}
#'   \code{\link{pars}} \code{\link{intensity}} \code{\link{dens_surf}}
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
#' width_bar <- load_width(where = width_loc, a = areas)
#' data(pars)
#' data(intensity)
#' surf_loc <- list(
#'   loc = db_loc, db = "HerringSpawn.mdb",
#'   fns = list(surface = "tSSSurface", all_spawn = "tSSAllspawn")
#' )
#' surf_spawn <- calc_surf_index(
#'   where = surf_loc, areas = areas, widths = width_bar, years = 2010:2015
#' )
#' surf_spawn$si
calc_surf_index <- function(where,
                            areas,
                            widths,
                            years,
                            intense = intensity,
                            intense_yrs =
                              pars$years$survey:(pars$years$layers - 1),
                            rescale_yrs =
                              pars$years$survey:(pars$years$assess - 1),
                            alpha = pars$surface$alpha,
                            beta = pars$surface$beta,
                            theta = eggs_to_sb(),
                            quiet = FALSE) {
  # Check input: NA and numeric
  check_numeric(
    dat = list(
      years = years, intense_yrs = intense_yrs, rescale_yrs = rescale_yrs,
      theta = theta
    ),
    quiet = quiet
  )
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
  # Check input: tibble rows
  check_tibble(dat = list(
    areas = areas, region = widths$region, section = widths$section,
    pool = widths$pool, intense = intense
  ), quiet = quiet)
  # Check areas: names
  if (!all(c(
    "SAR", "Region", "StatArea", "Section", "LocationCode", "Pool"
  ) %in% names(areas))) {
    stop("`areas` is missing columns", call. = FALSE)
  }
  # Check widths: region names
  if (!all(c("Region", "WidthReg") %in% names(widths$region))) {
    stop("`widths$region` is missing columns", call. = FALSE)
  }
  # Check widths: section names
  if (!all(c("Region", "Section", "WidthSec") %in% names(widths$section))) {
    stop("`widths$section` is missing columns", call. = FALSE)
  }
  # Check widths: pool names
  if (!all(c("Region", "Section", "Pool", "WidthPool") %in%
    names(widths$pool))) {
    stop("`widths$pool` is missing columns", call. = FALSE)
  }
  # Check years: range
  if (any(years < pars$years$assess) & !quiet) {
    message("`years` < ", pars$years$assess, ".")
  }
  # Check intense: names
  if (!all(c("Intensity", "Description", "Layers") %in% names(intense))) {
    stop("`intense` is missing columns", call. = FALSE)
  }
  # Check intense_yrs: range
  if (any(intense_yrs >= pars$years$layers) & !quiet) {
    message("`intense_yrs` >= ", pars$years$layers, ".")
  }
  # Check rescale_yrs: range
  if (any(rescale_yrs >= pars$years$assess) & !quiet) {
    message("`rescale_yrs` >= ", pars$years$assess, ".")
  }
  # Check theta: range
  if (any(na.omit(theta) < 0) & !quiet) message("`theta` < 0.")
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
  areas_sm <- areas %>%
    select(SAR, Region, StatArea, Section, LocationCode, Pool) %>%
    distinct() %>%
    as_tibble()
  # Load all spawn
  spawn <- dbReadTable(conn = access_db, name = where$fns$all_spawn) %>%
    rename(
      LocationCode = Loc_Code, SpawnNumber = Spawn_Number, WidthObs = Width
    ) %>%
    mutate(Method = str_to_title(Method)) %>%
    filter(Year %in% years, LocationCode %in% areas$LocationCode) %>%
    select(Year, LocationCode, SpawnNumber, Length, WidthObs, Method) %>%
    as_tibble()
  # Extract relevant surface data
  surface <- dbReadTable(conn = access_db, name = where$fns$surface) %>%
    rename(LocationCode = Loc_Code, SpawnNumber = Spawn_Number) %>%
    filter(Year %in% years, LocationCode %in% areas_sm$LocationCode) %>%
    left_join(y = areas_sm, by = "LocationCode") %>%
    left_join(y = spawn, by = c("Year", "LocationCode", "SpawnNumber")) %>%
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
      EggLayers = Grass + Rockweed + Kelp + BrownAlgae + LeafyRed + StringyRed +
        Rock + Other,
      Intensity = ifelse(Year %in% rescale_yrs & Intensity > 0,
        Intensity * 2 - 1, Intensity
      )
    ) %>%
    filter(Method %in% c("Surface", "Dive")) %>%
    select(
      Year, Region, StatArea, Section, LocationCode, Pool, SpawnNumber, Length,
      WidthObs, Intensity, EggLayers
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
      EggLayers = ifelse(Year %in% intense_yrs, Layers, EggLayers),
      # Egg density in thousands (10^3 * eggs / m^2); sample j
      EggDens = dens_surf(
        alpha = alpha, beta = beta, egg_layers = EggLayers, quiet = quiet
      )
    )
  # These are the 'original' manual updates that were in the Microsoft Access
  # database: some overwrite good data with no documented reason and have been
  # omitted, others have been omitted because the spawn survey was incomplete.
  # However, they (and others) are still present in the Microsoft Access
  # database, causing discrepancies for HG 1979, as well as WCVI 1982 and 1984.
  # They should get removed from the Microsoft Access database (especially
  # updates 1, 4, and 5 which cause errros). Update 2 is still relevant;
  # updates 3 and 6 no longer have an effect.
  # 1. HG (15 records): Year 1979, SA 2, Intensity 4 (update EggLayers to
  #    2.1496 using intensity table; 14 records overwrite good data)
  # 2. SoG (1 record): Year 1962, SA 14, Intensity 0 (update EggLayers to
  #    0.5529 using intensity 1: spawn was surveyed but not reported)
  # 3. WCVI (4 records): Year 1981, SA 24, EggLayers 0 (update EggLayers to
  #    0.5529 using intensity table)
  # 4. WCVI (7 records): Year 1982, SA 23, Intensity 3 (update EggLayers to
  #    1.3360 using intensity table; 7 records overwrite good data)
  # 5. WCVI (41 records): Year 1984, SA 24, Intensity 0 (update EggLayers to
  #    2.33 -- not sure why/how; 41 records overwrite good data)
  # 6. A27 (14 records): Year 1982, SA 27, EggLayers 0 (update EggLayers to
  #    2.98 using a historical average)
  # Get the number of records with no egg layer info
  no_layers <- eggs %>% filter(EggLayers == 0) # %>%
  # Error if there are missing values
  if (nrow(no_layers) > 0) {
    stop("Missing egg layers for ", nrow(no_layers), " record(s):",
      print(no_layers), ".",
      sep = "", call. = FALSE
    )
  }
  # Output egg layer info
  egg_layers <- eggs %>%
    group_by(Year, Region, StatArea, Section, LocationCode, SpawnNumber) %>%
    summarise(SurfLyrs = mean_na(EggLayers)) %>%
    ungroup()
  # Calculate egg density per spawn number/pool
  eggs_spawn <- eggs %>%
    group_by(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber, Pool
    ) %>%
    # Spawn s
    summarise(EggDens = mean_na(EggDens)) %>%
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
    summarise(SurfSI = sum_na(SurfSI)) %>%
    ungroup() %>%
    full_join(
      y = egg_layers,
      by = c(
        "Year", "Region", "StatArea", "Section", "LocationCode", "SpawnNumber"
      )
    )
  # Calculate annual si by spawn number
  si <- biomass_spawn %>%
    select(Year, Region, StatArea, Section, LocationCode, SpawnNumber, SurfSI)
  # Close the connection
  dbDisconnect(conn = access_db)
  # Assemble into a list
  res <- list(
    surface = surface, eggs = eggs, eggs_spawn = eggs_spawn,
    biomass_spawn = biomass_spawn, si = si
  )
  # Check output: tibble rows
  check_tibble(dat = list(si = res$si), quiet = quiet)
  # Check output: names
  if (!all(c(
    "Year", "Region", "StatArea", "Section", "LocationCode", "SpawnNumber",
    "SurfSI"
  ) %in% names(res$si))) {
    stop("`res$si` is missing columns", call. = FALSE)
  }
  # Return the list
  res
} # End calc_surf_index function

#' Calculate Macrocystis spawn number of eggs per plant.
#'
#' Calculate Pacific Herring Macrocystis spawn number of eggs per plant in
#' thousands of eggs per plant (10^3 * eggs / plant). This function implements a
#' nonlinear multiple regression model to estimate the number of eggs;
#' implemented in \code{\link{calc_macro_index}}
#' \insertCite{HaegeleSchweigert1990}{SpawnIndex}.
#'
#' @param xi Numeric. Regression slope; from \code{\link{pars}}
#'   \insertCite{HaegeleSchweigert1990}{SpawnIndex}.
#' @param gamma Numeric. Regression exponent on egg layers; from
#'   \code{\link{pars}} \insertCite{HaegeleSchweigert1990}{SpawnIndex}.
#' @param delta Numeric. Regression exponent on plant height; from
#'   \code{\link{pars}} \insertCite{HaegeleSchweigert1990}{SpawnIndex}.
#' @param epsilon Numeric. Regression exponent on number of stalks per plant;
#'   from \code{\link{pars}} \insertCite{HaegeleSchweigert1990}{SpawnIndex}.
#' @template param-egg_layers
#' @param height Numeric. Plant height in metres. Message if < 0.
#' @param stalks_per_plant Numeric. Number of stalks per plant. Message if < 0.
#' @template param-quiet
#' @importFrom Rdpack reprompt
#' @importFrom stats na.omit
#' @return Numeric. Number of eggs per plant in thousands. Message if < 0.
#' @references \insertAllCited
#' @seealso \code{\link{pars}} \code{\link{calc_macro_index}}
#' @family calculation functions
#' @export
#' @examples
#' data(pars)
#' eggs_macro(egg_layers = 4, height = 3, stalks_per_plant = 2)
eggs_macro <- function(xi = pars$macrocystis$xi,
                       gamma = pars$macrocystis$gamma,
                       delta = pars$macrocystis$delta,
                       epsilon = pars$macrocystis$epsilon,
                       egg_layers,
                       height,
                       stalks_per_plant,
                       quiet = FALSE) {
  # Check input: NA and numeric
  check_numeric(
    dat = list(
      xi = xi, gamma = gamma, delta = delta, epsilon = epsilon,
      egg_layers = egg_layers, height = height,
      stalks_per_plant = stalks_per_plant
    ),
    quiet = quiet
  )
  # Check egg_layers: range
  if (any(na.omit(egg_layers) < 0) & !quiet) message("`egg_layers` < 0.")
  # Check height: range
  if (any(na.omit(height) < 0) & !quiet) message("`height` < 0.")
  # Check stalks_per_plant: range
  if (any(na.omit(stalks_per_plant) < 0) & !quiet) {
    message("`stalks_per_plant` < 0.")
  }
  # Eggs per plant in thousands (10^3 * eggs / plant; Haegele and Schweigert
  # 1990)
  number <- xi * egg_layers^gamma * height^delta * stalks_per_plant^epsilon *
    1000
  # Check output: NA and numeric
  check_numeric(dat = list(number = number), quiet = quiet)
  # Check output: range
  if (any(na.omit(number) < 0) & !quiet) message("`number` < 0.")
  # Return number
  number
} # End eggs_macro function

#' Calculate the Macrocystis spawn index.
#'
#' Calculate the Pacific Herring Macrocystis spawn index in tonnes
#' \insertCite{HaegeleSchweigert1990}{SpawnIndex}. This function primarily
#' wrangles and prepares the data; the actual calculation is done by
#' \code{\link{eggs_macro}}.
#'
#' @template param-where
#' @template param-areas
#' @template param-years
#' @param chi Numeric. Transect swath (i.e., width) in metres.
#' @param xi Numeric. Regression slope; from \code{\link{pars}}
#'   \insertCite{HaegeleSchweigert1990}{SpawnIndex}.
#' @param gamma Numeric. Regression exponent on egg layers; from
#'   \code{\link{pars}} \insertCite{HaegeleSchweigert1990}{SpawnIndex}.
#' @param delta Numeric. Regression exponent on plant height; from
#'   \code{\link{pars}} \insertCite{HaegeleSchweigert1990}{SpawnIndex}.
#' @param epsilon Numeric. Regression exponent on number of stalks per plant;
#'   from \code{\link{pars}} \insertCite{HaegeleSchweigert1990}{SpawnIndex}.
#' @template param-theta
#' @template param-quiet
#' @importFrom odbc dbConnect odbc dbDisconnect
#' @importFrom DBI dbReadTable
#' @importFrom dplyr select distinct rename left_join filter %>% group_by
#'   summarise ungroup
#' @importFrom tibble as_tibble
#' @importFrom stringr str_to_title
#' @importFrom gfiscamutils mean_na sum_na unique_na
#' @importFrom tidyr replace_na
#' @importFrom Rdpack reprompt
#' @return List. The element \code{si} is a tibble with Macrocystis spawn index
#'   (\code{MacroSI}) in tonnes by spawn number and year. The spawn number is
#'   the finest spatial scale at which we calculate the spawn index. Other
#'   information in this tibble comes from \code{a}: Region, Statistical Area,
#'   Section, and Location code.
#' @note The `spawn index' is a relative index of spawning biomass.
#' @references \insertAllCited
#' @seealso \code{\link{HerringSpawn}} \code{\link{load_area_data}}
#'   \code{\link{eggs_to_sb}} \code{\link{pars}}
#'   \code{\link{eggs_macro}}
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
#' macro_spawn <- calc_macro_index(
#'   where = macro_loc, areas = areas, years = 2010:2015
#' )
#' macro_spawn$si
calc_macro_index <- function(where,
                             areas,
                             years,
                             chi = 2,
                             xi = pars$macrocystis$xi,
                             gamma = pars$macrocystis$gamma,
                             delta = pars$macrocystis$delta,
                             epsilon = pars$macrocystis$epsilon,
                             theta = eggs_to_sb(),
                             quiet = FALSE) {
  # Check input: NA and numeric
  check_numeric(
    dat = list(
      years = years, chi = chi, xi = xi, gamma = gamma, delta = delta,
      epsilon = epsilon, theta = theta
    ),
    quiet = quiet
  )
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
  # Check input: tibble rows
  check_tibble(dat = list(areas = areas), quiet = quiet)
  # Check areas: names
  if (!all(c(
    "SAR", "Region", "StatArea", "Section", "LocationCode", "Pool"
  ) %in% names(areas))) {
    stop("`areas` is missing columns", call. = FALSE)
  }
  # Check years: range
  if (any(years < pars$years$assess) & !quiet) {
    message("`years` < ", pars$years$assess, ".")
  }
  # Check chi: value
  if (chi != 2 & !quiet) message("`chi` != 2")
  # Check theta: range
  if (any(na.omit(theta) < 0) & !quiet) message("`theta` < 0.")
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
  areas_sm <- areas %>%
    select(Region, StatArea, Section, LocationCode, Pool) %>%
    distinct() %>%
    as_tibble()
  # Load all spawn
  spawn <- dbReadTable(conn = access_db, name = where$fns$all_spawn) %>%
    rename(
      LocationCode = Loc_Code, SpawnNumber = Spawn_Number,
      LengthMacro = Length_Macrocystis
    ) %>%
    mutate(Method = str_to_title(Method)) %>%
    filter(Year %in% years, LocationCode %in% areas_sm$LocationCode) %>%
    select(Year, LocationCode, SpawnNumber, LengthMacro, Length, Method) %>%
    as_tibble()
  # Get plant-level data
  plants <- dbReadTable(conn = access_db, name = where$fns$plants) %>%
    rename(LocationCode = Loc_Code, SpawnNumber = Spawn_Number) %>%
    filter(
      Year %in% years, LocationCode %in% areas_sm$LocationCode, !is.na(Mature)
    ) %>%
    select(Year, LocationCode, SpawnNumber, Transect, Mature) %>%
    as_tibble()
  # Get transect-level data
  transects <- dbReadTable(conn = access_db, name = where$fns$transects) %>%
    rename(LocationCode = Loc_Code, SpawnNumber = Spawn_Number) %>%
    filter(Year %in% years, LocationCode %in% areas_sm$LocationCode) %>%
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
    mutate(Swath = chi)
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
      Height = unique_na(Height[Mature > 0]),
      EggLayers = unique_na(Layers[Mature > 0]),
      Stalks = sum_na(Mature[Mature > 0]),
      Plants = length(Mature[Mature > 0])
    ) %>%
    ungroup() %>%
    mutate(
      Height = as.numeric(Height), Stalks = as.numeric(Stalks),
      Plants = as.numeric(Plants)
    )
  # Calculate spawn-level data
  biomass_spawn <- dat_trans %>%
    left_join(y = spawn, by = c("Year", "LocationCode", "SpawnNumber")) %>%
    mutate(LengthMacro = ifelse(is.na(LengthMacro), Length, LengthMacro)) %>%
    filter(Method %in% c("Surface", "Dive")) %>%
    group_by(Year, Region, StatArea, Section, LocationCode, SpawnNumber) %>%
    # Spawn s
    summarise(
      LengthMacro = unique(LengthMacro),
      Width = mean_na(Width),
      Area = sum_na(Area),
      Plants = sum_na(Plants),
      Stalks = sum_na(Stalks),
      Height = as.numeric(mean_na(Height)),
      EggLayers = as.numeric(mean_na(EggLayers)),
      StalksPerPlant = Stalks / Plants,
      # Eggs per plant in thousands (10^3 * eggs / plant); spawn s
      EggsPerPlant = eggs_macro(
        xi = xi, gamma = gamma, delta = delta, epsilon = epsilon,
        egg_layers = EggLayers, height = Height,
        stalks_per_plant = StalksPerPlant, quiet = quiet
      ),
      # Eggs density in thousands (10^3 * eggs / m^2; spawn s
      EggDens = EggsPerPlant * Plants / Area,
      # Biomass in tonnes, based on Hay (1985), and Hay and Brett (1988); spawn
      # s
      MacroSI = EggDens * LengthMacro * Width * 1000 / theta
    ) %>%
    rename(MacroLyrs = EggLayers) %>%
    ungroup()
  # Return the macrocystis spawn
  si <- biomass_spawn %>%
    select(Year, Region, StatArea, Section, LocationCode, SpawnNumber, MacroSI)
  # Close the connection
  dbDisconnect(conn = access_db)
  # Assemble into a list
  res <- list(
    dat = dat, dat_trans = dat_trans, biomass_spawn = biomass_spawn, si = si
  )
  # Check output: tibble rows
  check_tibble(dat = list(si = res$si), quiet = quiet)
  # Check output: names
  if (!all(c(
    "Year", "Region", "StatArea", "Section", "LocationCode", "SpawnNumber",
    "MacroSI"
  ) %in% names(res$si))) {
    stop("`res$si` is missing columns", call. = FALSE)
  }
  # Return the list
  res
} # End calc_macro_index function

#' Calculate understory spawn egg density on substrate.
#'
#' Calculate Pacific Herring understory spawn egg density on substrate in
#' thousands of eggs per square metre (10^3 * eggs / m^2). This function
#' implements a linear model to estimate egg density on substrate; implemented
#' in \code{\link{calc_under_index}} \insertCite{HaegeleEtal1979}{SpawnIndex}.
#'
#' @param varphi Numeric. Regression slope for substrate; from
#'   \code{\link{pars}} \insertCite{HaegeleEtal1979}{SpawnIndex}.
#' @template param-egg_layers
#' @template param-proportion
#' @template param-quiet
#' @importFrom Rdpack reprompt
#' @importFrom stats na.omit
#' @return Numeric. Egg density in thousands of eggs per square metre. Message
#'   if < 0.
#' @references \insertAllCited
#' @seealso \code{\link{pars}} \code{\link{calc_under_index}}
#'   \code{\link{dens_under_alg}}
#' @family calculation functions
#' @export
#' @examples
#' data(pars)
#' dens_under_sub(egg_layers = 4, proportion = 0.5)
dens_under_sub <- function(varphi = pars$understory$varphi,
                           egg_layers,
                           proportion,
                           quiet = FALSE) {
  # Check input: NA and numeric
  check_numeric(
    dat = list(
      varphi = varphi, egg_layers = egg_layers, proportion = proportion
    ),
    quiet = quiet
  )
  # Check egg_layers: range
  if (any(na.omit(egg_layers) < 0) & !quiet) message("`egg_layers` < 0.")
  # Check proportion: range
  if ((any(na.omit(proportion) < 0) | any(na.omit(proportion) > 1)) & !quiet) {
    message("`proportion` < 0 and/or > 1.")
  }
  # Egg density in thousands (eggs x 10^3 / m^2; Haegele et al. 1979)
  density <- varphi * egg_layers * proportion
  # Check output: NA and numeric
  check_numeric(dat = list(density = density), quiet = quiet)
  # Check density: range
  if (any(na.omit(density) < 0) & !quiet) message("`density` < 0.")
  # Return density
  density
} # End dens_under_sub function

#' Calculate for understory spawn egg density on algae.
#'
#' Calculate Pacific Herring understory spawn egg density on algae in thousands
#' of eggs per square metre (10^3 * eggs / m^2). This function implements a
#' generalized linear model to estimate egg density on algae; implemented in
#' \code{\link{calc_under_index}} \insertCite{Schweigert2005}{SpawnIndex}.
#'
#' @param vartheta Numeric. Regression slope for algae; from \code{\link{pars}}
#'   \insertCite{Schweigert2005}{SpawnIndex}.
#' @param varrho Numeric. Regression exponent on number of egg layers; from
#'   \code{\link{pars}} \insertCite{Schweigert2005}{SpawnIndex}.
#' @param varsigma Numeric. Regression exponent on proportion of algae; from
#'   \code{\link{pars}} \insertCite{Schweigert2005}{SpawnIndex}.
#' @template param-egg_layers
#' @template param-proportion
#' @param coeff Numeric. Algae coefficients; from \code{\link{algae_coefs}}
#'   \insertCite{Schweigert2005}{SpawnIndex}. Message if < 0.
#' @template param-quiet
#' @importFrom Rdpack reprompt
#' @importFrom stats na.omit
#' @return Numeric. Egg density in thousands of eggs per square metre. Message
#'   if < 0.
#' @references \insertAllCited
#' @seealso \code{\link{pars}} \code{\link{calc_under_index}}
#'   \code{\link{dens_under_sub}}
#' @family calculation functions
#' @export
#' @examples
#' data(pars)
#' dens_under_alg(egg_layers = 4, proportion = 0.5, coeff = 1.1)
dens_under_alg <- function(vartheta = pars$understory$vartheta,
                           varrho = pars$understory$varrho,
                           varsigma = pars$understory$varsigma,
                           egg_layers,
                           proportion,
                           coeff,
                           quiet = FALSE) {
  # Check input: NA and numeric
  check_numeric(
    dat = list(
      vartheta = vartheta, varrho = varrho, varsigma = varsigma,
      egg_layers = egg_layers, proportion = proportion, coeff = coeff
    ),
    quiet = quiet
  )
  # Check egg_layers: range
  if (any(na.omit(egg_layers) < 0) & !quiet) message("`egg_layers` < 0.")
  # Check proportion: range
  if ((any(na.omit(proportion) < 0) | any(na.omit(proportion) > 1)) & !quiet) {
    message("`proportion` < 0 and/or > 1.")
  }
  # Check coeff: range
  if (any(na.omit(coeff) < 0) & !quiet) message("`coeff` < 0.")
  # Egg density in thousands (10^3 * eggs / m^2; Schweigert 2005); quadrat
  # size coefficient not required because all quadrats are 0.5m^2 (1.0512)
  density <- vartheta * egg_layers^varrho * proportion^varsigma * coeff * 1.0512
  # Check output: NA and numeric
  check_numeric(dat = list(density = density), quiet = quiet)
  # Check density: range
  if (any(na.omit(density) < 0) & !quiet) message("`density` < 0.")
  # Return density
  density
} # End dens_under_alg function

#' Calculate the understory spawn index.
#'
#' Calculate the Pacific Herring understory spawn index in tonnes
#' \insertCite{HaegeleEtal1979,Schweigert2005}{SpawnIndex}. This function
#' primarily wrangles and prepares the data; the actual calculations are done by
#' \code{\link{dens_under_sub}} and \code{\link{dens_under_alg}}.
#'
#' @template param-where
#' @template param-areas
#' @template param-years
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
#' @template param-theta
#' @template param-quiet
#' @importFrom odbc dbConnect odbc dbDisconnect
#' @importFrom DBI dbReadTable
#' @importFrom dplyr select distinct rename left_join filter %>% ungroup
#'   bind_rows
#' @importFrom tibble as_tibble
#' @importFrom stringr str_to_title str_to_upper
#' @importFrom gfiscamutils mean_na sum_na unique_na wt_mean_na
#' @importFrom tidyr replace_na gather
#' @importFrom Rdpack reprompt
#' @return List. The element \code{si} is a tibble with understory spawn index
#'   (\code{UnderSI}) in tonnes by spawn number and year. The spawn number is
#'   the finest spatial scale at which we calculate the spawn index. Other
#'   information in this tibble comes from \code{a}: Region, Statistical Area,
#'   Section, and Location code.
#' @note The `spawn index' is a relative index of spawning biomass.
#' @references \insertAllCited
#' @seealso \code{\link{HerringSpawn}} \code{\link{load_area_data}}
#'   \code{\link{eggs_to_sb}} \code{\link{pars}}
#'   \code{\link{algae_coefs}} \code{\link{dens_under_sub}}
#'   \code{\link{dens_under_alg}}
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
#' under_spawn <- calc_under_index(
#'   where = under_loc, areas = areas, years = 2010:2015
#' )
#' under_spawn$si
calc_under_index <- function(where,
                             areas,
                             years,
                             alg_coefs = algae_coefs,
                             tau = under_width_facs,
                             varphi = pars$understory$varphi,
                             vartheta = pars$understory$vartheta,
                             varrho = pars$under$varrho,
                             varsigma = pars$understory$varsigma,
                             theta = eggs_to_sb(),
                             quiet = FALSE) {
  # Check input: NA and numeric
  check_numeric(
    dat = list(
      years = years, varphi = varphi, vartheta = vartheta, varrho = varrho,
      varsigma = varsigma, theta = theta
    ),
    quiet = quiet
  )
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
  # Check input: tibble rows
  check_tibble(
    dat = list(areas = areas, alg_coefs = alg_coefs, tau = tau), quiet = quiet
  )
  # Check areas: names
  if (!all(c(
    "SAR", "Region", "StatArea", "Section", "LocationCode", "Pool"
  ) %in% names(areas))) {
    stop("`areas` is missing columns", call. = FALSE)
  }
  # Check years: range
  if (any(years < pars$years$assess) & !quiet) {
    message("`years` < ", pars$years$assess, ".")
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
  # Check theta: range
  if (any(na.omit(theta) < 0) & !quiet) message("`theta` < 0.")
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
  areas_sm1 <- areas %>%
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
    filter(Year %in% years, LocationCode %in% areas_sm1$LocationCode) %>%
    select(Year, LocationCode, SpawnNumber, LengthAlgae, Length, Method) %>%
    as_tibble()
  # Load algae transects
  alg_trans <- dbReadTable(conn = access_db, name = where$fns$alg_trans) %>%
    rename(
      LocationCode = Loc_Code, SpawnNumber = Spawn_Number,
      QuadratSize = Quadrat_Size, WidthObs = Width_Recorded
    ) %>%
    filter(Year %in% years, LocationCode %in% areas_sm1$LocationCode) %>%
    select(Year, LocationCode, SpawnNumber, Transect, WidthObs, QuadratSize) %>%
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
      SubLayers = Layers_Bottom
    ) %>%
    filter(Year %in% years, LocationCode %in% areas_sm1$LocationCode) %>%
    mutate(SubProp = Percent_Bottom / 100) %>%
    select(
      Year, LocationCode, SpawnNumber, Transect, Station, SubLayers, SubProp
    ) %>%
    as_tibble()
  # Get egg layer info: substrate
  egg_layers_sub <- stations %>%
    group_by(Year, LocationCode, SpawnNumber, Transect) %>%
    summarise(Layers = mean_na(SubLayers)) %>%
    ungroup() %>%
    mutate(Source = "Substrate")
  # Load algae
  algae <- dbReadTable(conn = access_db, name = where$fns$algae) %>%
    rename(
      LocationCode = Loc_Code, SpawnNumber = Spawn_Number,
      AlgType = Type_Vegetation, AlgLayers = Layers_Vegetation
    ) %>%
    filter(Year %in% years, LocationCode %in% areas_sm1$LocationCode) %>%
    mutate(
      AlgType = str_to_upper(AlgType),
      AlgProp = Percent_Vegetation / 100,
      AlgProp = ifelse(AlgProp > 1, 1, AlgProp)
    ) %>%
    select(
      Year, LocationCode, SpawnNumber, Transect, Station, AlgType, AlgLayers,
      AlgProp
    ) %>%
    as_tibble()
  # Get egg layer info: algae
  egg_layers_alg <- algae %>%
    group_by(Year, LocationCode, SpawnNumber, Transect) %>%
    summarise(Layers = mean_na(AlgLayers)) %>%
    ungroup() %>%
    mutate(Source = "Algae")
  # Combine egg layer info
  egg_layers <- bind_rows(egg_layers_sub, egg_layers_alg) %>%
    group_by(Year, LocationCode, SpawnNumber, Transect) %>%
    summarise(Layers = mean_na(Layers)) %>%
    group_by(Year, LocationCode, SpawnNumber) %>%
    summarise(UnderLyrs = mean_na(Layers)) %>%
    ungroup()
  # If there are missing algae types
  if (any(!algae$AlgType %in% alg_coefs$AlgType)) {
    # Get missing algae type(s)
    miss_alg <- unique(algae$AlgType[!algae$AlgType %in%
      alg_coefs$AlgType])
    # Error, and show missing type(s)
    stop("Missing algae type(s): ", paste_nicely(miss_alg), ".", call. = FALSE)
  } # End if there are missing algae types
  # Get a small subset of area data
  areas_sm_2 <- areas %>%
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
    # Egg density in thousands (eggs x 10^3 / m^2); quadrat q
    mutate(EggDensSub = dens_under_sub(
      varphi = varphi, egg_layers = SubLayers, proportion = SubProp,
      quiet = quiet
    )) %>%
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
    # Egg density in thousands (10^3 * eggs / m^2); algae a
    mutate(EggDensAlg = dens_under_alg(
      vartheta = vartheta, varrho = varrho, varsigma = varsigma,
      egg_layers = AlgLayers, proportion = AlgProp, coeff = Coef, quiet = quiet
    )) %>%
    group_by(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber, Transect,
      Station
    ) %>%
    # Quadrat q
    summarise(EggDensAlg = sum_na(EggDensAlg)) %>%
    replace_na(replace = list(EggDensAlg = 0)) %>%
    ungroup()
  # Combine eggs
  eggs <- eggs_sub %>%
    full_join(y = eggs_alg, by = c(
      "Year", "Region", "StatArea", "Section", "LocationCode", "SpawnNumber",
      "Transect", "Station"
    )) %>%
    replace_na(replace = list(Width = 0, EggDensSub = 0, EggDensAlg = 0)) %>%
    mutate(EggDensSub = ifelse(Width > 0, EggDensSub, 0))
  # Calculate total egg density by station/quadrat
  eggs_station <- eggs %>%
    # Total egg density in thousands (10^3 * eggs / m^2); quadrat q
    mutate(EggDens = EggDensSub + EggDensAlg) %>%
    filter(!is.na(Station))
  # Widths
  widths <- eggs_station %>%
    group_by(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber, Transect
    ) %>%
    summarise(Width = unique(Width)) %>%
    group_by(Year, Region, StatArea, Section, LocationCode, SpawnNumber) %>%
    # Spawn s
    summarise(WidthBar = mean_na(Width)) %>%
    ungroup()
  # Calculate transect-level metrics
  eggs_trans <- eggs_station %>%
    filter(Width > 0) %>%
    group_by(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber, Transect
    ) %>%
    # Transect t
    summarise(
      EggDens = mean_na(EggDens),
      Width = unique(Width)
    ) %>%
    ungroup()
  # Calculate spawn number-level metrics
  eggs_spawn <- eggs_trans %>%
    left_join(y = spawn, by = c("Year", "LocationCode", "SpawnNumber")) %>%
    mutate(LengthAlgae = ifelse(is.na(LengthAlgae), Length, LengthAlgae)) %>%
    filter(Method %in% c("Surface", "Dive")) %>%
    left_join(y = widths, by = c(
      "Year", "Region", "StatArea", "Section", "LocationCode", "SpawnNumber"
    )) %>%
    group_by(Year, Region, StatArea, Section, LocationCode, SpawnNumber) %>%
    # Spawn s
    summarise(
      WidthBar = unique(WidthBar),
      LengthAlgae = unique(LengthAlgae),
      EggDens = wt_mean_na(EggDens, w = Width)
    ) %>%
    ungroup()
  # Calculate understory biomass by spawn number
  biomass_spawn <- eggs_spawn %>%
    mutate(
      # Biomass in tonnes, based on Hay (1985), and Hay and Brett (1988); spawn
      # s
      UnderSI = EggDens * LengthAlgae * WidthBar * 1000 / theta
    ) %>%
    left_join(y = egg_layers, by = c("Year", "LocationCode", "SpawnNumber"))
  # Calculate understory si by spawn number
  si <- biomass_spawn %>%
    select(Year, Region, StatArea, Section, LocationCode, SpawnNumber, UnderSI)
  # Close the connection
  dbDisconnect(conn = access_db)
  # Assemble into a list
  res <- list(
    stations = stations, algae = algae, eggs = eggs,
    eggs_station = eggs_station, eggs_trans = eggs_trans,
    eggs_spawn = eggs_spawn, biomass_spawn = biomass_spawn, si = si
  )
  # Check output: tibble rows
  check_tibble(dat = list(si = res$si), quiet = quiet)
  # Check output: names
  if (!all(c(
    "Year", "Region", "StatArea", "Section", "LocationCode", "SpawnNumber",
    "UnderSI"
  ) %in% names(res$si))) {
    stop("`res$si` is missing columns", call. = FALSE)
  }
  # Return the list
  res
} # End calc_under_index function
