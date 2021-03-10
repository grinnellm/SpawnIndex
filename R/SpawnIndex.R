#' SpawnIndex: Calculate the spawn index for Pacific Herring (\emph{Clupea
#' pallasii}) in British Columbia, Canada.
#'
#' The SpawnIndex package provides data, parameter values, and methods to
#' calculate the spawn index for Pacific Herring (\emph{Clupea pallasii}) in
#' British Columbia (BC), Canada. Essentially, spawn index calculations convert
#' spawn survey observations (e.g., spawn extent, number of egg layers,
#' substrate type) to the Pacific Herring spawn index in BC. There are three
#' types of spawn survey observations: surface spawn observations, Macrocystis
#' spawn observations, and understory spawn observations. In addition, we
#' include methods to convert eggs to biomass, and estimate spawning biomass in
#' spawn-on-kelp operations. Note that the 'spawn index' is a relative index of
#' spawning biomass.
#'
#' The SpawnIndex package provides data, and four families of functions: load,
#' calculation, check, and utility.
#'
#' @section Data: The data are understory spawn width correction factors
#'   (\code{\link{under_width_facs}}), parameters for spawn index calculations
#'   (\code{\link{pars}}), spawn intensity categories (\code{\link{intensity}}),
#'   and algae coefficients (\code{\link{algae_coefs}}). In addition, there is
#'   an example database with spawn survey observations
#'   (\code{\link{HerringSpawn}}).
#'
#' @section Load: The load functions are \code{\link{load_area_data}},
#'   \code{\link{load_all_spawn}}, and \code{\link{get_width}}.
#'
#' @section Calculation: The calculation functions are \code{\link{eggs_to_sb}},
#'   \code{\link{calc_sok_sb}}, \code{\link{dens_surf}},
#'   \code{\link{calc_surf_index}}, \code{\link{eggs_macro}},
#'   \code{\link{calc_macro_index}}, \code{\link{dens_under_sub}},
#'   \code{\link{dens_under_alg}}, and \code{\link{calc_under_index}}.
#'
#' @section Check: The check functions are \code{\link{check_numeric}}, and
#'   \code{\link{check_tibble}}.
#'
#' @section Utility: The utility function is \code{\link{paste_nicely}}.
#'
#' @docType package
#' @name SpawnIndex
#' @note The `spawn index' is a relative index of spawning biomass. Read the
#'   technical report for more details and background on spawn index
#'   calculations \insertCite{GrinnellEtalYYYY}{SpawnIndex}.
#' @references \insertAllCited{}
NULL
