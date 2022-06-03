#' SpawnIndex: `r read.dcf(file = "DESCRIPTION", fields = "Title")`.
#'
#' `r read.dcf(file = "DESCRIPTION", fields = "Description")`
#'
#' The SpawnIndex package provides raw data, data, and four families of
#' functions: load, calculation, check, and utility.
#'
#' @section Raw data: The raw data is an example database with spawn survey
#'   observations (\code{\link{HerringSpawn}}).
#'
#' @section Data: The data are region names (\code{\link{regions}}), understory
#'   spawn width correction factors (\code{\link{under_width_facs}}), parameters
#'   for spawn index calculations (\code{\link{pars}}), spawn intensity
#'   categories (\code{\link{intensity}}), algae coefficients
#'   (\code{\link{algae_coefs}}), and a simple feature collection with Section
#'   polygons (\code{\link{sections}}).
#'
#' @section Load functions: The load functions are \code{\link{load_area_data}},
#'   \code{\link{load_all_spawn}}, \code{\link{load_width}}, and
#'   \code{\link{load_sections}}.
#'
#' @section Calculation functions: The calculation functions are
#'   \code{\link{eggs_to_sb}}, \code{\link{calc_sok_index}},
#'   \code{\link{dens_surf}}, \code{\link{calc_surf_index}},
#'   \code{\link{eggs_macro}}, \code{\link{calc_macro_index}},
#'   \code{\link{dens_under_sub}}, \code{\link{dens_under_alg}}, and
#'   \code{\link{calc_under_index}}.
#'
#' @section Check functions: The check functions are
#'   \code{\link{check_numeric}}, \code{\link{check_tibble}}, and
#'   \code{\link{check_where}}.
#'
#' @section Utility functions: The utility function is
#'   \code{\link{paste_nicely}}.
#'
#' @docType package
#' @name SpawnIndex
#' @note The `spawn index' is a relative index of spawning biomass. Read the
#'   technical report for more details and background on spawn index
#'   calculations \insertCite{GrinnellEtalYYYY}{SpawnIndex}.
#' @references \insertAllCited{}
NULL
