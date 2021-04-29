#' Cross-walk table for regions and region names.
#'
#' Cross-walk table with stock assessment region (SAR) numbers, regions, region
#' names, and whether regions are major or minor
#' \insertCite{GrinnellEtalYYYY}{SpawnIndex}.
#'
#' @format Tibble with rows for regions and columns for attributes.
#' @note JS is not an official SAR. In addition, JS sections 132 and 135 are
#'   also in SoG.
#' @docType data
#' @references \insertAllCited{}
#' @seealso \code{\link{load_area_data}}
#' @family parameters
#' @examples
#' data(regions)
#' regions
"regions"

#' Understory spawn width correction factors.
#'
#' Spawn width correction factors for Pacific Herring understory spawn surveys
#' by region and year. These correction factors account for an error that
#' resulted in underestimated spawn width for understory dive surveys
#' \insertCite{ClearyEtal2017,GrinnellEtalYYYY}{SpawnIndex}.
#'
#' @format Tibble with rows for years and columns for regions.
#' @note Adjust spawn width for affected understory spawn surveys by multiplying
#'   the observed width by the corresponding correction factor for the affected
#'   years.
#' @docType data
#' @references \insertAllCited{}
#' @seealso \code{\link{calc_under_index}}
#' @family parameters
#' @examples
#' data(under_width_facs)
#' under_width_facs
"under_width_facs"

#' Parameters for spawn index calculations.
#'
#' Parameters for Pacific Herring spawn index calculations: converting eggs to
#' biomass, spawn-on-kelp (SOK) calculations, as well as surface, Macrocystis,
#' and understory spawn index calculations. In addition, years in which there
#' were changes to the survey protocol and data.
#'
#' @format List with items:
#' \describe{
#' \item{conversion}{List with items:
#'   \emph{omega} is the number of eggs per kilogram of female spawners
#'     \insertCite{Hay1985,HayBrett1988}{SpawnIndex}, and
#'   \emph{female} is the proportion of spawners that are female.}
#' \item{sok}{List with items:
#'   \emph{nu} is the proportion of SOK product that is kelp
#'     \insertCite{ShieldsEtal1985}{SpawnIndex},
#'   \emph{upsilon} is the SOK product weight increase during a 24-hour brining
#'     period as a proportion \insertCite{WhyteEnglar1977}{SpawnIndex}, and
#'   \emph{w} is the average weight in kilograms of a fertilized egg
#'     \insertCite{HayMiller1982}{SpawnIndex}.}
#' \item{surface}{List with items:
#'   \emph{alpha} is the regression intercept
#'     \insertCite{SchweigertEtal1997}{SpawnIndex}, and
#'   \emph{beta} is the regression slope
#'     \insertCite{SchweigertEtal1997}{SpawnIndex}.}
#' \item{macrocystis}{List with items:
#'   \emph{xi} is the regression slope
#'     \insertCite{HaegeleSchweigert1990}{SpawnIndex},
#'   \emph{gamma} is the regression exponent on egg layers
#'     \insertCite{HaegeleSchweigert1990}{SpawnIndex},
#'   \emph{delta} is the regression exponent on plant height
#'     \insertCite{HaegeleSchweigert1990}{SpawnIndex}, and
#'   \emph{epsilon} is the regression exponent on number of stalks per plant
#'     \insertCite{HaegeleSchweigert1990}{SpawnIndex}.}
#' \item{understory}{List with items:
#'   \emph{varphi} is the regression slope for substrate
#'     \insertCite{HaegeleEtal1979}{SpawnIndex},
#'   \emph{vartheta} is the regression slope for algae
#'     \insertCite{Schweigert2005}{SpawnIndex},
#'   \emph{varrho} is the regression exponent on number of egg layers
#'     \insertCite{Schweigert2005}{SpawnIndex}, and
#'   \emph{varsigma} is the regression exponent on proportion of algae
#'     \insertCite{Schweigert2005}{SpawnIndex}.}
#' \item{years}{List with items \insertCite{GrinnellEtalYYYY}{SpawnIndex}:
#'   \emph{survey} is the first year of spawn survey data,
#'   \emph{assess} is the first year of spawn survey data that is reliable for
#'     indexing purposes,
#'   \emph{nine_cats} is the first year that spawn surveyors used nine
#'     (instead of five) spawn intensity categories,
#'   \emph{layers} is the first year that spawn surveyors estimated the number
#'     of egg layers directly (instead of intensity categories), and
#'   \emph{dive} is the first year of the dive survey period.}}
#' @docType data
#' @importFrom Rdpack reprompt
#' @references \insertAllCited{}
#' @seealso \code{\link{load_all_spawn}} \code{\link{eggs_to_sb}}
#'   \code{\link{calc_sok_index}} \code{\link{calc_surf_index}}
#'   \code{\link{calc_macro_index}} \code{\link{calc_under_index}}
#' @family parameters
#' @examples
#' data(pars)
#' pars
"pars"

#' Spawn intensity categories and number of egg layers.
#'
#' Spawn intensity categories and number of egg layers for Pacific Herring
#' surface spawn surveys. From `r pars$years$survey` to
#' `r pars$years$layers - 1`, surface spawn surveyors categorized spawn by
#' subjective 'intensity' categories instead of directly estimating the number
#' of egg layers
#' \insertCite{HayKronlund1987,SchweigertStocker1988,GrinnellEtalYYYY}{SpawnIndex}.
#' Surveyors used five categories from `r pars$years$survey` to
#' `r pars$years$nine_cats - 1`, and then nine categories from
#' `r pars$years$nine_cats` to `r pars$years$layers - 1`.
#'
#' @format Tibble with 9 rows and 3 columns: intensity category, description,
#'   and number of egg layers.
#' @docType data
#' @importFrom Rdpack reprompt
#' @references \insertAllCited{}
#' @seealso \code{\link{calc_surf_index}}
#' @family parameters
#' @examples
#' data(intensity)
#' intensity
"intensity"

#' Algae types and coefficients.
#'
#' Algae types and coefficients for Pacific Herring understory spawn surveys
#' \insertCite{Schweigert2005}{SpawnIndex}. Algae coefficients account for the
#' effect of algae morphology on Pacific Herring egg density
#' \insertCite{GrinnellEtalYYYY}{SpawnIndex}.
#'
#' @format Tibble with 8 rows and 3 columns: algae type, algae code, and
#'   coefficient.
#' @docType data
#' @importFrom Rdpack reprompt
#' @references \insertAllCited{}
#' @seealso \code{\link{calc_under_index}}
#' @family parameters
#' @examples
#' data(algae_coefs)
#' algae_coefs
"algae_coefs"
