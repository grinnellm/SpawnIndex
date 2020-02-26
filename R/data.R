#' Understory spawn width correction factors.
#'
#' Spawn width correction factors for Pacific Herring understory spawn surveys
#' by region and year.
#'
#' @format Tibble with 12 rows and 8 columns (one for year, and one for each
#'   regions).
#' @note Adjust spawn width for affected understory spawn surveys by multiplying
#'   the observed width by the corresponding correction fator for the affected
#'   years.
#' @docType data
#' @usage data(underWidthFac)
#' @seealso \code{\link{CalcUnderSpawn}}
#' @examples
#' data(underWidthFac)
"underWidthFac"

#' Parameters for spawn index calculations
#'
#' Parameters for Pacific Herring spawn index calculations: converting eggs to
#' biomass, spawn-on-kelp calculations, as well as surface, Macrocystis, and
#' understory spawn index calculations.
#'
#' @format List with 5 items:
#' \describe{
#'   \item{conversion}{List with 2 items:
#'     omega is the number of eggs per kilogram of female spawners
#'     \insertCite{Hay1985,HayBrett1988}{SpawnIndex}, and
#'     phi is the proportion of spawners that are female.}
#'   \item{SOK}{List with 3 items:
#'     nu,
#'     upsilon, and
#'     M.}
#'   \item{surface}{List with 2 items:
#'     alpha is the regression intercept, and
#'     beta is the regression slope.}
#'   \item{macrocystis}{List with 4 items:
#'     beta,
#'     gamma,
#'     delta, and
#'     epsilon.}
#'   \item{understory}{List with 4 items:
#'     alpha,
#'     beta,
#'     gamma, and
#'     delta.}
#' }
#' @docType data
#' @references \insertAllCited{}
#' @usage data(pars)
#' @seealso \code{\link{CalcEggConversion}} \code{\link{CalcBiomassSOK}}
#'   \code{\link{CalcSurfSpawn}} \code{\link{CalcMacroSpawn}}
#'   \code{\link{CalcUnderSpawn}}
#' @examples
#' data(pars)
"pars"

#' Example Pacific Herring spawn survey database.
#'
#' Example Pacific Herring spawn survey database. This database contains a
#' subset of tables from the spawn survey database, and some additional data so
#' that examples run.
#'
#' @format MS Access databases.
#' @docType data
#' @seealso \code{\link{LoadAreaData}} \code{\link{CalcSurfSpawn}}
#'   \code{\link{CalcMacroSpawn}} \code{\link{CalcUnderSpawn}}
#'
system.file("extdata", "HerringSpawn.mdb", package = "SpawnIndex")
