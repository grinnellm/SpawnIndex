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
#' @seealso \code{\link{CalcUnderSpawn}}
#' @examples
#' data(underWidthFac)
#' underWidthFac
"underWidthFac"

#' Parameters for spawn index calculations.
#'
#' Parameters for Pacific Herring spawn index calculations: converting eggs to
#' biomass, spawn-on-kelp calculations, as well as surface, Macrocystis, and
#' understory spawn index calculations.
#'
#' @format List with 5 items:
#' \describe{
#' \item{conversion}{List with 2 items:
#'   \emph{omega} is the number of eggs per kilogram of female spawners
#'   \insertCite{Hay1985,HayBrett1988}{SpawnIndex}, and \emph{phi} is the
#'   proportion of spawners that are female.}
#' \item{SOK}{List with 3 items:
#'   \emph{nu} is the proportion of SOK product that is eggs, not kelp
#'   \insertCite{ShieldsEtal1985}{SpawnIndex}, \emph{upsilon} is the proportion
#'   of SOK product that is eggs after brining
#'   \insertCite{WhyteEnglar1977}{SpawnIndex}, and \emph{M} is the average
#'   weight in kilograms of a fertilized egg
#'   \insertCite{HayMiller1982}{SpawnIndex}.}
#' \item{surface}{List with 2 items:
#'   \emph{alpha} is the regression intercept
#'   \insertCite{SchweigertEtal1997}{SpawnIndex}, and \emph{beta} is the
#'   regression slope \insertCite{SchweigertEtal1997}{SpawnIndex}.}
#' \item{macrocystis}{List with 4 items: \emph{beta} is the regression slope
#'   \insertCite{HaegeleSchweigert1990}{SpawnIndex}, \emph{gamma} is the
#'   regression exponent on egg layers
#'   \insertCite{HaegeleSchweigert1990}{SpawnIndex}, \emph{delta} is the
#'   regression exponent on plant height
#'   \insertCite{HaegeleSchweigert1990}{SpawnIndex}, and \emph{epsilon} is the
#'   regression exponent on number of stalks per plant
#'   \insertCite{HaegeleSchweigert1990}{SpawnIndex}.}
#' \item{understory}{List
#'   with 4 items: \emph{alpha} is the regression slope for substrate
#'   \insertCite{HaegeleEtal1979}{SpawnIndex}, \emph{beta} is the regression
#'   slope for algae \insertCite{Schweigert2005}{SpawnIndex}, \emph{gamma} is
#'   the regression exponent on number of egg layers
#'   \insertCite{Schweigert2005}{SpawnIndex}, and \emph{delta} is the regression
#'   exponent on proportion of algae \insertCite{Schweigert2005}{SpawnIndex}.} }
#' @docType data
#' @importFrom Rdpack reprompt
#' @references \insertAllCited{}
#' @seealso \code{\link{CalcEggConversion}} \code{\link{CalcBiomassSOK}}
#'   \code{\link{CalcSurfSpawn}} \code{\link{CalcMacroSpawn}}
#'   \code{\link{CalcUnderSpawn}}
#' @examples
#' data(pars)
#' pars
"pars"

#' Spawn intensity categories and number of egg layers.
#'
#' Spawn intensity categories and number of egg layers for Pacific Herring
#' surface spawn surveys
#' \insertCite{HayKronlund1987,SchweigertStocker1988}{SpawnIndex}.
#'
#' @format Tibble with 9 rows and 3 columns: intensity category, description,
#'   and number of egg layers.
#' @docType data
#' @importFrom Rdpack reprompt
#' @references \insertAllCited{}
#' @seealso \code{\link{CalcSurfSpawn}}
#' @examples
#' data(intensity)
#' intensity
"intensity"

#' Algae types and coefficients.
#'
#' Algae types and coefficients for Pacific Herring understory spawn surveys
#' \insertCite{Schweigert2005}{SpawnIndex}. Algae coefficients account for the
#' effect of algae morphology on Pacific Herring egg density.
#'
#' @format Tibble with 8 rows and 3 columns: algae type, algae code, and
#'   coefficient.
#' @docType data
#' @importFrom Rdpack reprompt
#' @references \insertAllCited{}
#' @seealso \code{\link{CalcUnderSpawn}}
#' @examples
#' data(algaeCoefs)
#' algaeCoefs
"algaeCoefs"

#' Example Pacific Herring spawn survey database.
#'
#' Example Pacific Herring spawn survey database. This database contains a
#' subset of tables from the spawn survey database, and some additional tables
#' from other databases to ensure the examples work.
#'
#' @format MS Access databases.
#' @docType data
#' @seealso \code{\link{LoadAreaData}} \code{\link{CalcSurfSpawn}}
#'   \code{\link{CalcMacroSpawn}} \code{\link{CalcUnderSpawn}}
#' @note This is a 32-bit MS Access database, and it requires 32-bit R to access
#'   the data. In addition, MS Windows is required to access the data using the
#'   RODBC package.
#'
system.file("extdata", "HerringSpawn.mdb",
  package = "SpawnIndex",
  mustWork = TRUE
)
