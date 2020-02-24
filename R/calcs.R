#' Convert number of eggs to spawn index in tonnes.
#'
#' Calculate the conversion factor for Pacific Herring, to convert the number of
#' eggs to spawn index (i.e., biomass) in tonnes.
#'
#' @param fecundity The number of eggs per kilogram of female spawners (default
#' 200000).
#' @param pFemale The proportion of spawners that are female (default 0.5).
#'
#' @importFrom Rdpack reprompt
#'
#' @return Numeric. The conversion factor for eggs to spawn index in tonnes
#' (i.e., biomass). Divide the number of eggs by the conversion factor to get
#' biomass.
#'
#' @references
#' \insertRef{Hay1985}{SpawnIndex}
#'
#' \insertRef{HayBrett1988}{SpawnIndex}
#'
CalcEggConversion <- function( fecundity=200000, pFemale=0.5 ) {
  # Eggs per tonne: eggs/kilogram female * proportion female * kilograms/tonne
  eggsPerTonne <- fecundity * pFemale * 1000
  # Return the conversion factor
  return( eggsPerTonne )
}  # End CalcEggConversion function

