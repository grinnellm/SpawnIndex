# R packages
library(tibble)
library(readr)

# Parameter values
pars <- list(
  conversion = list(omega = 200000, female = 0.5),
  SOK = list(nu = 0.12, upsilon = (19.73 - 6.53) / 100, M = 2.38 * 10^-6),
  surface = list(alpha = 14.698, beta = 212.218),
  macrocystis = list(
    xi = 0.073, gamma = 0.673, delta = 0.932, epsilon = 0.703
  ),
  understory = list(
    varphi = 340, vartheta = 600.567, varrho = 0.6355, varsigma = 1.413
  )
)
save(pars, file = file.path("data", "pars.RData"))

# Intensity categories
intensity <- tibble(
  Intensity = 1:9,
  Description = c(
    "Very light", NA, "Light", NA, "Medium", NA, "Heavy", NA, "Very heavy"
  ),
  Layers = c(
    0.5529, 0.9444, 1.3360, 2.1496, 2.9633, 4.1318, 5.3002, 6.5647, 7.8291
  )
)
save(intensity, file = file.path("data", "intensity.RData"))

# Algae coefficients
algaeCoefs <- tibble(
  AlgaeName = c(
    "Grasses", "Grunge", "Kelp (flat)", "Kelp (standing)", "Leafy algae",
    "Rockweed", "Sargassum", "Stringy algae"
  ),
  AlgType = c("GR", "GG", "KF", "KS", "LA", "RW", "SM", "SA"),
  Coef = c(0.9715, 1.0000, 0.9119, 1.1766, 0.6553, 0.7793, 1.1766, 1.0000)
)
save(algaeCoefs, file = file.path("data", "algaeCoefs.RData"))

# Understory spawn width correction factors
underWidthFac <- read_csv(
  "Year, HG, PRD, CC, SoG, WCVI, A27, A2W
   2003, 1, 1.075, 1.075, 1.075, 1.075, 1.075, 1
   2004, 1, 1.075, 1.075, 1.075, 1.075, 1.075, 1
   2005, 1, 1.075, 1.075, 1.075, 1.075, 1.075, 1
   2006, 1, 1.075, 1.075, 1.075, 1.075, 1.075, 1
   2007, 1, 1.075, 1.075, 1.075, 1.075, 1.075, 1
   2008, 1, 1.075, 1.075, 1.075, 1.075, 1.075, 1
   2009, 1.15, 1.075, 1.075, 1.075, 1.075, 1.075, 1.15
   2010, 1.15, 1.075, 1.075, 1.075, 1.075, 1.075, 1.15
   2011, 1.15, 1.075, 1.075, 1.075, 1.075, 1.075, 1.15
   2012, 1.15, 1.075, 1.075, 1.075, 1.075, 1.075, 1.15
   2013, 1.15, 1.15, 1.075, 1.075, 1.075, 1, 1.15
   2014, 1.15, 1.15, 1, 1, 1, 1, 1.15"
)
save(underWidthFac, file = file.path("data", "underWidthFac.RData"))
