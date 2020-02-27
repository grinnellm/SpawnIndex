# pars <- list(
#   conversion=list(omega=200000, phi=0.5),
#   SOK=list(nu=0.88, upsilon=1/1.13, M=2.38*10^-6),
#   surface=list(alpha=14.698, beta=212.218),
#   macrocystis=list(beta=0.073, gamma=0.673, delta=0.932, epsilon=0.703),
#   understory=list(alpha=340, beta=600.567, gamma=0.6355, delta=1.413)
# )
# save( pars, file=file.path("data", "pars.RData") )

# intensity <- tibble(
#   Category=1:9,
#   Description=c("Very light", NA, "Light", NA, "Medium", NA, "Heavy", NA,
#                 "Very heavy"),
#   Layers=c(0.5529, 0.9444, 1.3360, 2.1496, 2.9633, 4.1318, 5.3002, 6.5647,
#            7.8291)
# )
# save( intensity, file=file.path("data", "intensity.RData"))

# algaeCoefs <- tibble(
#   AlgaeType=c("Grasses", "Grunge", "Kelp (flat)", "Kelp (standing", "Leafy algae",
#           "Rockweed", "Sargassum", "Stringy algae"),
#   AlgaeCode=c("GR", "GG", "KF", "KS", "LA", "RW", "SM", "SA"),
#   Coefficient=c(0.9715, 1.0000, 0.9119, 1.1766, 0.6553, 0.7793, 1.1766, 1.0000 )
# )
# save( algaeCoefs, file=file.path("data", "algaeCoefs.RData"))
