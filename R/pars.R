pars <- list(
  conversion=list(omega=200000, phi=0.5),
  SOK=list(nu=0.88, upsilon=1/1.13, M=2.38*10^-6),
  surface=list(alpha=14.698, beta=212.218),
  macrocystis=list(beta=0.073, gamma=0.673, delta=0.932, epsilon=0.703),
  understory=list(alpha=340, beta=600.567, gamma=0.6355, delta=1.413)
)
save( pars, file=file.path("data", "pars.RData") )
