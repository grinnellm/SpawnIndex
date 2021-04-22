test_that("Understory width factors", {
  expect_type(under_width_facs, "list")
  expect_equal(dim(under_width_facs), c(12, 8))
  expect_named(
    under_width_facs,
    c("Year", "HG", "PRD", "CC", "SoG", "WCVI", "A27", "A2W")
  )
  expect_setequal(under_width_facs$Year, 2003:2014)
  expect_setequal(
    unique(unlist(under_width_facs[, 2:8])),
    c(1.000, 1.075, 1.150)
  )
})

test_that("Parameters", {
  expect_type(pars, "list")
  expect_named(
    pars,
    c("conversion", "sok", "surface", "macrocystis", "understory", "years")
  )
  expect_type(pars$conversion, "list")
  expect_named(pars$conversion, c("omega", "female"))
  expect_setequal(pars$conversion, c(2e+05, 0.5))
  expect_type(pars$sok, "list")
  expect_named(pars$sok, c("nu", "upsilon", "egg_weight"))
  expect_setequal(pars$sok, c(0.12, 0.132, 2.38e-06))
  expect_type(pars$surface, "list")
  expect_named(pars$surface, c("alpha", "beta"))
  expect_setequal(pars$surface, c(14.698, 212.218))
  expect_type(pars$macrocystis, "list")
  expect_named(pars$macrocystis, c("xi", "gamma", "delta", "epsilon"))
  expect_setequal(pars$macrocystis, c(0.073, 0.673, 0.932, 0.703))
  expect_type(pars$understory, "list")
  expect_named(pars$understory, c("varphi", "vartheta", "varrho", "varsigma"))
  expect_setequal(pars$understory, c(340, 600.567, 0.6355, 1.413))
  expect_type(pars$years, "list")
  expect_named(
    pars$years, c("survey", "assess", "nine_cats", "layers", "dive")
  )
  expect_setequal(pars$years, c(1928, 1951, 1969, 1979, 1988))
})

test_that("Intensity", {
  expect_type(intensity, "list")
  expect_equal(dim(intensity), c(9, 3))
  expect_named(intensity, c("Intensity", "Description", "Layers"))
  expect_setequal(intensity$Intensity, 1:9)
  expect_setequal(
    intensity$Layers,
    c(0.5529, 0.9444, 1.3360, 2.1496, 2.9633, 4.1318, 5.3002, 6.5647, 7.8291)
  )
})

test_that("Algae coefficients", {
  expect_type(algae_coefs, "list")
  expect_equal(dim(algae_coefs), c(8, 3))
  expect_named(algae_coefs, c("AlgaeName", "AlgType", "Coef"))
  expect_setequal(
    algae_coefs$AlgType,
    c("GR", "GG", "KF", "KS", "LA", "RW", "SM", "SA")
  )
  expect_setequal(
    algae_coefs$Coef,
    c(0.9715, 1.000, 0.9119, 1.1766, 0.6553, 0.7793)
  )
})
