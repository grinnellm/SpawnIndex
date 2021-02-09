test_that("Understory width factors", {
  expect_type(under_width_facs, "list")
  expect_equal(dim(under_width_facs), c(12, 8))
  expect_named(
    under_width_facs,
    c("Year", "HG", "PRD", "CC", "SoG", "WCVI", "A27", "A2W")
  )
})

test_that("Parameters", {
  expect_type(pars, "list")
  expect_named(
    pars,
    c("conversion", "SOK", "surface", "macrocystis", "understory")
  )
  expect_type(pars$conversion, "list")
  expect_named(pars$conversion, c("omega", "female"))
  expect_type(pars$SOK, "list")
  expect_named(pars$SOK, c("nu", "upsilon", "M"))
  expect_type(pars$surface, "list")
  expect_named(pars$surface, c("alpha", "beta"))
  expect_type(pars$macrocystis, "list")
  expect_named(pars$macrocystis, c("xi", "gamma", "delta", "epsilon"))
  expect_type(pars$understory, "list")
  expect_named(pars$understory, c("varphi", "vartheta", "varrho", "varsigma"))
})

test_that("Intensity", {
  expect_type(intensity, "list")
  expect_equal(dim(intensity), c(9, 3))
  expect_named(intensity, c("Intensity", "Description", "Layers"))
})

test_that("Algae coefficients", {
  expect_type(algae_coefs, "list")
  expect_equal(dim(algae_coefs), c(8, 3))
  expect_named(algae_coefs, c("AlgaeName", "AlgType", "Coef"))
})
