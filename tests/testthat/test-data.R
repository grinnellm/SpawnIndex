context("data.R")

test_that("Understory width factors", {
  expect_type(underWidthFac, "list")
  expect_equal(dim(underWidthFac), c(12, 8))
  expect_named(
    underWidthFac,
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
  expect_type(algaeCoefs, "list")
  expect_equal(dim(algaeCoefs), c(8, 3))
  expect_named(algaeCoefs, c("AlgaeName", "AlgType", "Coef"))
})
