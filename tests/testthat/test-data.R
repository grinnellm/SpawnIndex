context("data.R")

test_that("Understory width factors", {
  expect_is(underWidthFac, "data.frame")
  expect_equal(dim(underWidthFac), c(12, 8))
  expect_named(
    underWidthFac,
    c("Year", "HG", "PRD", "CC", "SoG", "WCVI", "A27", "A2W")
  )
})

test_that("Parameters", {
  expect_is(pars, "list")
  expect_named(
    pars,
    c("conversion", "SOK", "surface", "macrocystis", "understory")
  )
  expect_named(pars$conversion, c("omega", "female"))
  expect_named(pars$SOK, c("nu", "upsilon", "M"))
  expect_named(pars$surface, c("alpha", "beta"))
  expect_named(pars$macrocystis, c("xi", "gamma", "delta", "epsilon"))
  expect_named(pars$understory, c("varphi", "vartheta", "varrho", "varsigma"))
})

test_that("Intensity", {
  expect_is(intensity, "data.frame")
  expect_equal(dim(intensity), c(9, 3))
  expect_named(intensity, c("Intensity", "Description", "Layers"))
})

test_that("Algae coefficients", {
  expect_is(algaeCoefs, "data.frame")
  expect_equal(dim(algaeCoefs), c(8, 3))
  expect_named(algaeCoefs, c("AlgaeName", "AlgType", "Coef"))
})
