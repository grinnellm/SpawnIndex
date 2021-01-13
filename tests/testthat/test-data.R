context("data.R")

test_that("Understory width factors", {
  expect_equal(
    dim(underWidthFac),
    c(12, 8)
  )
})

test_that("Understory width factor names", {
  expect_equal(
    names(underWidthFac),
    c("Year", "HG", "PRD", "CC", "SoG", "WCVI", "A27", "A2W")
  )
})

test_that("Parameters is a list", {
  expect_equal(
    is.list(pars),
    TRUE
  )
})

test_that("Parameters has names", {
  expect_equal(
    names(pars),
    c("conversion", "SOK", "surface", "macrocystis", "understory" )
  )
})

test_that("Intensity", {
  expect_equal(
    dim(intensity),
    c(9, 3)
  )
})

test_that("Intensity names", {
  expect_equal(
    names(intensity),
    c("Intensity", "Description", "Layers" )
  )
})

test_that("Algae coefficients", {
  expect_equal(
    dim(algaeCoefs),
    c(8, 3)
  )
})

test_that("Algae coefficient names", {
  expect_equal(
    names(algaeCoefs),
    c("AlgaeName", "AlgType", "Coef")
  )
})
