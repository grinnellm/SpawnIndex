context("calcs.R")

test_that("Egg conversion factor", {
  expect_equal(
    CalcEggConversion(),
    1e+08
  )
})

test_that("SOK biomass", {
  expect_equal(
    CalcBiomassSOK(SOK = 100),
    0.3266324,
    tolerance = 0.001
  )
})
