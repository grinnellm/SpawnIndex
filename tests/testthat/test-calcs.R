context("calcs.R")

test_that("Egg conversion factor", {
  expect_is(CalcEggConversion(), "numeric")
  expect_equal(CalcEggConversion(), 1e+08)
})

test_that("SOK biomass", {
  expect_is(CalcBiomassSOK(SOK = 100), "numeric")
  expect_equal(CalcBiomassSOK(SOK = 100), 0.32, 0.01)
})
