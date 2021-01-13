context("calcs.R")

test_that("Egg conversion factor", {
  expect_equal(
    is.numeric(CalcEggConversion()),
    TRUE
  )
})

test_that("SOK biomass", {
  expect_equal(
    is.numeric(CalcBiomassSOK(SOK = 100)),
    TRUE
  )
})
