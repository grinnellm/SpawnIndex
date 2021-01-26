test_that("Egg conversion factor", {
  expect_type(CalcEggConversion(), "double")
  expect_equal(CalcEggConversion(), 1e+08)
})

test_that("SOK biomass", {
  expect_type(CalcBiomassSOK(SOK = 100), "double")
  expect_equal(CalcBiomassSOK(SOK = 100), 0.32, 0.01)
})
