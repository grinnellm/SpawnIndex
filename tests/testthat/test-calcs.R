test_that("Egg conversion factor", {
  expect_type(calc_egg_conversion(), "double")
  expect_equal(calc_egg_conversion(), 1e+08)
})

test_that("SOK biomass", {
  expect_type(calc_biomass_sok(SOK = 100), "double")
  expect_equal(calc_biomass_sok(SOK = 100), 0.32, 0.01)
})
