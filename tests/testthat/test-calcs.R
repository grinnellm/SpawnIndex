test_that("Egg conversion factor", {
  expect_type(calc_egg_conversion(), "double")
  expect_error(calc_egg_conversion(omega = "omega"))
  expect_message(calc_egg_conversion(omega = as.numeric(NA)))
  expect_message(calc_egg_conversion(omega = -100))
  expect_error(calc_egg_conversion(female = "female"))
  expect_message(calc_egg_conversion(female = as.numeric(NA)))
  expect_message(calc_egg_conversion(female = 1.1))
  expect_message(calc_egg_conversion(female = -0.1))
  expect_silent(calc_egg_conversion(omega = -100, female = -0.1, quiet = TRUE))
  expect_silent(calc_egg_conversion(omega = -100, female = 1.1, quiet = TRUE))
  expect_silent(calc_egg_conversion(
    omega = as.numeric(NA), female = as.numeric(NA), quiet = TRUE
  ))
})

test_that("SOK biomass", {
  expect_type(calc_biomass_sok(sok = 100), "double")
  expect_error(calc_biomass_sok(sok = "SOK"))
  expect_message(calc_biomass_sok(sok = as.numeric(NA)))
  expect_message(calc_biomass_sok(sok = -100))
  expect_error(calc_biomass_sok(sok = 100, nu = "nu"))
  expect_message(calc_biomass_sok(sok = 100, nu = as.numeric(NA)))
  expect_message(calc_biomass_sok(sok = 100, nu = 1.1))
  expect_message(calc_biomass_sok(sok = 100, nu = -0.1))
  expect_error(calc_biomass_sok(sok = 100, upsilon = "upsilon"))
  expect_message(calc_biomass_sok(sok = 100, upsilon = as.numeric(NA)))
  expect_message(calc_biomass_sok(sok = 100, upsilon = 1.1))
  expect_message(calc_biomass_sok(sok = 100, upsilon = -0.1))
  expect_error(calc_biomass_sok(sok = 100, M = "M"))
  expect_message(calc_biomass_sok(sok = 100, M = as.numeric(NA)))
  expect_message(calc_biomass_sok(sok = 100, M = -0.1))
  expect_error(calc_biomass_sok(sok = 100, theta = "theta"))
  expect_message(calc_biomass_sok(sok = 100, theta = as.numeric(NA)))
  expect_message(calc_biomass_sok(sok = 100, theta = -100))
  expect_silent(calc_biomass_sok(
    sok = -100, nu = 1.1, upsilon = 1.1, M = -0.1, theta = -100, quiet = TRUE
  ))
  expect_silent(calc_biomass_sok(
    sok = -100, nu = -0.1, upsilon = -0.1, M = -0.1, theta = -100, quiet = TRUE
  ))
  expect_silent(calc_biomass_sok(
    sok = as.numeric(NA), nu = as.numeric(NA), upsilon = as.numeric(NA),
    M = as.numeric(NA), theta = as.numeric(NA), quiet = TRUE
  ))
})
