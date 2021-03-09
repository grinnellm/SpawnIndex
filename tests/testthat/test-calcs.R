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
  expect_error(calc_biomass_sok(sok = 100, w = "w"))
  expect_message(calc_biomass_sok(sok = 100, w = as.numeric(NA)))
  expect_message(calc_biomass_sok(sok = 100, w = -0.1))
  expect_error(calc_biomass_sok(sok = 100, theta = "theta"))
  expect_message(calc_biomass_sok(sok = 100, theta = as.numeric(NA)))
  expect_message(calc_biomass_sok(sok = 100, theta = -100))
  expect_silent(
    calc_biomass_sok(
      sok = -100, nu = 1.1, upsilon = 1.1, w = -0.1, theta = -100, quiet = TRUE
    )
  )
  expect_silent(
    calc_biomass_sok(
      sok = -100, nu = -0.1, upsilon = -0.1, w = -0.1, theta = -100,
      quiet = TRUE
    )
  )
  expect_silent(
    calc_biomass_sok(
      sok = as.numeric(NA), nu = as.numeric(NA), upsilon = as.numeric(NA),
      w = as.numeric(NA), theta = as.numeric(NA), quiet = TRUE
    )
  )
})

test_that("Surface egg density", {
  expect_type(egg_dens_surf(egg_layers = 4), "double")
  expect_message(egg_dens_surf(egg_layers = as.numeric(NA)))
  expect_message(egg_dens_surf(egg_layers = 0))
  expect_error(egg_dens_surf(egg_layers = 4, alpha = "alpha"))
  expect_message(egg_dens_surf(egg_layers = 4, alpha = as.numeric(NA)))
  expect_error(egg_dens_surf(egg_layers = 4, beta = "beta"))
  expect_message(egg_dens_surf(egg_layers = 4, beta = as.numeric(NA)))
  expect_silent(egg_dens_surf(egg_layers = 0, quiet = TRUE))
  expect_silent(
    egg_dens_surf(
      egg_layers = as.numeric(NA), alpha = as.numeric(NA),
      beta = as.numeric(NA), quiet = TRUE
    )
  )
})

test_that("Macrocystis number of eggs", {
  expect_type(
    num_eggs_macro(egg_layers = 4, height = 3, stalks_per_plant = 2), "double"
  )
  expect_message(
    num_eggs_macro(
      xi = as.numeric(NA), egg_layers = 4, height = 3, stalks_per_plant = 2
    )
  )
  expect_message(
    num_eggs_macro(
      gamma = as.numeric(NA), egg_layers = 4, height = 3, stalks_per_plant = 2
    )
  )
  expect_message(
    num_eggs_macro(
      delta = as.numeric(NA), egg_layers = 4, height = 3, stalks_per_plant = 2
    )
  )
  expect_message(
    num_eggs_macro(
      epsilon = as.numeric(NA), egg_layers = 4, height = 3, stalks_per_plant = 2
    )
  )
  expect_message(
    num_eggs_macro(
      egg_layers = as.numeric(NA), height = 3, stalks_per_plant = 2
    )
  )
  expect_message(
    num_eggs_macro(egg_layers = 0, height = 3, stalks_per_plant = 2)
  )
  expect_message(
    num_eggs_macro(
      egg_layers = 4, height = as.numeric(NA), stalks_per_plant = 2
    )
  )
  expect_message(
    num_eggs_macro(egg_layers = 4, height = 0, stalks_per_plant = 2)
  )
  expect_message(
    num_eggs_macro(
      egg_layers = 4, height = 3, stalks_per_plant = as.numeric(NA)
    )
  )
  expect_message(
    num_eggs_macro(egg_layers = 4, height = 3, stalks_per_plant = 0)
  )
  expect_silent(
    num_eggs_macro(
      egg_layers = 0, height = 0, stalks_per_plant = 0, quiet = TRUE
    )
  )
  expect_error(
    num_eggs_macro(egg_layers = 4, height = 3, stalks_per_plant = 2, xi = "xi")
  )
  expect_error(
    num_eggs_macro(
      egg_layers = 4, height = 3, stalks_per_plant = 2, gamma = "gamma"
    )
  )
  expect_error(
    num_eggs_macro(
      egg_layers = 4, height = 3, stalks_per_plant = 2, delta = "delta"
    )
  )
  expect_error(
    num_eggs_macro(
      egg_layers = 4, height = 3, stalks_per_plant = 2, epsilon = "epsilon"
    )
  )
  expect_silent(
    num_eggs_macro(
      xi = as.numeric(NA), gamma = as.numeric(NA), delta = as.numeric(NA),
      epsilon = as.numeric(NA), egg_layers = as.numeric(NA),
      height = as.numeric(NA), stalks_per_plant = as.numeric(NA), quiet = TRUE
    )
  )
})

test_that("Understory egg density on substrate", {
  expect_type(egg_dens_under_sub(sub_layers = 4, sub_prop = 0.5), "double")
  expect_message(
    egg_dens_under_sub(sub_layers = as.numeric(NA), sub_prop = 0.5)
  )
  expect_message(egg_dens_under_sub(sub_layers = 0, sub_prop = 0.5))
  expect_message(egg_dens_under_sub(sub_layers = 4, sub_prop = as.numeric(NA)))
  expect_message(egg_dens_under_sub(sub_layers = 4, sub_prop = 0))
  expect_message(egg_dens_under_sub(sub_layers = 4, sub_prop = 1.1))
  expect_error(
    egg_dens_under_sub(sub_layers = 4, sub_prop = 0.5, varphi = "varphi")
  )
  expect_message(
    egg_dens_under_sub(sub_layers = 4, sub_prop = 0.5, varphi = as.numeric(NA))
  )
  expect_silent(egg_dens_under_sub(sub_layers = 0, sub_prop = 0, quiet = TRUE))
  expect_silent(
    egg_dens_under_sub(
      sub_layers = as.numeric(NA), sub_prop = as.numeric(NA),
      varphi = as.numeric(NA), quiet = TRUE
    )
  )
})

test_that("Understory egg density on algae", {
  expect_type(
    egg_dens_under_alg(alg_layers = 4, alg_prop = 0.5, coeff = 1.1),
    "double"
  )
  expect_message(
    egg_dens_under_alg(alg_layers = as.numeric(NA), alg_prop = 0.5, coeff = 1.1)
  )
  expect_message(
    egg_dens_under_alg(alg_layers = 0, alg_prop = 0.5, coeff = 1.1)
  )
  expect_message(
    egg_dens_under_alg(alg_layers = 4, alg_prop = as.numeric(NA), coeff = 1.1)
  )
  expect_message(egg_dens_under_alg(alg_layers = 4, alg_prop = 0, coeff = 1.1))
  expect_message(
    egg_dens_under_alg(alg_layers = 4, alg_prop = 1.1, coeff = 1.1)
  )
  expect_message(
    egg_dens_under_alg(alg_layers = 4, alg_prop = 0.5, coeff = as.numeric(NA))
  )
  expect_message(
    egg_dens_under_alg(alg_layers = 4, alg_prop = 0.5, coeff = -0.1)
  )
  expect_error(
    egg_dens_under_alg(
      alg_layers = 4, alg_prop = 0.5, coeff = 1.1, vartheta = "vartheta"
    )
  )
  expect_message(
    egg_dens_under_alg(
      alg_layers = 4, alg_prop = 0.5, coeff = 1.1, vartheta = as.numeric(NA)
    )
  )
  expect_error(
    egg_dens_under_alg(
      alg_layers = 4, alg_prop = 0.5, coeff = 1.1, varrho = "varrho"
    )
  )
  expect_message(
    egg_dens_under_alg(
      alg_layers = 4, alg_prop = 0.5, coeff = 1.1, varrho = as.numeric(NA)
    )
  )
  expect_error(
    egg_dens_under_alg(
      alg_layers = 4, alg_prop = 0.5, coeff = 1.1, varsigma = "varsigma"
    )
  )
  expect_message(
    egg_dens_under_alg(
      alg_layers = 4, alg_prop = 0.5, coeff = 1.1, varsigma = as.numeric(NA)
    )
  )
  expect_silent(
    egg_dens_under_alg(alg_layers = 0, alg_prop = 0, coeff = -0.1, quiet = TRUE)
  )
  expect_silent(
    egg_dens_under_alg(
      alg_layers = as.numeric(NA), alg_prop = as.numeric(NA),
      vartheta = as.numeric(NA), varrho = as.numeric(NA),
      varsigma = as.numeric(NA), coeff = as.numeric(NA), quiet = TRUE
    )
  )
})
