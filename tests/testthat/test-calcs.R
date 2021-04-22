test_that("Egg conversion factor", {
  expect_type(eggs_to_sb(), "double")
  expect_error(eggs_to_sb(omega = "omega"))
  expect_message(eggs_to_sb(omega = as.numeric(NA)))
  expect_message(eggs_to_sb(omega = -100))
  expect_error(eggs_to_sb(female = "female"))
  expect_message(eggs_to_sb(female = as.numeric(NA)))
  expect_message(eggs_to_sb(female = 1.1))
  expect_message(eggs_to_sb(female = -0.1))
  expect_silent(eggs_to_sb(omega = -100, female = -0.1, quiet = TRUE))
  expect_silent(eggs_to_sb(omega = -100, female = 1.1, quiet = TRUE))
  expect_silent(eggs_to_sb(
    omega = as.numeric(NA), female = as.numeric(NA), quiet = TRUE
  ))
})

test_that("SOK biomass", {
  expect_type(calc_sok_sb(sok = 100), "double")
  expect_error(calc_sok_sb(sok = "SOK"))
  expect_message(calc_sok_sb(sok = as.numeric(NA)))
  expect_message(calc_sok_sb(sok = -100))
  expect_error(calc_sok_sb(sok = 100, nu = "nu"))
  expect_message(calc_sok_sb(sok = 100, nu = as.numeric(NA)))
  expect_message(calc_sok_sb(sok = 100, nu = 1.1))
  expect_message(calc_sok_sb(sok = 100, nu = -0.1))
  expect_error(calc_sok_sb(sok = 100, upsilon = "upsilon"))
  expect_message(calc_sok_sb(sok = 100, upsilon = as.numeric(NA)))
  expect_message(calc_sok_sb(sok = 100, upsilon = 1.1))
  expect_message(calc_sok_sb(sok = 100, upsilon = -0.1))
  expect_error(calc_sok_sb(sok = 100, egg_weight = "egg_weight"))
  expect_message(calc_sok_sb(sok = 100, egg_weight = as.numeric(NA)))
  expect_message(calc_sok_sb(sok = 100, egg_weight = -0.1))
  expect_error(calc_sok_sb(sok = 100, theta = "theta"))
  expect_message(calc_sok_sb(sok = 100, theta = as.numeric(NA)))
  expect_message(calc_sok_sb(sok = 100, theta = -100))
  expect_silent(
    calc_sok_sb(
      sok = -100, nu = 1.1, upsilon = 1.1, egg_weight = -0.1, theta = -100,
      quiet = TRUE
    )
  )
  expect_silent(
    calc_sok_sb(
      sok = -100, nu = -0.1, upsilon = -0.1, egg_weight = -0.1, theta = -100,
      quiet = TRUE
    )
  )
  expect_silent(
    calc_sok_sb(
      sok = as.numeric(NA), nu = as.numeric(NA), upsilon = as.numeric(NA),
      egg_weight = as.numeric(NA), theta = as.numeric(NA), quiet = TRUE
    )
  )
})

test_that("Surface egg density", {
  expect_type(dens_surf(egg_layers = 4), "double")
  expect_equal(dens_surf(egg_layers = 0), 0)
  expect_message(dens_surf(egg_layers = as.numeric(NA)))
  expect_message(dens_surf(egg_layers = -0.1))
  expect_error(dens_surf(egg_layers = 4, alpha = "alpha"))
  expect_message(dens_surf(egg_layers = 4, alpha = as.numeric(NA)))
  expect_error(dens_surf(egg_layers = 4, beta = "beta"))
  expect_message(dens_surf(egg_layers = 4, beta = as.numeric(NA)))
  expect_silent(dens_surf(egg_layers = -0.1, quiet = TRUE))
  expect_silent(
    dens_surf(
      egg_layers = as.numeric(NA), alpha = as.numeric(NA),
      beta = as.numeric(NA), quiet = TRUE
    )
  )
})

test_that("Macrocystis number of eggs", {
  expect_type(
    eggs_macro(egg_layers = 4, height = 3, stalks_per_plant = 2), "double"
  )
  expect_equal(eggs_macro(egg_layers = 0, height = 3, stalks_per_plant = 2), 0)
  expect_equal(eggs_macro(egg_layers = 4, height = 0, stalks_per_plant = 2), 0)
  expect_equal(eggs_macro(egg_layers = 4, height = 3, stalks_per_plant = 0), 0)
  expect_message(
    eggs_macro(
      xi = as.numeric(NA), egg_layers = 4, height = 3, stalks_per_plant = 2
    )
  )
  expect_message(
    eggs_macro(
      gamma = as.numeric(NA), egg_layers = 4, height = 3, stalks_per_plant = 2
    )
  )
  expect_message(
    eggs_macro(
      delta = as.numeric(NA), egg_layers = 4, height = 3, stalks_per_plant = 2
    )
  )
  expect_message(
    eggs_macro(
      epsilon = as.numeric(NA), egg_layers = 4, height = 3, stalks_per_plant = 2
    )
  )
  expect_message(
    eggs_macro(egg_layers = as.numeric(NA), height = 3, stalks_per_plant = 2)
  )
  expect_message(
    eggs_macro(egg_layers = -0.1, height = 3, stalks_per_plant = 2)
  )
  expect_message(
    eggs_macro(egg_layers = 4, height = as.numeric(NA), stalks_per_plant = 2)
  )
  expect_message(
    eggs_macro(egg_layers = 4, height = -0.1, stalks_per_plant = 2)
  )
  expect_message(
    eggs_macro(egg_layers = 4, height = 3, stalks_per_plant = as.numeric(NA))
  )
  expect_message(
    eggs_macro(egg_layers = 4, height = 3, stalks_per_plant = -0.1)
  )
  expect_silent(
    eggs_macro(
      egg_layers = -0.1, height = -0.1, stalks_per_plant = -0.1, quiet = TRUE
    )
  )
  expect_error(
    eggs_macro(egg_layers = 4, height = 3, stalks_per_plant = 2, xi = "xi")
  )
  expect_error(
    eggs_macro(
      egg_layers = 4, height = 3, stalks_per_plant = 2, gamma = "gamma"
    )
  )
  expect_error(
    eggs_macro(
      egg_layers = 4, height = 3, stalks_per_plant = 2, delta = "delta"
    )
  )
  expect_error(
    eggs_macro(
      egg_layers = 4, height = 3, stalks_per_plant = 2, epsilon = "epsilon"
    )
  )
  expect_silent(
    eggs_macro(
      xi = as.numeric(NA), gamma = as.numeric(NA), delta = as.numeric(NA),
      epsilon = as.numeric(NA), egg_layers = as.numeric(NA),
      height = as.numeric(NA), stalks_per_plant = as.numeric(NA), quiet = TRUE
    )
  )
})

test_that("Understory egg density on substrate", {
  expect_type(dens_under_sub(egg_layers = 4, proportion = 0.5), "double")
  expect_equal(dens_under_sub(egg_layers = 0, proportion = 0.5), 0)
  expect_equal(dens_under_sub(egg_layers = 4, proportion = 0), 0)
  expect_message(dens_under_sub(egg_layers = as.numeric(NA), proportion = 0.5))
  expect_message(dens_under_sub(egg_layers = -0.1, proportion = 0.5))
  expect_message(dens_under_sub(egg_layers = 4, proportion = as.numeric(NA)))
  expect_message(dens_under_sub(egg_layers = 4, proportion = -0.1))
  expect_message(dens_under_sub(egg_layers = 4, proportion = 1.1))
  expect_error(
    dens_under_sub(egg_layers = 4, proportion = 0.5, varphi = "varphi")
  )
  expect_message(
    dens_under_sub(egg_layers = 4, proportion = 0.5, varphi = as.numeric(NA))
  )
  expect_silent(
    dens_under_sub(egg_layers = -0.1, proportion = -0.1, quiet = TRUE)
  )
  expect_silent(
    dens_under_sub(
      egg_layers = as.numeric(NA), proportion = as.numeric(NA),
      varphi = as.numeric(NA), quiet = TRUE
    )
  )
})

test_that("Understory egg density on algae", {
  expect_type(
    dens_under_alg(egg_layers = 4, proportion = 0.5, coeff = 1.1), "double"
  )
  expect_equal(dens_under_alg(egg_layers = 0, proportion = 0.5, coeff = 1.1), 0)
  expect_equal(dens_under_alg(egg_layers = 4, proportion = 0, coeff = 1.1), 0)
  expect_equal(dens_under_alg(egg_layers = 4, proportion = 0.5, coeff = 0), 0)
  expect_message(
    dens_under_alg(egg_layers = as.numeric(NA), proportion = 0.5, coeff = 1.1)
  )
  expect_message(
    dens_under_alg(egg_layers = -0.1, proportion = 0.5, coeff = 1.1)
  )
  expect_message(
    dens_under_alg(egg_layers = 4, proportion = as.numeric(NA), coeff = 1.1)
  )
  expect_message(dens_under_alg(egg_layers = 4, proportion = -0.1, coeff = 1.1))
  expect_message(dens_under_alg(egg_layers = 4, proportion = 1.1, coeff = 1.1))
  expect_message(
    dens_under_alg(egg_layers = 4, proportion = 0.5, coeff = as.numeric(NA))
  )
  expect_message(dens_under_alg(egg_layers = 4, proportion = 0.5, coeff = -0.1))
  expect_error(
    dens_under_alg(
      egg_layers = 4, proportion = 0.5, coeff = 1.1, vartheta = "vartheta"
    )
  )
  expect_message(
    dens_under_alg(
      egg_layers = 4, proportion = 0.5, coeff = 1.1, vartheta = as.numeric(NA)
    )
  )
  expect_error(
    dens_under_alg(
      egg_layers = 4, proportion = 0.5, coeff = 1.1, varrho = "varrho"
    )
  )
  expect_message(
    dens_under_alg(
      egg_layers = 4, proportion = 0.5, coeff = 1.1, varrho = as.numeric(NA)
    )
  )
  expect_error(
    dens_under_alg(
      egg_layers = 4, proportion = 0.5, coeff = 1.1, varsigma = "varsigma"
    )
  )
  expect_message(
    dens_under_alg(
      egg_layers = 4, proportion = 0.5, coeff = 1.1, varsigma = as.numeric(NA)
    )
  )
  expect_silent(
    dens_under_alg(
      egg_layers = -0.1, proportion = -0.1, coeff = -0.1, quiet = TRUE
    )
  )
  expect_silent(
    dens_under_alg(
      egg_layers = as.numeric(NA), proportion = as.numeric(NA),
      vartheta = as.numeric(NA), varrho = as.numeric(NA),
      varsigma = as.numeric(NA), coeff = as.numeric(NA), quiet = TRUE
    )
  )
})
