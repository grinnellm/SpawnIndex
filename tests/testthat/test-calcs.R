context("calcs.R")

test_that("egg conversion factor", {
  expect_equal(
    CalcEggConversion(),
    1e+08)
})
