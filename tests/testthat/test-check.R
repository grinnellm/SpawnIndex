test_that("Check numeric", {
  expect_error(check_numeric(dat = as.numeric(NA)))
  expect_message(check_numeric(dat = list(x = as.numeric(NA))))
  expect_silent(check_numeric(dat = list(x = as.numeric(NA)), quiet = TRUE))
  expect_error(check_numeric(dat = list(x = "x")))
  expect_silent(check_numeric(dat = list(x = 1:5)))
})

test_that("Check tibble", {
  expect_error(check_tibble(dat = 1:4))
  expect_message(check_tibble(dat = list(x = tibble::tibble())))
  expect_silent(check_tibble(dat = list(x = tibble::tibble()), quiet = TRUE))
  expect_error(check_tibble(dat = list(x = data.frame(x = 1))))
  expect_silent(check_tibble(dat = list(x = tibble::tibble(x = 1))))
})
