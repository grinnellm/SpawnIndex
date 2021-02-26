test_that("Check numeric", {
  expect_error(check_numeric(dat = as.numeric(NA)))
  expect_message(check_numeric(dat = list(x = as.numeric(NA))))
  expect_error(check_numeric(dat = list(x = "x")))
})

test_that("Check tibble", {
  expect_error(check_tibble(dat = tibble))
  expect_message(check_tibble(dat = list(x = tibble())))
  expect_error(check_tibble(dat = list(x = data.frame(x = 1))))
})
