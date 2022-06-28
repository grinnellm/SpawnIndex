test_that("Paste nicely", {
  expect_error(paste_nicely(x = letters[1:3], do_quotes = "Yes"))
  expect_error(paste_nicely(x = letters[1:3], n_char = 3))
  expect_type(paste_nicely(x = letters[1:3]), "character")
  expect_equal(paste_nicely(x = letters[1:2]), "a and b")
  expect_equal(paste_nicely(x = letters[1:3]), "a, b, and c")
  expect_equal(
    paste_nicely(x = letters[1:3], do_quotes = TRUE), "`a`, `b`, and `c`"
  )
  expect_equal(paste_nicely(x = letters[1:2], do_quotes = TRUE), "`a` and `b`")
})

test_that("Min NA", {
  expect_equal(min_na(x = NA), NA)
  expect_equal(min_na(x = c(1:3, NA)), 1)
  expect_true(is.na(min_na(x = c(1:3, NA), omit_na = FALSE)))
})

test_that("Max NA", {
  expect_equal(max_na(x = NA), NA)
  expect_equal(max_na(x = c(1:3, NA)), 3)
  expect_true(is.na(max_na(x = c(1:3, NA), omit_na = FALSE)))
})

test_that("Sum NA", {
  expect_equal(sum_na(x = NA), NA)
  expect_equal(sum_na(x = c(1:3, NA)), 6)
  expect_true(is.na(sum_na(x = c(1:3, NA), omit_na = FALSE)))
})

test_that("Mean NA", {
  expect_equal(mean_na(x = NA), NA)
  expect_equal(mean_na(x = c(1:3, NA)), 2)
  expect_true(is.na(mean_na(x = c(1:3, NA), omit_na = FALSE)))
})

test_that("Weighted mean NA", {
  expect_equal(wt_mean_na(x = NA), NA)
  expect_equal(wt_mean_na(x = c(1:3, NA)), 2)
  expect_equal(wt_mean_na(x = c(1:3, NA), w = c(1, 1, 2, NA)), 2.25)
  expect_true(is.na(wt_mean_na(x = c(1:3, NA), omit_na = FALSE)))
})

test_that("Rolling mean NA", {
  expect_equal(roll_mean_na(dat = c(NA, NA), n = 2), c(NA, NA))
  expect_equal(roll_mean_na(dat = c(1:3, NA), n = 2), c(1.0, 2.0, 3.0, 2.5))
})

test_that("Unique NA", {
  expect_equal(unique_na(x = NA), NA)
  expect_equal(unique_na(x = c(1:3, NA)), c(1:3, NA))
})
