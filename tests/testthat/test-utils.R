test_that("Paste nicely", {
  expect_error(paste_nicely(x = letters[1:3], do_quotes = "Yes"))
  expect_error(paste_nicely(x = letters[1:3], n_char = 3))
  expect_type(paste_nicely(x = letters[1:3]), "character")
  expect_equal(paste_nicely(x = letters[1:2]), "a and b")
  expect_equal(paste_nicely(x = letters[1:3]), "a, b, and c")
  expect_equal(
    paste_nicely(x = letters[1:3], do_quotes = TRUE), "`a`, `b`, and `c`"
  )
  expect_equal(
    paste_nicely(x = letters[1:2], do_quotes = TRUE), "`a` and `b`"
  )
})
