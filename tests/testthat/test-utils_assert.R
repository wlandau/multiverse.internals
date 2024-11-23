test_that("assert_file()", {
  temp <- tempfile()
  on.exit(unlink(temp))
  expect_error(assert_file(temp))
  file.create(temp)
  expect_silent(assert_file(temp))
})

test_that("assert_character_scalar()", {
  expect_silent(assert_character_scalar("x"))
  expect_error(assert_character_scalar(1))
  expect_error(assert_character_scalar(NULL))
  expect_error(assert_character_scalar(letters))
  expect_error(assert_character_scalar(character(0L)))
})

test_that("assert_positive_scalar()", {
  expect_silent(assert_positive_scalar(1))
  expect_error(assert_positive_scalar(-1))
  expect_error(assert_positive_scalar(NULL))
  expect_error(assert_positive_scalar(letters))
  expect_error(assert_positive_scalar(character(0L)))
})
