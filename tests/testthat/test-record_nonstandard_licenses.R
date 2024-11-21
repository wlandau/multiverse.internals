test_that("record_nonstandard_licenses()", {
  skip_if_offline()
  temp <- tempfile()
  on.exit(unlink(temp))
  record_nonstandard_licenses(
    repo = "https://wlandau.r-universe.dev",
    path = temp
  )
  expect_true(file.exists(temp))
})
