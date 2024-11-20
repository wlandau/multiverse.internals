test_that("list_organizations", {
  skip_if_offline()
  out <- list_organizations(
    owner = "r-multiverse",
    repo = "contributions"
  )
  expect_true(is.character(out))
  expect_gt(length(out), 0L)
  expect_false(anyNA(out))
  expect_true(all(nzchar(out)))
  expect_true("r-multiverse" %in% out)
})
