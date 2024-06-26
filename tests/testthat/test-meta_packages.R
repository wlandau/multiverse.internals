test_that("meta_packages()", {
  out <- meta_packages(repo = "https://wlandau.r-universe.dev")
  expect_true(is.data.frame(out))
  expect_gt(nrow(out), 1L)
  fields <- c(
    "package",
    "version",
    "remotesha"
  )
  expect_true(all(fields %in% colnames(out)))
})
