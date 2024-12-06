test_that("meta_checks()", {
  out <- meta_checks(repo = "https://wlandau.r-universe.dev")
  expect_true(is.data.frame(out))
  expect_gt(nrow(out), 1L)
  fields <- c("package", "url", "issues")
  expect_true(all(fields %in% colnames(out)))
})
