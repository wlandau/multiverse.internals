test_that("r_version_staging()", {
  out <- r_version_staging()
  expect_true(is.data.frame(out))
  expect_equal(nrow(out), 1L)
  expect_false(anyNA(out))
  expect_equal(
    sort(c(colnames(out))),
    sort(c("date", "full", "short", "major", "minor", "patch"))
  )
})
