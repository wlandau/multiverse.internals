test_that("read_advisories()", {
  advisories <- read_advisories()
  expect_true(is.data.frame(advisories))
  expect_equal(
    sort(colnames(advisories)),
    sort(c("package", "version", "advisories"))
  )
  expect_gt(nrow(advisories), 0L)
})
