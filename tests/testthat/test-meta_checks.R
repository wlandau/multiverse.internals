test_that("meta_checks()", {
  out <- meta_checks(repo = "https://wlandau.r-universe.dev")
  expect_true(is.data.frame(out))
  expect_gt(nrow(out), 1L)
  fields <- c(
    "_status",
    "_winbinary",
    "_macbinary",
    "_wasmbinary",
    "_linuxdevel",
    "_buildurl"
  )
  expect_true(all(fields %in% colnames(out)))
})
