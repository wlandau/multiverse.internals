test_that("issues_checks() mocked", {
  issues <- issues_checks(meta = mock_meta_checks)
  url <- "https://github.com/r-universe/r-multiverse/actions/runs"
  expected <- list(
    httpgd = list(
      "_linuxdevel" = "success", "_macbinary" = "success",
      "_wasmbinary" = "none", "_winbinary" = "success", "_status" = "success",
      "_buildurl" = file.path(url, "9403635056")
    ),
    INLA = list(
      "_linuxdevel" = "src-failure", "_macbinary" = "src-failure",
      "_wasmbinary" = "src-failure", "_winbinary" = "src-failure",
      "_status" = "src-failure",
      "_buildurl" = file.path(url, "9296256187")
    ),
    polars = list(
      "_linuxdevel" = "failure", "_macbinary" = "arm64-failure",
      "_wasmbinary" = "none", "_winbinary" = "success", "_status" = "success",
      "_buildurl" = file.path(url, "9360739181")
    ),
    SBC = list(
      "_linuxdevel" = "failure", "_macbinary" = "success",
      "_wasmbinary" = "success", "_winbinary" = "success",
      "_status" = "failure",
      "_buildurl" = file.path(url, "9412009979")
    ),
    stantargets = list(
      "_linuxdevel" = "failure", "_macbinary" = "success",
      "_wasmbinary" = "success", "_winbinary" = "success",
      "_status" = "success",
      "_buildurl" = file.path(url, "9412009826")
    ),
    tidytensor = list(
      "_linuxdevel" = "failure", "_macbinary" = "success",
      "_wasmbinary" = "success", "_winbinary" = "success",
      "_status" = "failure",
      "_buildurl" = file.path(url, "9412009544")
    )
  )
  expect_equal(issues[order(names(issues))], expected[order(names(expected))])
})

test_that("issues_checks() on a small repo", {
  meta <- meta_checks(repo = "https://wlandau.r-universe.dev")
  issues <- issues_checks(meta = meta)
  expect_true(is.list(issues))
})
