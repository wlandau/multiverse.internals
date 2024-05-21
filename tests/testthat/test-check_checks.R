test_that("check_checks() mocked", {
  issues <- check_checks(mock = mock_checks)
  url <- "https://github.com/r-universe/r-multiverse/actions/runs"
  expected <- list(
    httpgd = list(
      "_linuxdevel" = "success", "_macbinary" = "success", 
      "_wasmbinary" = "none", "_winbinary" = "success", "_status" = "success", 
      "_buildurl" = file.path(url, "8998732459")
    ), 
    INLA = list(
      "_linuxdevel" = "src-failure", "_macbinary" = "src-failure", 
      "_wasmbinary" = "src-failure", "_winbinary" = "src-failure", 
      "_status" = "src-failure",
      "_buildurl" = file.path(url, "8487512222")
    ), 
    polars = list(
      "_linuxdevel" = "failure", "_macbinary" = "arm64-failure", 
      "_wasmbinary" = "none", "_winbinary" = "success", "_status" = "success", 
      "_buildurl" = file.path(url, "9005231218")
    ), 
    SBC = list(
      "_linuxdevel" = "failure", "_macbinary" = "success",
      "_wasmbinary" = "success", "_winbinary" = "success",
      "_status" = "failure",
      "_buildurl" = file.path(url, "8998731731")
    ), 
    stantargets = list(
      "_linuxdevel" = "failure", "_macbinary" = "success", 
      "_wasmbinary" = "success", "_winbinary" = "success", 
      "_status" = "success",
      "_buildurl" = file.path(url, "8998732490")
    ), 
    string2path = list(
      "_linuxdevel" = "success", "_macbinary" = "success", 
      "_wasmbinary" = "none", "_winbinary" = "success", "_status" = "success", 
      "_buildurl" = file.path(url, "8998732437")
    ), 
    tidytensor = list(
      "_linuxdevel" = "failure", "_macbinary" = "success", 
      "_wasmbinary" = "success", "_winbinary" = "success", 
      "_status" = "failure",
      "_buildurl" = file.path(url, "8998732025")
    )
  )
  expect_equal(issues[order(names(issues))], expected[order(names(expected))])
})

test_that("check_checks() on a small repo", {
  issues <- check_checks(repo = "https://wlandau.r-universe.dev")
  expect_true(is.list(issues))
  expect_named(issues)
})
