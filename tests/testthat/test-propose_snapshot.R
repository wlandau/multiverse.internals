test_that("propose_snapshot()", {
  dir_staging <- tempfile()
  path_staging <- file.path(dir_staging, "staging")
  dir.create(dir_staging)
  on.exit(unlink(path_staging, recursive = TRUE))
  mock <- system.file(
    "mock",
    package = "multiverse.internals",
    mustWork = TRUE
  )
  file.copy(
    from = file.path(mock, "staging"),
    to = dir_staging,
    recursive = TRUE
  )
  propose_snapshot(path_staging = path_staging)
  expect_equal(
    readLines(file.path(path_staging, "snapshot.url")),
    paste0(
      "https://staging.r-multiverse.org/api/snapshot/tar",
      "?types=src,win,mac",
      "&binaries=",
      meta_snapshot()$r,
      "&skip_packages=issue,removed-has-issue"
    )
  )
  meta <- jsonlite::read_json(
    file.path(path_staging, "meta.json"),
    simplifyVector = TRUE
  )
  expect_equal(length(meta), 6L)
  expect_equal(meta$reset, meta_snapshot()$reset)
  expect_equal(meta$staging, meta_snapshot()$staging)
  expect_equal(meta$snapshot, meta_snapshot()$snapshot)
  expect_equal(meta$r, meta_snapshot()$r)
  expect_true(is.character(meta$cran))
  expect_true(is.character(meta$r_multiverse))
})
