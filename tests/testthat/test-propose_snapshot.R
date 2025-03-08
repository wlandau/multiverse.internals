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
      r_version_staging()$short,
      "&skip_packages=issue,removed-has-issue"
    )
  )
  meta <- jsonlite::read_json(
    file.path(path_staging, "meta.json"),
    simplifyVector = TRUE
  )
  expect_equal(length(meta), 5L)
  expect_equal(meta$date_staging, date_staging())
  expect_equal(meta$date_snapshot, date_snapshot())
  expect_equal(meta$r_version, r_version_staging()$short)
  expect_true(is.character(meta$r_multiverse))
  expect_true(is.character(meta$cran))
})
