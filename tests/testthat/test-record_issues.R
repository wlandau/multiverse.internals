test_that("record_issues() mocked", {
  output <- tempfile()
  record_issues(
    versions = mock_versions(),
    mock = list(
      checks = mock_meta_checks,
      packages = mock_meta_packages,
      today = "2024-01-01"
    ),
    output = output
  )
  expect_equal(
    sort(c(list.files(output))),
    sort(
      c(
        "audio.whisper",
        "httpgd",
        "INLA",
        "polars",
        "SBC",
        "stantargets",
        "string2path",
        "tidypolars",
        "tidytensor",
        "version_decremented",
        "version_unmodified"
      )
    )
  )
  runs <- "https://github.com/r-universe/r-multiverse/actions/runs"
  expect_equal(
    jsonlite::read_json(file.path(output, "INLA"), simplifyVector = TRUE),
    list(
      checks = list(
        "_linuxdevel" = "src-failure",
        "_macbinary" = "src-failure",
        "_wasmbinary" = "src-failure",
        "_winbinary" = "src-failure",
        "_status" = "src-failure",
        "_buildurl" = file.path(runs, "8487512222")
      ),
      date = "2024-01-01"
    )
  )
  expect_equal(
    jsonlite::read_json(
      file.path(output, "stantargets"),
      simplifyVector = TRUE
    ),
    list(
      checks = list(
        "_linuxdevel" = "failure",
        "_macbinary" = "success",
        "_wasmbinary" = "success",
        "_winbinary" = "success",
        "_status" = "success",
        "_buildurl" = file.path(runs, "8998732490")
      ),
      descriptions = list(
        remotes = c("hyunjimoon/SBC", "stan-dev/cmdstanr")
      ),
      date = "2024-01-01"
    )
  )
  expect_equal(
    jsonlite::read_json(
      file.path(output, "version_decremented"),
      simplifyVector = TRUE
    ),
    list(
      versions = list(
        version_current = "0.0.1",
        hash_current = "hash_0.0.1",
        version_highest = "1.0.0",
        hash_highest = "hash_1.0.0"
      ),
      date = "2024-01-01"
    )
  )
  expect_equal(
    jsonlite::read_json(
      file.path(output, "version_unmodified"),
      simplifyVector = TRUE
    ),
    list(
      versions = list(
        version_current = "1.0.0",
        hash_current = "hash_1.0.0-modified",
        version_highest = "1.0.0",
        hash_highest = "hash_1.0.0"
      ),
      date = "2024-01-01"
    )
  )
})

test_that("record_issues() date works", {
  output <- tempfile()
  record_issues(
    versions = mock_versions(),
    mock = list(
      checks = mock_meta_checks,
      packages = mock_meta_packages,
      today = "2024-01-01"
    ),
    output = output
  )
  record_issues(
    versions = mock_versions(),
    mock = list(
      checks = mock_meta_checks,
      packages = mock_meta_packages
    ),
    output = output
  )
  for (file in list.files(output, full.names = TRUE)) {
    date <- jsonlite::read_json(file, simplifyVector = TRUE)$date
    expect_equal(date, "2024-01-01")
  }
  never_fixed <- c(
    "httpgd",
    "INLA",
    "stantargets",
    "string2path",
    "tidytensor",
    "version_decremented"
  )
  once_fixed <- c(
    "audio.whisper",
    "polars",
    "SBC",
    "tidypolars",
    "version_unmodified"
  )
  lapply(file.path(output, once_fixed), unlink)
  record_issues(
    versions = mock_versions(),
    mock = list(
      checks = mock_meta_checks,
      packages = mock_meta_packages
    ),
    output = output
  )
  for (package in never_fixed) {
    path <- file.path(output, package)
    date <- jsonlite::read_json(path, simplifyVector = TRUE)$date
    expect_equal(date, "2024-01-01")
  }
  today <- format(Sys.Date(), fmt = "yyyy-mm-dd")
  for (package in once_fixed) {
    path <- file.path(output, package)
    date <- jsonlite::read_json(path, simplifyVector = TRUE)$date
    expect_equal(date, today)
  }
})

test_that("record_issues() on a small repo", {
  output <- tempfile()
  versions <- tempfile()
  record_versions(
    versions = versions,
    repo = "https://wlandau.r-universe.dev"
  )
  record_issues(
    repo = "https://wlandau.r-universe.dev",
    versions = versions,
    output = output
  )
  expect_true(dir.exists(output))
})

test_that("record_issues() with dependency problems", {
  output <- tempfile()
  lines <- c(
    "[",
    " {",
    " \"package\": \"nanonext\",",
    " \"version_current\": \"1.0.0\",",
    " \"hash_current\": \"hash_1.0.0-modified\",",
    " \"version_highest\": \"1.0.0\",",
    " \"hash_highest\": \"hash_1.0.0\"",
    " }",
    "]"
  )
  versions <- tempfile()
  writeLines(lines, versions)
  meta_checks <- mock_meta_checks[1L, ]
  meta_checks$package <- "crew"
  meta_checks[["_winbinary"]] <- "failure"
  record_issues(
    versions = versions,
    mock = list(
      checks = meta_checks,
      packages = mock_meta_packages_graph,
      today = "2024-01-01"
    ),
    output = output
  )
  expect_true(dir.exists(output))
  expect_equal(
    jsonlite::read_json(
      file.path(output, "nanonext"),
      simplifyVector = TRUE
    ),
    list(
      versions = list(
        version_current = "1.0.0",
        hash_current = "hash_1.0.0-modified",
        version_highest = "1.0.0",
        hash_highest = "hash_1.0.0"
      ),
      date = "2024-01-01"
    )
  )
  expect_equal(
    jsonlite::read_json(
      file.path(output, "mirai"),
      simplifyVector = TRUE
    ),
    list(
      dependencies = list(
        nanonext = list()
      ),
      date = "2024-01-01"
    )
  )
  expect_equal(
    jsonlite::read_json(
      file.path(output, "crew"),
      simplifyVector = TRUE
    ),
    list(
      checks = list(
        "_linuxdevel" = "success",
        "_macbinary" = "success",
        "_wasmbinary" = "success",
        "_winbinary" = "failure",
        "_status" = "success",
        "_buildurl" = file.path(
          "https://github.com/r-universe/r-multiverse/actions",
          "runs/8998731783"
        )
      ),
      dependencies = list(
        nanonext = "mirai"
      ),
      date = "2024-01-01"
    )
  )
  expect_equal(
    jsonlite::read_json(
      file.path(output, "crew.aws.batch"),
      simplifyVector = TRUE
    ),
    list(
      dependencies = list(
        crew = list(),
        nanonext = "crew"
      ),
      date = "2024-01-01"
    )
  )
  expect_equal(
    jsonlite::read_json(
      file.path(output, "crew.cluster"),
      simplifyVector = TRUE
    ),
    list(
      dependencies = list(
        crew = list(),
        nanonext = "crew"
      ),
      date = "2024-01-01"
    )
  )
})
