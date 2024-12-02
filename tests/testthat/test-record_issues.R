test_that("record_issues() mocked", {
  output <- tempfile()
  record_issues(
    versions = mock_versions(),
    mock = list(
      checks = mock_meta_checks,
      packages = mock_meta_packages,
      today = "2024-01-01"
    ),
    output = output,
    verbose = FALSE
  )
  issues <- jsonlite::read_json(output, simplifyVector = TRUE)
  expect_equal(
    sort(names(issues)),
    sort(
      c(
        "audio.whisper",
        "INLA",
        "polars",
        "SBC",
        "stantargets",
        "targetsketch",
        "tidypolars",
        "tidytensor",
        "version_decremented",
        "version_unmodified"
      )
    )
  )
  runs <- "https://github.com/r-universe/r-multiverse/actions/runs"
  expect_equal(
    issues$INLA,
    list(
      checks = list(
        "_linuxdevel" = "src-failure",
        "_macbinary" = "src-failure",
        "_winbinary" = "src-failure",
        "_status" = "src-failure",
        "_buildurl" = file.path(runs, "9296256187")
      ),
      date = "2024-01-01",
      version = list(),
      remote_hash = list()
    )
  )
  expect_equal(
    issues$stantargets,
    list(
      checks = list(
        "_linuxdevel" = "failure",
        "_macbinary" = "success",
        "_winbinary" = "success",
        "_status" = "success",
        "_buildurl" = file.path(runs, "9412009826")
      ),
      descriptions = list(
        remotes = c("hyunjimoon/SBC", "stan-dev/cmdstanr")
      ),
      date = "2024-01-01",
      version = "0.1.1",
      remote_hash = "bbdda1b4a44a3d6a22041e03eed38f27319d8f32"
    )
  )
  expect_equal(
    issues$targetsketch,
    list(
      descriptions = list(
        license = "non-standard"
      ),
      date = "2024-01-01",
      version = "0.0.1",
      remote_hash = "a199a734b16f91726698a19e5f147f57f79cb2b6"
    )
  )
  expect_equal(
    issues$version_decremented,
    list(
      versions = list(
        version_current = "0.0.1",
        hash_current = "hash_0.0.1",
        version_highest = "1.0.0",
        hash_highest = "hash_1.0.0"
      ),
      date = "2024-01-01",
      version = list(),
      remote_hash = list()
    )
  )
  expect_equal(
    issues$version_unmodified,
    list(
      versions = list(
        version_current = "1.0.0",
        hash_current = "hash_1.0.0-modified",
        version_highest = "1.0.0",
        hash_highest = "hash_1.0.0"
      ),
      date = "2024-01-01",
      version = list(),
      remote_hash = list()
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
    output = output,
    verbose = FALSE
  )
  record_issues(
    versions = mock_versions(),
    mock = list(
      checks = mock_meta_checks,
      packages = mock_meta_packages
    ),
    output = output,
    verbose = FALSE
  )
  issues <- jsonlite::read_json(output, simplifyVector = TRUE)
  for (package in names(issues)) {
    expect_equal(issues[[package]]$date, "2024-01-01")
  }
  never_fixed <- c(
    "INLA",
    "stantargets",
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
  for (package in once_fixed) {
    issues[[package]] <- NULL
  }
  jsonlite::write_json(x = issues, path = output, pretty = TRUE)
  record_issues(
    versions = mock_versions(),
    mock = list(
      checks = mock_meta_checks,
      packages = mock_meta_packages
    ),
    output = output,
    verbose = FALSE
  )
  issues <- jsonlite::read_json(output, simplifyVector = TRUE)
  for (package in never_fixed) {
    expect_equal(issues[[package]]$date, "2024-01-01")
  }
  today <- format(Sys.Date(), fmt = "yyyy-mm-dd")
  for (package in once_fixed) {
    expect_equal(issues[[package]]$date, today)
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
    output = output,
    verbose = FALSE
  )
  expect_true(file.exists(output))
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
  on.exit(unlink(c(output, versions), recursive = TRUE))
  writeLines(lines, versions)
  meta_checks <- mock_meta_checks[1L, ]
  meta_checks$package <- "crew"
  meta_checks[["_winbinary"]] <- "failure"
  suppressMessages(
    record_issues(
      versions = versions,
      mock = list(
        checks = meta_checks,
        packages = mock_meta_packages_graph,
        today = "2024-01-01"
      ),
      output = output,
      verbose = TRUE
    )
  )
  expect_true(file.exists(output))
  issues <- jsonlite::read_json(output, simplifyVector = TRUE)
  expect_equal(
    issues$nanonext,
    list(
      versions = list(
        version_current = "1.0.0",
        hash_current = "hash_1.0.0-modified",
        version_highest = "1.0.0",
        hash_highest = "hash_1.0.0"
      ),
      date = "2024-01-01",
      version = "1.1.0.9000",
      remote_hash = "85dd672a44a92c890eb40ea9ebab7a4e95335c2f"
    )
  )
  expect_equal(
    issues$mirai,
    list(
      dependencies = list(
        nanonext = list()
      ),
      date = "2024-01-01",
      version = "1.1.0.9000",
      remote_hash = "7015695b7ef82f82ab3225ac2d226b2c8f298097"
    )
  )
  expect_equal(
    issues$crew,
    list(
      checks = list(
        "_linuxdevel" = "success",
        "_macbinary" = "success",
        "_winbinary" = "failure",
        "_status" = "success",
        "_buildurl" = file.path(
          "https://github.com/r-universe/r-multiverse/actions",
          "runs/9412009683"
        )
      ),
      dependencies = list(
        nanonext = "mirai"
      ),
      date = "2024-01-01",
      version = "0.9.3.9002",
      remote_hash = "eafad0276c06dec2344da2f03596178c754c8b5e"
    )
  )
  expect_equal(
    issues$crew.aws.batch,
    list(
      dependencies = list(
        crew = list(),
        nanonext = "crew"
      ),
      date = "2024-01-01",
      version = "0.0.5.9000",
      remote_hash = "4d9e5b44e2942d119af963339c48d134e84de458"
    )
  )
  expect_equal(
    issues$crew.cluster,
    list(
      dependencies = list(
        crew = list(),
        nanonext = "crew"
      ),
      date = "2024-01-01",
      version = "0.3.1",
      remote_hash = "d4ac61fd9a1d9539088ffebdadcd4bb713c25ee1"
    )
  )
})
