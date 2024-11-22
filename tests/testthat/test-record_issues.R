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
  expect_equal(
    sort(c(list.files(output))),
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
    jsonlite::read_json(file.path(output, "INLA"), simplifyVector = TRUE),
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
    jsonlite::read_json(
      file.path(output, "stantargets"),
      simplifyVector = TRUE
    ),
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
    jsonlite::read_json(
      file.path(output, "targetsketch"),
      simplifyVector = TRUE
    ),
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
      date = "2024-01-01",
      version = list(),
      remote_hash = list()
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
  for (file in list.files(output, full.names = TRUE)) {
    date <- jsonlite::read_json(file, simplifyVector = TRUE)$date
    expect_equal(date, "2024-01-01")
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
  lapply(file.path(output, once_fixed), unlink)
  record_issues(
    versions = mock_versions(),
    mock = list(
      checks = mock_meta_checks,
      packages = mock_meta_packages
    ),
    output = output,
    verbose = FALSE
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
    output = output,
    verbose = FALSE
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
      date = "2024-01-01",
      version = "1.1.0.9000",
      remote_hash = "85dd672a44a92c890eb40ea9ebab7a4e95335c2f"
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
      date = "2024-01-01",
      version = "1.1.0.9000",
      remote_hash = "7015695b7ef82f82ab3225ac2d226b2c8f298097"
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
    jsonlite::read_json(
      file.path(output, "crew.aws.batch"),
      simplifyVector = TRUE
    ),
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
    jsonlite::read_json(
      file.path(output, "crew.cluster"),
      simplifyVector = TRUE
    ),
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
