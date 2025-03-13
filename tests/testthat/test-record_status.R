test_that("record_status() mocked", {
  output <- tempfile()
  record_status(
    versions = mock_versions(),
    mock = list(
      checks = mock_meta_checks,
      packages = mock_meta_packages,
      today = "2024-01-01"
    ),
    output = output,
    verbose = FALSE
  )
  status <- jsonlite::read_json(output, simplifyVector = TRUE)
  expect_failed <- c(
    "audio.whisper", "colorout", "demographr", "geographr", "glaredb",
    "healthyr", "igraph", "INLA", "loneliness",
    "prqlr", "SBC", "stantargets",
    "taxizedb", "tidytensor", "webseq", "wildfires", "targetsketch",
    "tidypolars", "version_decremented", "version_unmodified"
  )
  for (package in expect_failed) {
    expect_false(status[[package]]$success)
  }
  expect_succeeded <- c(
    "audio.vadwebrtc", "cmdstanr", "duckdb", "httpgd", "ichimoku",
    "mirai", "multitools", "multiverse.internals", "nanonext",
    "polars", "secretbase", "string2path", "tinytest", "zstdlite"
  )
  for (package in expect_succeeded) {
    expect_true(status[[package]]$success)
  }
  runs <- "https://github.com/r-universe/r-multiverse/actions/runs"
  expect_equal(
    status$INLA,
    list(
      checks = list(
        url = file.path(runs, "11566311732"),
        status = list(
          `linux R-devel` = "MISSING",
          `mac R-release` = "MISSING",
          `win R-release` = "MISSING"
        )
      ),
      success = FALSE,
      date = "2024-01-01",
      version = list(),
      remote_hash = list()
    )
  )
  expect_equal(
    status$stantargets,
    list(
      checks = list(
        url = file.path(runs, "12139784185"),
        status = list(
          `linux R-4.5.0` = "WARNING",
          `mac R-4.4.2` = "WARNING",
          `win R-4.4.2` = "WARNING"
        )
      ),
      descriptions = list(
        remotes = c("hyunjimoon/SBC", "stan-dev/cmdstanr")
      ),
      success = FALSE,
      date = "2024-01-01",
      version = "0.1.1",
      remote_hash = "bbdda1b4a44a3d6a22041e03eed38f27319d8f32"
    )
  )
  expect_equal(
    status$targetsketch,
    list(
      descriptions = list(
        license = "non-standard"
      ),
      success = FALSE,
      date = "2024-01-01",
      version = "0.0.1",
      remote_hash = "a199a734b16f91726698a19e5f147f57f79cb2b6"
    )
  )
  expect_equal(
    status$version_decremented,
    list(
      versions = list(
        version_current = "0.0.1",
        hash_current = "hash_0.0.1",
        version_highest = "1.0.0",
        hash_highest = "hash_1.0.0"
      ),
      success = FALSE,
      date = "2024-01-01",
      version = list(),
      remote_hash = list()
    )
  )
  expect_equal(
    status$version_unmodified,
    list(
      versions = list(
        version_current = "1.0.0",
        hash_current = "hash_1.0.0-modified",
        version_highest = "1.0.0",
        hash_highest = "hash_1.0.0"
      ),
      success = FALSE,
      date = "2024-01-01",
      version = list(),
      remote_hash = list()
    )
  )
})

test_that("record_status() date works", {
  output <- tempfile()
  record_status(
    versions = mock_versions(),
    mock = list(
      checks = mock_meta_checks,
      packages = mock_meta_packages,
      today = "2024-01-01"
    ),
    output = output,
    verbose = FALSE
  )
  record_status(
    versions = mock_versions(),
    mock = list(
      checks = mock_meta_checks,
      packages = mock_meta_packages
    ),
    output = output,
    verbose = FALSE
  )
  status <- jsonlite::read_json(output, simplifyVector = TRUE)
  for (package in names(status)) {
    expect_equal(status[[package]]$date, "2024-01-01")
  }
  never_fixed <- c(
    "INLA",
    "stantargets",
    "tidytensor",
    "version_decremented"
  )
  once_fixed <- c(
    "audio.whisper",
    "SBC",
    "tidypolars",
    "version_unmodified"
  )
  for (package in once_fixed) {
    status[[package]] <- NULL
  }
  jsonlite::write_json(x = status, path = output, pretty = TRUE)
  record_status(
    versions = mock_versions(),
    mock = list(
      checks = mock_meta_checks,
      packages = mock_meta_packages
    ),
    output = output,
    verbose = FALSE
  )
  status <- jsonlite::read_json(output, simplifyVector = TRUE)
  for (package in never_fixed) {
    expect_equal(status[[package]]$date, "2024-01-01")
  }
  today <- format(Sys.Date(), fmt = "yyyy-mm-dd")
  for (package in once_fixed) {
    expect_equal(status[[package]]$date, today)
  }
})

test_that("record_status() on a small repo", {
  output <- tempfile()
  versions <- tempfile()
  record_versions(
    versions = versions,
    repo = "https://wlandau.r-universe.dev"
  )
  record_status(
    repo = "https://wlandau.r-universe.dev",
    versions = versions,
    output = output,
    verbose = FALSE
  )
  expect_true(file.exists(output))
})

test_that("record_status() with dependency problems", {
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
  suppressMessages(
    record_status(
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
  status <- jsonlite::read_json(output, simplifyVector = TRUE)
  expect_equal(
    status$nanonext,
    list(
      versions = list(
        version_current = "1.0.0",
        hash_current = "hash_1.0.0-modified",
        version_highest = "1.0.0",
        hash_highest = "hash_1.0.0"
      ),
      success = FALSE,
      date = "2024-01-01",
      version = "1.1.0.9000",
      remote_hash = "85dd672a44a92c890eb40ea9ebab7a4e95335c2f"
    )
  )
  expect_equal(
    status$mirai,
    list(
      dependencies = list(
        nanonext = list()
      ),
      success = FALSE,
      date = "2024-01-01",
      version = "1.1.0.9000",
      remote_hash = "7015695b7ef82f82ab3225ac2d226b2c8f298097"
    )
  )
  expect_equal(
    status$crew,
    list(
      checks = list(url = meta_checks$url, status = meta_checks$status[[1L]]),
      dependencies = list(
        nanonext = "mirai"
      ),
      success = FALSE,
      date = "2024-01-01",
      version = "0.9.3.9002",
      remote_hash = "eafad0276c06dec2344da2f03596178c754c8b5e"
    )
  )
  expect_equal(
    status$crew.aws.batch,
    list(
      dependencies = list(
        crew = list(),
        nanonext = "crew"
      ),
      success = FALSE,
      date = "2024-01-01",
      version = "0.0.5.9000",
      remote_hash = "4d9e5b44e2942d119af963339c48d134e84de458"
    )
  )
  expect_equal(
    status$crew.cluster,
    list(
      dependencies = list(
        crew = list(),
        nanonext = "crew"
      ),
      success = FALSE,
      date = "2024-01-01",
      version = "0.3.1",
      remote_hash = "d4ac61fd9a1d9539088ffebdadcd4bb713c25ee1"
    )
  )
})
