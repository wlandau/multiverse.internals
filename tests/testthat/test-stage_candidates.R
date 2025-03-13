test_that("stage_candidates()", {
  # First time in the cycle.
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
  file_config <- file.path(path_staging, "config.json")
  file_staging <- file.path(path_staging, "packages.json")
  json_staging <- jsonlite::read_json(file_staging, simplifyVector = TRUE)
  stage_candidates(path_staging = path_staging)
  config <- jsonlite::read_json(file_config, simplifyVector = TRUE)
  expect_equal(names(config), "cran_version")
  expect_true(is.character(config$cran_version))
  expect_equal(config$cran_version, meta_snapshot()$dependency_freeze)
  packages <- jsonlite::read_json(file_staging, simplifyVector = TRUE)
  package <- c("issue", "removed-has-issue", "removed-no-issue", "staged")
  expect_equal(
    packages,
    data.frame(
      package = package,
      url = file.path("https://github.com/owner", package),
      branch = c("*release", "*release", "sha-removed-no-issue", "sha-staged")
    )
  )
  meta <- jsonlite::read_json(
    file.path(path_staging, "snapshot.json"),
    simplifyVector = TRUE
  )
  expect_equal(length(meta), 6L)
  expect_equal(meta$dependency_freeze, meta_snapshot()$dependency_freeze)
  expect_equal(meta$candidate_freeze, meta_snapshot()$candidate_freeze)
  expect_equal(meta$snapshot, meta_snapshot()$snapshot)
  expect_equal(meta$r, meta_snapshot()$r)
  expect_true(is.character(meta$cran))
  expect_true(is.character(meta$r_multiverse))
  # Staged package breaks.
  file_status <- file.path(path_staging, "status.json")
  json_status <- jsonlite::read_json(file_status, simplifyVector = TRUE)
  json_status$staged <- json_status$issue
  jsonlite::write_json(json_status, file_status, pretty = TRUE)
  stage_candidates(path_staging = path_staging)
  packages <- jsonlite::read_json(file_staging, simplifyVector = TRUE)
  package <- c("issue", "removed-has-issue", "removed-no-issue", "staged")
  expect_equal(
    packages,
    data.frame(
      package = package,
      url = file.path("https://github.com/owner", package),
      branch = c("*release", "*release", "sha-removed-no-issue", "sha-staged")
    )
  )
  # Broken package gets fixed.
  file_status <- file.path(path_staging, "status.json")
  json_status <- jsonlite::read_json(file_status, simplifyVector = TRUE)
  hash <- json_status$issue$remote_hash
  json_status$issue <- json_status[["removed-no-issue"]]
  json_status$issue$remote_hash <- hash
  jsonlite::write_json(json_status, file_status, pretty = TRUE)
  stage_candidates(path_staging = path_staging)
  packages <- jsonlite::read_json(file_staging, simplifyVector = TRUE)
  package <- c("issue", "removed-has-issue", "removed-no-issue", "staged")
  expect_equal(
    packages,
    data.frame(
      package = package,
      url = file.path("https://github.com/owner", package),
      branch = c("sha-issue", "*release", "sha-removed-no-issue", "sha-staged")
    )
  )
})
