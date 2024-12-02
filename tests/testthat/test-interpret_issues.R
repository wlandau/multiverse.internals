test_that("interpret_issues() with no problems", {
  expect_equal(
    interpret_issues("abc", list()),
    "Package abc has no recorded issues."
  )
})

test_that("interpret_issues() with advisories", {
  skip_if_offline()
  mock <- mock_meta_packages
  for (field in c("_id", "_dependencies", "distro", "remotes")) {
    mock[[field]] <- NULL
  }
  example <- mock$package == "nanonext"
  commonmark <- mock[example,, drop = FALSE] # nolint
  commonmark$package <- "commonmark"
  commonmark$version <- "0.2"
  readxl <- mock[example,, drop = FALSE] # nolint
  readxl$package <- "readxl"
  readxl$version <- "1.4.1"
  meta <- rbind(
    mock[seq_len(nrow(mock)), ], # to suppress a warning
    commonmark,
    readxl
  )
  output <- tempfile()
  versions <- tempfile()
  record_versions(
    versions = versions,
    repo = "https://wlandau.r-universe.dev"
  )
  on.exit(unlink(c(output, versions), recursive = TRUE))
  record_issues(
    mock = list(packages = meta, checks = mock_meta_checks),
    output = output,
    versions = versions
  )
  issues <- jsonlite::read_json(output, simplifyVector = TRUE)
  out <- interpret_issues("commonmark", issues)
  expect_true(
    grepl(
      "Found the following advisories in the R Consortium Advisory Database",
      out
    )
  )
})

test_that("interpret_issues() with bad licenses", {
  skip_if_offline()
  mock <- mock_meta_packages[mock_meta_packages$package == "targetsketch", ]
  output <- tempfile()
  versions <- tempfile()
  record_versions(
    versions = versions,
    repo = "https://wlandau.r-universe.dev"
  )
  on.exit(unlink(c(output, versions), recursive = TRUE))
  record_issues(
    mock = list(packages = mock, checks = mock_meta_checks),
    output = output,
    versions = versions
  )
  issues <- jsonlite::read_json(output, simplifyVector = TRUE)
  out <- interpret_issues("targetsketch", issues)
  expect_true(
    grepl(
      "targetsketch declares license",
      out
    )
  )
})

test_that("interpret_issues() checks etc.", {
  skip_if_offline()
  output <- tempfile()
  lines <- c(
    "[",
    " {",
    " \"package\": \"constantversion\",",
    " \"version_current\": \"1.0.0\",",
    " \"hash_current\": \"hash_1.0.0-modified\",",
    " \"version_highest\": \"1.0.0\",",
    " \"hash_highest\": \"hash_1.0.0\"",
    " }",
    "]"
  )
  versions <- tempfile()
  writeLines(lines, versions)
  on.exit(unlink(c(output, versions), recursive = TRUE))
  record_issues(
    mock = list(packages = mock_meta_packages, checks = mock_meta_checks),
    output = output,
    versions = versions
  )
  issues <- jsonlite::read_json(output, simplifyVector = TRUE)
  expect_true(
    grepl(
      "Not all checks succeeded on R-universe",
      interpret_issues("INLA", issues)
    )
  )
  expect_true(
    grepl(
      "Not all checks succeeded on R-universe",
      interpret_issues("polars", issues)
    )
  )
  expect_true(
    grepl(
      "Package releases should not use the 'Remotes:' field",
      interpret_issues("audio.whisper", issues),
      fixed = TRUE
    )
  )
  expect_true(
    grepl(
      "bnosac/audio.vadwebrtc",
      interpret_issues("audio.whisper", issues),
      fixed = TRUE
    )
  )
  expect_true(
    grepl(
      "One or more dependencies have issues",
      interpret_issues("tidypolars", issues)
    )
  )
  expect_true(
    grepl(
      "The version number of the current release should be highest version",
      interpret_issues("constantversion", issues)
    )
  )
})

test_that("interpret_issues() with complicated dependency problems", {
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
  expect_true(
    grepl(
      "\nnanonext: mirai\n",
      interpret_issues("crew", issues)
    )
  )
  expect_true(
    grepl(
      "\nnanonext: crew\n",
      interpret_issues("crew.aws.batch", issues)
    )
  )
  expect_true(
    grepl(
      "Dependency nanonext is explicitly mentioned in",
      interpret_issues("mirai", issues)
    )
  )
})
