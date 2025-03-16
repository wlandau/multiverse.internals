test_that("record_status() mocked", {
  output <- tempfile()
  record_status(
    versions = mock_versions(),
    mock = list(packages = mock_meta_packages),
    output = output,
    verbose = FALSE
  )
  out <- readLines(output)
  status <- jsonlite::read_json(output, simplifyVector = TRUE)
  expect_equal(sort(names(status)), sort(names(mock_status)))
  names <- sort(names(status))
  expect_equal(status[names], mock_status[names])
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
  mock <- mock_meta_packages
  mock$issues_r_cmd_check[mock$package == "crew"] <-
    mock$issues_r_cmd_check[mock$package == "targets"]
  suppressMessages(
    record_status(
      versions = versions,
      mock = list(
        packages = mock,
        today = "2024-01-01"
      ),
      output = output,
      verbose = TRUE
    )
  )
  expect_true(file.exists(output))
  status <- jsonlite::read_json(output, simplifyVector = TRUE)
  expect_equal(
    status$nanonext$versions,
    list(
      version_current = "1.0.0",
      hash_current = "hash_1.0.0-modified",
      version_highest = "1.0.0",
      hash_highest = "hash_1.0.0"
    )
  )
  expect_equal(
    status$mirai$dependencies,
    list(nanonext = list())
  )
  expect_equal(
    status$crew$dependencies,
    list(nanonext = "mirai")
  )
  expect_equal(
    status$crew$r_cmd_check$issues,
    mock$issues_r_cmd_check[mock$package == "targets"][[1L]]
  )
  expect_equal(
    status$crew.aws.batch$dependencies,
    list(
      crew = list(),
      nanonext = "crew"
    )
  )
  expect_equal(
    status$crew.cluster$dependencies,
    list(
      crew = list(),
      nanonext = "crew"
    )
  )
  packages <- c("nanonext", "mirai", "crew", "crew.cluster", "crew.aws.batch")
  for (package in packages) {
    expect_false(status[[package]]$success)
  }
})
