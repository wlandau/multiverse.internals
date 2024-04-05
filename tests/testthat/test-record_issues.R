test_that("version_issues() in a mock repo", {
  # Temporary files used in the mock test.
  manifest <- tempfile()
  # First update to the manifest.
  contents <- data.frame(
    package = c(
      "package_unmodified",
      "version_decremented",
      "version_incremented",
      "version_unmodified"
    ),
    version_current = rep("1.0.0", 4L),
    hash_current = rep("hash_1.0.0", 4L)
  )
  record_versions(manifest = manifest, current = contents)
  expect_equal(
    version_issues(manifest),
    data.frame(
      package = character(0L),
      version_current = character(0L),
      hash_current = character(0L)
    )
  )
  # Update the manifest after no changes to packages or versions.
  suppressMessages(
    record_versions(manifest = manifest, current = contents)
  )
  expect_equal(
    version_issues(manifest),
    data.frame(
      package = character(0L),
      version_current = character(0L),
      hash_current = character(0L),
      version_highest = character(0L),
      hash_highest = character(0L)
    )
  )
  # Update the packages in all the ways indicated above.
  index <- contents$package == "version_decremented"
  contents$version_current[index] <- "0.0.1"
  contents$hash_current[index] <- "hash_0.0.1"
  index <- contents$package == "version_incremented"
  contents$version_current[index] <- "2.0.0"
  contents$hash_current[index] <- "hash_2.0.0"
  index <- contents$package == "version_unmodified"
  contents$version_current[index] <- "1.0.0"
  contents$hash_current[index] <- "hash_1.0.0-modified"
  for (index in seq_len(2L)) {
    record_versions(
      manifest = manifest,
      current = contents
    )
    out <- version_issues(manifest)
    rownames(out) <- NULL
    expect_equal(
      out,
      data.frame(
        package = c("version_decremented", "version_unmodified"),
        version_current = c("0.0.1", "1.0.0"),
        hash_current = c("hash_0.0.1", "hash_1.0.0-modified"),
        version_highest = c("1.0.0", "1.0.0"),
        hash_highest = c("hash_1.0.0", "hash_1.0.0"),
        version_okay = c(FALSE, FALSE)
      )
    )
  }
  # Remove temporary files
  unlink(manifest)
})

test_that("record_issues() in a mock repo", {
  # Temporary files used in the mock test.
  manifest <- tempfile()
  output <- tempfile()
  # First update to the manifest.
  contents <- data.frame(
    package = c(
      "package_unmodified",
      "version_decremented",
      "version_incremented",
      "version_unmodified"
    ),
    version_current = rep("1.0.0", 4L),
    hash_current = rep("hash_1.0.0", 4L)
  )
  record_versions(manifest = manifest, current = contents)
  record_issues(manifest = manifest, output = output)
  expect_equal(list.files(output), character(0L))
  # Update the manifest after no changes to packages or versions.
  suppressMessages(
    record_versions(manifest = manifest, current = contents)
  )
  record_issues(manifest = manifest, output = output)
  expect_equal(list.files(output), character(0L))
  # Update the packages in all the ways indicated above.
  index <- contents$package == "version_decremented"
  contents$version_current[index] <- "0.0.1"
  contents$hash_current[index] <- "hash_0.0.1"
  index <- contents$package == "version_incremented"
  contents$version_current[index] <- "2.0.0"
  contents$hash_current[index] <- "hash_2.0.0"
  index <- contents$package == "version_unmodified"
  contents$version_current[index] <- "1.0.0"
  contents$hash_current[index] <- "hash_1.0.0-modified"
  for (index in seq_len(2L)) {
    record_versions(
      manifest = manifest,
      current = contents
    )
    record_issues(manifest = manifest, output = output)
    expect_equal(
      sort(list.files(output)),
      sort(c("version_decremented", "version_unmodified"))
    )
    out <- jsonlite::read_json(file.path(output, "version_decremented"))
    expect_equal(
      unlist(out, recursive = TRUE),
      c(
        package = "version_decremented",
        version_current = "0.0.1",
        hash_current = "hash_0.0.1",
        version_highest = "1.0.0",
        hash_highest = "hash_1.0.0",
        version_okay = FALSE
      )
    )
    out <- jsonlite::read_json(file.path(output, "version_unmodified"))
    expect_equal(
      unlist(out, recursive = TRUE),
      c(
        package = "version_unmodified",
        version_current = "1.0.0",
        hash_current = "hash_1.0.0-modified",
        version_highest = "1.0.0",
        hash_highest = "hash_1.0.0",
        version_okay = FALSE
      )
    )
  }
  # Remove temporary files
  unlink(manifest)
})
