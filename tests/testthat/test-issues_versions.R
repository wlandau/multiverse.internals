test_that("issues_versions() mocked", {
  # Temporary files used in the mock test.
  versions <- tempfile()
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
  record_versions(versions = versions, current = contents)
  expect_equal(nrow(issues_versions(versions)), 0L)
  expect_equal(
    sort(colnames(issues_versions(versions))),
    sort(c("package", "versions"))
  )
  # Update the manifest after no changes to packages or versions.
  suppressMessages(
    record_versions(versions = versions, current = contents)
  )
  expect_equal(nrow(issues_versions(versions)), 0L)
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
  expected <- data.frame(
    package = c("version_decremented", "version_unmodified")
  )
  expected$versions <- list(
    list(
      version_current = "0.0.1",
      hash_current = "hash_0.0.1",
      version_highest = "1.0.0",
      hash_highest = "hash_1.0.0"
    ),
    list(
      version_current = "1.0.0",
      hash_current = "hash_1.0.0-modified",
      version_highest = "1.0.0",
      hash_highest = "hash_1.0.0"
    )
  )
  for (index in seq_len(2L)) {
    record_versions(
      versions = versions,
      current = contents
    )
    out <- issues_versions(versions)
    expect_equal(out, expected)
  }
  # Remove temporary files
  unlink(versions)
})
