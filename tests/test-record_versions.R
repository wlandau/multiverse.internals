# Temporary files used in the mock test.
manifest <- tempfile()
issues <- tempfile()

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
r.releases.utils::record_versions(
  manifest = manifest,
  issues = issues,
  current = contents
)
written <- jsonlite::read_json(manifest)
expected <- list(
  list(
    package = "package_unmodified",
    version_current = "1.0.0",
    hash_current = "hash_1.0.0"
  ),
  list(
    package = "version_decremented",
    version_current = "1.0.0",
    hash_current = "hash_1.0.0"
  ),
  list(
    package = "version_incremented",
    version_current = "1.0.0",
    hash_current = "hash_1.0.0"
  ),
  list(
    package = "version_unmodified",
    version_current = "1.0.0",
    hash_current = "hash_1.0.0"
  )
)
stopifnot(identical(written, expected))
stopifnot(!file.exists(issues))

# Update the manifest after no changes to packages or versions.
r.releases.utils::record_versions(
  manifest = manifest,
  issues = issues,
  current = contents
)
written <- jsonlite::read_json(manifest)
expected <- list(
  list(
    package = "package_unmodified",
    version_current = "1.0.0",
    hash_current = "hash_1.0.0",
    version_highest = "1.0.0",
    hash_highest = "hash_1.0.0"
  ),
  list(
    package = "version_decremented",
    version_current = "1.0.0",
    hash_current = "hash_1.0.0",
    version_highest = "1.0.0",
    hash_highest = "hash_1.0.0"
  ),
  list(
    package = "version_incremented",
    version_current = "1.0.0",
    hash_current = "hash_1.0.0",
    version_highest = "1.0.0",
    hash_highest = "hash_1.0.0"
  ),
  list(
    package = "version_unmodified",
    version_current = "1.0.0",
    hash_current = "hash_1.0.0",
    version_highest = "1.0.0",
    hash_highest = "hash_1.0.0"
  )
)
stopifnot(identical(written, expected))
stopifnot(file.exists(issues))
stopifnot(identical(jsonlite::read_json(issues), list()))

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
r.releases.utils::record_versions(
  manifest = manifest,
  issues = issues,
  current = contents
)
written <- jsonlite::read_json(manifest)
expected <- list(
  list(
    package = "package_unmodified",
    version_current = "1.0.0",
    hash_current = "hash_1.0.0",
    version_highest = "1.0.0",
    hash_highest = "hash_1.0.0"
  ),
  list(
    package = "version_decremented",
    version_current = "0.0.1",
    hash_current = "hash_0.0.1",
    version_highest = "1.0.0",
    hash_highest = "hash_1.0.0"
  ),
  list(
    package = "version_incremented",
    version_current = "2.0.0",
    hash_current = "hash_2.0.0",
    version_highest = "2.0.0",
    hash_highest = "hash_2.0.0"
  ),
  list(
    package = "version_unmodified",
    version_current = "1.0.0",
    hash_current = "hash_1.0.0-modified",
    version_highest = "1.0.0",
    hash_highest = "hash_1.0.0"
  )
)
stopifnot(identical(written, expected))
stopifnot(file.exists(issues))
written_issues <- jsonlite::read_json(issues)
expected_issues <- list(
  list(
    package = "version_decremented",
    version_current = "0.0.1",
    hash_current = "hash_0.0.1",
    version_highest = "1.0.0",
    hash_highest = "hash_1.0.0"
  ),
  list(
    package = "version_unmodified",
    version_current = "1.0.0",
    hash_current = "hash_1.0.0-modified",
    version_highest = "1.0.0",
    hash_highest = "hash_1.0.0"
  )
)
stopifnot(identical(written_issues, expected_issues))

# Remove temporary files
unlink(c(manifest, issues))

# The manifest can be created and updated from the actual repo.
manifest <- tempfile()
issues <- tempfile()
r.releases.utils::record_versions(manifest = manifest, issues = issues)
stopifnot(file.exists(manifest))
r.releases.utils::record_versions(manifest = manifest, issues = issues)
contents <- jsonlite::read_json(manifest)
stopifnot(is.character(contents[[1L]]$package))
stopifnot(length(contents[[1L]]$package) == 1L)
stopifnot(file.exists(manifest))
stopifnot(file.exists(issues))
unlink(c(manifest, issues))
