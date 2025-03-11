test_that("filter_packages", {
  path_packages <- tempfile()
  dir.create(path_packages)
  on.exit(unlink(path_packages, recursive = TRUE))
  mock <- system.file(
    file.path("mock", "packages"),
    package = "multiverse.internals",
    mustWork = TRUE
  )
  file.copy(mock, path_packages, recursive = TRUE)
  path_staging <- tempfile()
  dir.create(path_staging)
  json_staging <- data.frame(
    package = c("arrow", "crew", "mirai"),
    repo = rep("", 3L),
    branch = c("*release", "sha-crew", "sha-mirai")
  )
  file_staging <- file.path(path_staging, "packages.json")
  jsonlite::write_json(json_staging, file_staging, pretty = TRUE)
  filter_packages(path_packages, path_staging)
  listings <- list.files(
    path_packages,
    recursive = TRUE,
    pattern = "PACKAGES$",
    full.names = TRUE
  )
  for (listing in listings) {
    expect_true(file.exists(listing))
    data <- read.dcf(listing)
    expect_equal(sort(data[, "Package"]), sort(c("crew", "mirai")))
    data_gz <- read.dcf(gzfile(file.path(dirname(listing), "PACKAGES.gz")))
    expect_equal(sort(data_gz[, "Package"]), sort(c("crew", "mirai")))
  }
})
