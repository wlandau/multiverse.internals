test_that("filter_packages", {
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  mock <- system.file(
    "packages",
    package = "multiverse.internals",
    mustWork = TRUE
  )
  file.copy(mock, path, recursive = TRUE)
  staged <- tempfile()
  jsonlite::write_json(c("crew", "mirai"), staged, pretty = TRUE)
  filter_packages(path, staged)
  listings <- list.files(
    path,
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
