test_that("promote_packages", {
  dir_production <- tempfile()
  dir_community <- tempfile()
  path_production <- file.path(dir_production, "production")
  path_community <- file.path(dir_community, "community")
  dir.create(dir_production)
  dir.create(dir_community)
  on.exit(unlink(path_production, recursive = TRUE))
  on.exit(unlink(path_community, recursive = TRUE), add = TRUE)
  mock <- system.file(
    "mock",
    package = "multiverse.internals",
    mustWork = TRUE
  )
  file.copy(
    from = file.path(mock, "production"),
    to = dir_production,
    recursive = TRUE
  )
  file.copy(
    from = file.path(mock, "community"),
    to = dir_community,
    recursive = TRUE
  )
  json <- jsonlite::read_json(file.path(path_community, "packages.json"))
  names <- vapply(
    json,
    function(x) x$package,
    FUN.VALUE = character(1L)
  )
  meta_community <- data.frame(
    package = names,
    remotesha = paste0("sha-", names)
  )
  promote_packages(
    path_production = path_production,
    path_community = path_community,
    meta = meta_community
  )
  packages <- jsonlite::read_json(
    file.path(path_production, "packages.json"),
    simplifyVector = TRUE
  )
  expect_true(is.data.frame(packages))
  expect_equal(dim(packages), c(6L, 3L))
  expect_equal(
    packages$package,
    c(
      "community-good", "community-notice", "community-remove",
      "production-good", "production-notice", "production-remove"
    )
  )
  expect_equal(
    packages$url,
    file.path("https://github.com/owner", packages$package)
  )
  expect_equal(packages$branch, paste0("sha-", packages$package))
  removing <- jsonlite::read_json(
    file.path(path_production, "removing.json"),
    simplifyVector = TRUE
  )
  expect_equal(removing, "production-removing")
})
