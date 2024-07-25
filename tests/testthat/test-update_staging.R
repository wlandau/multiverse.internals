test_that("update_staging()", {
  dir_staging <- tempfile()
  dir_community <- tempfile()
  path_staging <- file.path(dir_staging, "staging")
  path_community <- file.path(dir_community, "community")
  dir.create(dir_staging)
  dir.create(dir_community)
  on.exit(unlink(path_staging, recursive = TRUE))
  on.exit(unlink(path_community, recursive = TRUE), add = TRUE)
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
  update_staging(
    path_staging = path_staging,
    path_community = path_community,
    mock = list(community = meta_community)
  )
  packages <- jsonlite::read_json(
    file.path(path_staging, "packages.json"),
    simplifyVector = TRUE
  )
  expect_true(is.data.frame(packages))
  expect_equal(dim(packages), c(4L, 3L))
  names <- c("change", "checks", "keep", "promote")
  expect_equal(sort(packages$package), sort(names))
  expect_equal(
    packages$url,
    file.path("https://github.com/owner", packages$package)
  )
  expect_equal(packages$branch, paste0("sha-", packages$package))
})
