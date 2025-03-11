test_that("rclone_includes()", {
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
  file_config <- file.path(path_staging, "config.json")
  file_staged <- file.path(path_staging, "staged.json")
  file_staging <- file.path(path_staging, "packages.json")
  file_community <- file.path(path_community, "packages.json")
  json_staging <- jsonlite::read_json(file_staging)
  json_community <- jsonlite::read_json(file_community)
  names_staging <- vapply(
    json_staging,
    function(x) x$package,
    FUN.VALUE = character(1L)
  )
  names_community <- vapply(
    json_community,
    function(x) x$package,
    FUN.VALUE = character(1L)
  )
  meta_staging <- data.frame(
    package = names_staging,
    version = "1.2.3",
    remotesha = paste0("sha-", names_staging)
  )
  meta_community <- data.frame(
    package = names_community,
    remotesha = paste0("sha-", names_community)
  )
  unlink(file_staged)
  stage_candidates(
    path_staging = path_staging,
    path_community = path_community,
    mock = list(community = meta_community)
  )
  file_packages <- file.path(path_staging, "include-packages.txt")
  file_meta <- file.path(path_staging, "include-meta.txt")
  expect_false(file.exists(file_packages))
  expect_false(file.exists(file_meta))
  rclone_includes(
    path_staging,
    mock = list(staging = meta_staging)
  )
  expect_true(file.exists(file_packages))
  expect_true(file.exists(file_meta))
  include_packages <- readLines(file_packages)
  expect_equal(
    sort(include_packages),
    sort(
      c(
        "src/contrib/staged_1.2.3.tar.gz",
        "src/contrib/removed-no-issue_1.2.3.tar.gz",
        "bin/macosx/*/contrib/4.4/staged_1.2.3.tgz",
        "bin/macosx/*/contrib/4.4/removed-no-issue_1.2.3.tgz",
        "bin/windows/contrib/4.4/staged_1.2.3.zip",
        "bin/windows/contrib/4.4/removed-no-issue_1.2.3.zip"
      )
    )
  )
  include_meta <- readLines(file_meta)
  expect_equal(
    sort(include_meta),
    sort(
      c(
        "src/contrib/PACKAGES",
        "src/contrib/PACKAGES.gz",
        "bin/macosx/*/contrib/4.4/PACKAGES",
        "bin/macosx/*/contrib/4.4/PACKAGES.gz",
        "bin/windows/contrib/4.4/PACKAGES",
        "bin/windows/contrib/4.4/PACKAGES.gz"
      )
    )
  )
})
