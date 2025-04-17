test_that("rclone_includes()", {
  dir_staging <- tempfile()
  path_staging <- file.path(dir_staging, "staging")
  dir.create(dir_staging)
  on.exit(unlink(path_staging, recursive = TRUE))
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
  file_config <- file.path(path_staging, "config.json")
  file_staging <- file.path(path_staging, "packages.json")
  json_staging <- jsonlite::read_json(file_staging)
  names_staging <- vapply(
    json_staging,
    function(x) x$package,
    FUN.VALUE = character(1L)
  )
  stage_candidates(path_staging = path_staging)
  file_packages <- file.path(path_staging, "include-packages.txt")
  file_meta <- file.path(path_staging, "include-meta.txt")
  expect_false(file.exists(file_packages))
  expect_false(file.exists(file_meta))
  rclone_includes(path_staging)
  expect_true(file.exists(file_packages))
  expect_true(file.exists(file_meta))
  include_packages <- readLines(file_packages)
  expect_equal(
    sort(include_packages),
    sort(
      sprintf(
        c(
          "src/contrib/staged_2.0.5.tar.gz",
          "src/contrib/removed-no-issue_2.0.6.tar.gz",
          "bin/macosx/*/contrib/%s/staged_2.0.5.tgz",
          "bin/macosx/*/contrib/%s/removed-no-issue_2.0.6.tgz",
          "bin/windows/contrib/%s/staged_2.0.5.zip",
          "bin/windows/contrib/%s/removed-no-issue_2.0.6.zip"
        ),
        meta_snapshot()$r
      )
    )
  )
  include_meta <- readLines(file_meta)
  expect_equal(
    sort(include_meta),
    sort(
      sprintf(
        c(
          "src/contrib/PACKAGES",
          "src/contrib/PACKAGES.gz",
          "bin/macosx/*/contrib/%s/PACKAGES",
          "bin/macosx/*/contrib/%s/PACKAGES.gz",
          "bin/windows/contrib/%s/PACKAGES",
          "bin/windows/contrib/%s/PACKAGES.gz"
        ),
        meta_snapshot()$r
      )
    )
  )
})
