test_that("update_status()", {
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
    remotesha = paste0("sha-", names_staging)
  )
  meta_community <- data.frame(
    package = names_community,
    remotesha = paste0("sha-", names_community)
  )
  path_status <- tempfile()
  on.exit(unlink(path_status, recursive = TRUE), add = TRUE)
  update_status(
    path_status = path_status,
    path_staging = path_staging,
    path_community = path_community,
    mock = list(staging = meta_staging, community = meta_community)
  )
  expect_true(
    all(
      file.exists(file.path(path_status, c("community.html", "staging.html")))
    )
  )
  lines_staging <- readLines(file.path(path_status, "staging.html"))
  expect_true(any(grepl(">issue<", lines_staging, fixed = TRUE)))
  expect_true(any(grepl(">removed-has-issue<", lines_staging, fixed = TRUE)))
  expect_false(any(grepl(">freeze<", lines_staging, fixed = TRUE)))
  out_staging <- file.path(path_status, "staging")
  out_community <- file.path(path_status, "community")
  expect_equal(
    sort(list.files(out_staging)),
    sort(
      c(
        paste0(names_staging, ".html"),
        paste0(names_staging, ".xml")
      )
    )
  )
  expect_equal(
    sort(list.files(out_community)),
    sort(
      c(
        paste0(names_community, ".html"),
        paste0(names_community, ".xml")
      )
    )
  )
  for (repo in c("staging", "community")) {
    expect_true(
      any(
        grepl(
          pattern = "R-multiverse checks passed",
          readLines(file.path(path_status, repo, "freeze.html"))
        )
      )
    )
    expect_true(
      any(
        grepl(
          pattern = "sha-freeze",
          readLines(file.path(path_status, repo, "freeze.xml"))
        )
      )
    )
    expect_true(
      any(
        grepl(
          pattern = "found issues",
          readLines(file.path(path_status, repo, "issue.html"))
        )
      )
    )
    expect_true(
      any(
        grepl(
          pattern = "issues found",
          readLines(file.path(path_status, repo, "issue.xml"))
        )
      )
    )
    expect_true(
      any(
        grepl(
          pattern = "sha-issue",
          readLines(file.path(path_status, repo, "issue.xml"))
        )
      )
    )
  }
  update_status(
    path_status = path_status,
    path_staging = path_staging,
    path_community = path_community,
    mock = list(staging = meta_staging[1L, ], community = meta_community[1L, ])
  )
  expect_equal(
    sort(list.files(out_staging)),
    sort(paste0(meta_staging[1L, "package"], c(".html", ".xml")))
  )
  expect_equal(
    sort(list.files(out_community)),
    sort(sort(paste0(meta_community[1L, "package"], c(".html", ".xml"))))
  )
})
