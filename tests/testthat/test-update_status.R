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
  json_staging <- jsonlite::read_json(file_staging, simplifyVector = TRUE)
  json_community <- jsonlite::read_json(file_community, simplifyVector = TRUE)
  skip_if_not_installed("gert")
  skip_if_offline()
  path_status <- tempfile()
  gert::git_clone(
    url = "https://github.com/r-multiverse/status",
    path = path_status
  )
  on.exit(unlink(path_status, recursive = TRUE), add = TRUE)
  stage_candidates(path_staging = path_staging)
  update_status(
    path_status = path_status,
    path_staging = path_staging,
    path_community = path_community
  )
  expect_true(
    all(
      file.exists(
        file.path(
          path_status,
          c("community.md", "staging.md", "production.md")
        )
      )
    )
  )
  lines_production <- readLines(file.path(path_status, "production.md"))
  expect_true(any(grepl(meta_snapshot()$snapshot, lines_production)))
  expect_true(any(grepl("[staged]", lines_production, fixed = TRUE)))
  expect_true(
    any(grepl("[removed-no-issue]", lines_production, fixed = TRUE))
  )
  expect_false(any(grepl("[issue]", lines_production, fixed = TRUE)))
  expect_false(
    any(grepl("[removed-has-issue]", lines_production, fixed = TRUE))
  )
  lines_staging <- readLines(file.path(path_status, "staging.md"))
  expect_true(any(grepl("[issue]", lines_staging, fixed = TRUE)))
  expect_true(any(grepl("[removed-has-issue]", lines_staging, fixed = TRUE)))
  expect_false(any(grepl("[staged]", lines_staging, fixed = TRUE)))
  lines_community <- readLines(file.path(path_status, "community.md"))
  expect_true(any(grepl("[issue]", lines_community, fixed = TRUE)))
  expect_false(
    any(grepl("[removed-has-issue]", lines_community, fixed = TRUE))
  )
  expect_false(any(grepl("[staged]", lines_community, fixed = TRUE)))
  out_staging <- file.path(path_status, "staging")
  out_community <- file.path(path_status, "community")
  expect_equal(
    sort(list.files(out_staging)),
    sort(
      c(
        paste0(json_staging$package, ".html"),
        paste0(json_staging$package, ".xml")
      )
    )
  )
  expect_equal(
    sort(list.files(out_community)),
    sort(
      c(
        paste0(json_community$package, ".html"),
        paste0(json_community$package, ".xml")
      )
    )
  )
  for (repo in c("staging", "community")) {
    expect_true(
      any(
        grepl(
          pattern = "R-multiverse checks passed",
          readLines(file.path(path_status, repo, "staged.html"))
        )
      )
    )
    expect_true(
      any(
        grepl(
          pattern = "staged: success",
          readLines(file.path(path_status, repo, "staged.html"))
        )
      )
    )
    expect_true(
      any(
        grepl(
          pattern = "sha-staged",
          readLines(file.path(path_status, repo, "staged.xml"))
        )
      )
    )
    expect_true(
      any(
        grepl(
          pattern = "issues found",
          readLines(file.path(path_status, repo, "issue.html"))
        )
      )
    )
    expect_true(
      any(
        grepl(
          pattern = "issue: issues found",
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
    path_community = path_community
  )
  expect_equal(
    sort(list.files(out_staging)),
    sort(
      c(
        paste0(json_staging$package, ".html"),
        paste0(json_staging$package, ".xml")
      )
    )
  )
  expect_equal(
    sort(list.files(out_community)),
    sort(
      c(
        paste0(json_community$package, ".html"),
        paste0(json_community$package, ".xml")
      )
    )
  )
})
