test_that("issues_r_cmd_check() mocked", {
  status <- issues_r_cmd_check(meta = mock_meta_packages)
  url <- "https://github.com/r-universe/r-multiverse/actions/runs"
  expected <- structure(list(package = c(
    "audio.whisper", "bridgestan", "colorout",
    "demographr", "geographr", "glaredb", "healthyr", "INLA", "loneliness",
    "multiverse.internals", "SBC", "stantargets", "targets", "tidypolars",
    "tidytensor", "webseq", "wildfires"
  ), r_cmd_check = list(
    list(
      issues = list(
        "linux x86_64 R-4.4.3" = "WARNING", "mac aarch64 R-4.4.3" = "WARNING",
        "mac x86_64 R-4.4.3" = "WARNING", "win x86_64 R-4.4.3" = "WARNING"
      ),
      url =
        "https://github.com/r-universe/r-multiverse/actions/runs/13631287847"
    ),
    list(issues = list(
      "linux R-4.4.3" = "ERROR", "mac R-4.4.3" = "ERROR",
      "win R-4.4.3" = "ERROR"
    ), url =
      "https://github.com/r-universe/r-multiverse/actions/runs/13631289190"),
    list(issues = list(
      "mac aarch64 R-4.4.3" = "WARNING", "mac x86_64 R-4.4.3" = "WARNING",
      win = "MISSING"
    ), url =
      "https://github.com/r-universe/r-multiverse/actions/runs/13631286813"),
    list(issues = list(
      "linux R-4.4.3" = "WARNING", "mac R-4.4.3" = "WARNING",
      "win R-4.4.3" = "WARNING"
    ), url =
      "https://github.com/r-universe/r-multiverse/actions/runs/13631287991"),
    list(issues = list(
      "linux R-4.4.3" = "WARNING", "mac R-4.4.3" = "WARNING",
      "win R-4.4.3" = "WARNING"
    ), url =
      "https://github.com/r-universe/r-multiverse/actions/runs/13631287654"),
    list(
      issues = list(win = "MISSING"),
      url =
        "https://github.com/r-universe/r-multiverse/actions/runs/13631287270"
    ),
    list(issues = list(
      "linux R-4.4.3" = "WARNING", "mac R-4.4.3" = "WARNING",
      "win R-4.4.3" = "WARNING"
    ), url =
      "https://github.com/r-universe/r-multiverse/actions/runs/13631288201"
    ),
    list(issues = list(
      linux = "MISSING", mac = "MISSING", win = "MISSING",
      source = "FAILURE"
    ), url =
      "https://github.com/r-universe/r-multiverse/actions/runs/13631286185"
    ),
    list(issues = list(
      "linux R-4.4.3" = "WARNING", "mac R-4.4.3" = "WARNING",
      "win R-4.4.3" = "WARNING"
    ), url =
      "https://github.com/r-universe/r-multiverse/actions/runs/13631286586"
    ),
    list(
      issues = list("linux R-4.4.3" = "ERROR"),
      url =
        "https://github.com/r-universe/r-multiverse/actions/runs/13860842188"
    ),
    list(issues = list(
      "linux R-4.4.3" = "WARNING", "mac R-4.4.3" = "WARNING",
      "win R-4.4.3" = "WARNING"
    ),
    url =
      "https://github.com/r-universe/r-multiverse/actions/runs/13631288318"
    ),
    list(issues = list(
      "linux R-4.4.3" = "WARNING", "mac R-4.4.3" = "WARNING",
      "win R-4.4.3" = "WARNING"
    ),
    url =
      "https://github.com/r-universe/r-multiverse/actions/runs/13631287435"
    ),
    list(
      issues = list("mac R-4.4.3" = "ERROR"),
      url =
        "https://github.com/r-universe/r-multiverse/actions/runs/13845626109"
    ),
    list(
      issues = list("win R-4.4.3" = "ERROR"),
      url =
        "https://github.com/r-universe/r-multiverse/actions/runs/13771935997"
    ),
    list(
      issues = list(
        "linux R-4.4.3" = "WARNING", "mac R-4.4.3" = "WARNING",
        "win R-4.4.3" = "WARNING"
      ),
      url =
        "https://github.com/r-universe/r-multiverse/actions/runs/13631289402"
    ),
    list(
      issues = list(
        "linux R-4.4.3" = "WARNING", "mac R-4.4.3" = "WARNING",
        "win R-4.4.3" = "WARNING"
      ),
      url =
        "https://github.com/r-universe/r-multiverse/actions/runs/13631287034"
    ),
    list(issues = list(
      "linux R-4.4.3" = "WARNING", "mac R-4.4.3" = "WARNING",
      "win R-4.4.3" = "WARNING"
    ),
    url =
      "https://github.com/r-universe/r-multiverse/actions/runs/13631287153"
    )
  )), row.names = c(
    NA,
    -17L
  ), class = "data.frame")
  expect_equal(status[order(names(status))], expected[order(names(expected))])
})

test_that("issues_r_cmd_check() on a small repo", {
  meta <- meta_packages(repo = "https://wlandau.r-universe.dev")
  status <- issues_r_cmd_check(meta = meta)
  expect_true(is.list(status))
})
