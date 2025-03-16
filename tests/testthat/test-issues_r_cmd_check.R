test_that("issues_r_cmd_check() mocked", {
  status <- issues_r_cmd_check(meta = mock_meta_packages)
  url <- "https://github.com/r-universe/r-multiverse/actions/runs"
  expected <- list(audio.whisper = list(
    url_r_cmd_check =
      "https://github.com/r-universe/r-multiverse/actions/runs/13631287847",
    issues_r_cmd_check = list(
      "linux x86_64 R-4.4.3" = "WARNING",
      "mac aarch64 R-4.4.3" = "WARNING", "mac x86_64 R-4.4.3" = "WARNING",
      "win x86_64 R-4.4.3" = "WARNING"
    )
  ), bridgestan = list(
    url_r_cmd_check =
      "https://github.com/r-universe/r-multiverse/actions/runs/13631289190",
    issues_r_cmd_check = list(
      "linux R-4.4.3" = "ERROR", "mac R-4.4.3" = "ERROR",
      "win R-4.4.3" = "ERROR"
    )
  ), colorout = list(
    url_r_cmd_check =
      "https://github.com/r-universe/r-multiverse/actions/runs/13631286813",
    issues_r_cmd_check = list(
      "mac aarch64 R-4.4.3" = "WARNING", "mac x86_64 R-4.4.3" = "WARNING",
      win = "MISSING"
    )
  ), demographr = list(
    url_r_cmd_check =
      "https://github.com/r-universe/r-multiverse/actions/runs/13631287991",
    issues_r_cmd_check = list(
      "linux R-4.4.3" = "WARNING", "mac R-4.4.3" = "WARNING",
      "win R-4.4.3" = "WARNING"
    )
  ), geographr = list(
    url_r_cmd_check =
      "https://github.com/r-universe/r-multiverse/actions/runs/13631287654",
    issues_r_cmd_check = list(
      "linux R-4.4.3" = "WARNING", "mac R-4.4.3" = "WARNING",
      "win R-4.4.3" = "WARNING"
    )
  ), glaredb = list(
    url_r_cmd_check =
      "https://github.com/r-universe/r-multiverse/actions/runs/13631287270",
    issues_r_cmd_check = list(win = "MISSING")
  ), healthyr = list(
    url_r_cmd_check =
      "https://github.com/r-universe/r-multiverse/actions/runs/13631288201",
    issues_r_cmd_check = list(
      "linux R-4.4.3" = "WARNING", "mac R-4.4.3" = "WARNING",
      "win R-4.4.3" = "WARNING"
    )
  ), INLA = list(
    url_r_cmd_check =
      "https://github.com/r-universe/r-multiverse/actions/runs/13631286185",
    issues_r_cmd_check = list(
      linux = "MISSING", mac = "MISSING",
      win = "MISSING", source = "FAILURE"
    )
  ), loneliness = list(
    url_r_cmd_check =
      "https://github.com/r-universe/r-multiverse/actions/runs/13631286586",
    issues_r_cmd_check = list(
      "linux R-4.4.3" = "WARNING", "mac R-4.4.3" = "WARNING",
      "win R-4.4.3" = "WARNING"
    )
  ), multiverse.internals = list(
    url_r_cmd_check =
      "https://github.com/r-universe/r-multiverse/actions/runs/13860842188",
    issues_r_cmd_check = list("linux R-4.4.3" = "ERROR")
  ), SBC = list(
    url_r_cmd_check =
      "https://github.com/r-universe/r-multiverse/actions/runs/13631288318",
    issues_r_cmd_check = list(
      "linux R-4.4.3" = "WARNING", "mac R-4.4.3" = "WARNING",
      "win R-4.4.3" = "WARNING"
    )
  ), stantargets = list(
    url_r_cmd_check =
      "https://github.com/r-universe/r-multiverse/actions/runs/13631287435",
    issues_r_cmd_check = list(
      "linux R-4.4.3" = "WARNING", "mac R-4.4.3" = "WARNING",
      "win R-4.4.3" = "WARNING"
    )
  ), targets = list(
    url_r_cmd_check =
      "https://github.com/r-universe/r-multiverse/actions/runs/13845626109",
    issues_r_cmd_check = list("mac R-4.4.3" = "ERROR")
  ), tidypolars = list(
    url_r_cmd_check =
      "https://github.com/r-universe/r-multiverse/actions/runs/13771935997",
    issues_r_cmd_check = list("win R-4.4.3" = "ERROR")
  ), tidytensor = list(
    url_r_cmd_check =
      "https://github.com/r-universe/r-multiverse/actions/runs/13631289402",
    issues_r_cmd_check = list(
      "linux R-4.4.3" = "WARNING", "mac R-4.4.3" = "WARNING",
      "win R-4.4.3" = "WARNING"
    )
  ), webseq = list(
    url_r_cmd_check =
      "https://github.com/r-universe/r-multiverse/actions/runs/13631287034",
    issues_r_cmd_check = list(
      "linux R-4.4.3" = "WARNING", "mac R-4.4.3" = "WARNING",
      "win R-4.4.3" = "WARNING"
    )
  ), wildfires = list(
    url_r_cmd_check =
      "https://github.com/r-universe/r-multiverse/actions/runs/13631287153",
    issues_r_cmd_check = list(
      "linux R-4.4.3" = "WARNING", "mac R-4.4.3" = "WARNING",
      "win R-4.4.3" = "WARNING"
    )
  ))
  expect_equal(status[order(names(status))], expected[order(names(expected))])
})

test_that("issues_r_cmd_check() on a small repo", {
  meta <- meta_packages(repo = "https://wlandau.r-universe.dev")
  status <- issues_r_cmd_check(meta = meta)
  expect_true(is.list(status))
})
