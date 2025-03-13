test_that("status_checks() mocked", {
  status <- status_checks(meta = mock_meta_checks)
  url <- "https://github.com/r-universe/r-multiverse/actions/runs"
  expected <- list(
    audio.whisper = list(
      url = file.path(url, "12103194809"),
      issues = list(
        "linux x86_64 R-4.5.0" = "WARNING",
        "mac aarch64 R-4.4.2" = "WARNING",
        "mac x86_64 R-4.4.2" = "WARNING",
        "win x86_64 R-4.4.2" = "WARNING"
      )
    ),
    colorout = list(
      url = file.path(url, "12063016496"),
      issues = list(
        "mac aarch64 R-4.4.2" = "WARNING",
        "mac x86_64 R-4.4.2" = "WARNING",
        "win R-release" = "MISSING"
      )
    ),
    demographr = list(
      url = file.path(url, "11898760503"),
      issues = list(
        "linux R-4.5.0" = "WARNING",
        "mac R-4.4.2" = "WARNING",
        "win R-4.4.2" = "WARNING"
      )
    ),
    geographr = list(
      url = file.path(url, "11898762523"),
      issues = list(
        "linux R-4.5.0" = "WARNING",
        "mac R-4.4.2" = "WARNING",
        "win R-4.4.2" = "WARNING"
      )
    ),
    glaredb = list(
      url = file.path(url, "11876493507"),
      issues = list("win R-release" = "MISSING")
    ),
    healthyr = list(
      url = file.path(url, "11898763290"),
      issues = list(
        "linux R-4.5.0" = "WARNING",
        "mac R-4.4.2" = "WARNING",
        "win R-4.4.2" = "WARNING"
      )
    ),
    igraph = list(
      url = file.path(url, "12133807748"),
      issues = list("win x86_64 R-4.4.2" = "WARNING")
    ),
    INLA = list(
      url = file.path(url, "11566311732"),
      issues = list(
        "linux R-devel" = "MISSING",
        "mac R-release" = "MISSING",
        "win R-release" = "MISSING"
      )
    ),
    loneliness = list(
      url = file.path(url, "11898763908"),
      issues = list(
        "linux R-4.5.0" = "WARNING",
        "mac R-4.4.2" = "WARNING",
        "win R-4.4.2" = "WARNING"
      )
    ),
    prqlr = list(
      url = file.path(url, "11620617366"),
      issues = list(
        "mac R-release" = "MISSING",
        "win R-release" = "MISSING"
      )
    ),
    SBC = list(
      url = file.path(url, "11947266936"),
      issues = list(
        "linux R-4.5.0" = "WARNING",
        "mac R-4.4.2" = "WARNING",
        "win R-4.4.2" = "WARNING"
      )
    ),
    stantargets = list(
      url = file.path(url, "12139784185"),
      issues = list(
        "linux R-4.5.0" = "WARNING",
        "mac R-4.4.2" = "WARNING",
        "win R-4.4.2" = "WARNING"
      )
    ),
    taxizedb = list(
      url = file.path(url, "11825909535"),
      issues = list(
        "mac R-4.4.2" = "ERROR",
        "win R-4.4.2" = "ERROR"
      )
    ),
    tidytensor = list(
      url = file.path(url, "12133663821"),
      issues = list(
        "linux R-4.5.0" = "WARNING",
        "mac R-4.4.2" = "WARNING",
        "win R-4.4.2" = "WARNING"
      )
    ),
    webseq = list(
      url = file.path(url, "11825909757"),
      issues = list(
        "linux R-4.5.0" = "WARNING",
        "mac R-4.4.2" = "WARNING",
        "win R-4.4.2" = "WARNING"
      )
    ),
    wildfires = list(
      url = file.path(url, "11898765070"),
      issues = list(
        "linux R-4.5.0" = "WARNING",
        "mac R-4.4.2" = "WARNING",
        "win R-4.4.2" = "WARNING"
      )
    )
  )
  expect_equal(status[order(names(status))], expected[order(names(expected))])
})

test_that("status_checks() on a small repo", {
  meta <- meta_checks(repo = "https://wlandau.r-universe.dev")
  status <- status_checks(meta = meta)
  expect_true(is.list(status))
})
