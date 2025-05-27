test_that("issues_r_cmd_check() mocked", {
  status <- issues_r_cmd_check(meta = mock_meta_packages)
  url <- "https://github.com/r-universe/r-multiverse/actions/runs"
  package <- c(
    "adbcbigquery",
    "adbcflightsql",
    "adbcsnowflake",
    "audio.whisper",
    "bridgestan",
    "colorout",
    "demographr",
    "geographr",
    "glaredb",
    "healthyr",
    "INLA",
    "loneliness",
    "Rvision",
    "SBC",
    "stantargets",
    "tidypolars",
    "tidytensor",
    "webseq",
    "wildfires"
  )
  r_cmd_check <- list(
    data.frame(
      check = c("WARNING", "WARNING"),
      config = c("macos-release-x86_64", "macos-release-arm64"),
      r = c("4.5.0", "4.5.0"),
      url = file.path(
        url,
        c(
          "14849535334/job/42165268088",
          "14849535334/job/42165268446"
        )
      )
    ),
    data.frame(
      check = c("WARNING", "WARNING"),
      config = c("macos-release-x86_64", "macos-release-arm64"),
      r = c("4.5.0", "4.5.0"),
      url = file.path(
        url,
        c(
          "14849539414/job/42165265697",
          "14849539414/job/42165266384"
        )
      )
    ),
    data.frame(
      check = c("WARNING", "WARNING"),
      config = c("macos-release-x86_64", "macos-release-arm64"),
      r = c("4.5.0", "4.5.0"),
      url = file.path(
        url,
        c(
          "14849541628/job/42165269426",
          "14849541628/job/42165270204"
        )
      )
    ),
    data.frame(
      check = c("WARNING", "WARNING", "WARNING", "WARNING", "WARNING"),
      config = c(
        "linux-release-x86_64", "windows-release", "linux-release-arm64",
        "macos-release-x86_64", "macos-release-arm64"
      ),
      r = c("4.5.0", "4.5.0", "4.5.0", "4.5.0", "4.5.0"),
      url = file.path(
        url,
        c(
          "14900366215/job/42170782320",
          "14900366215/job/42170782526",
          "14900366215/job/42170782712",
          "14900366215/job/42170782956",
          "14900366215/job/42170783436"
        )
      )
    ),
    data.frame(
      check = c("ERROR", "ERROR", "ERROR"),
      config = c("macos-release-arm64", "linux-release", "windows-release"),
      r = c("4.5.0", "4.5.0", "4.5.0"),
      url = file.path(
        url,
        c(
          "14711427981/job/42149749244",
          "14711427981/job/42149749646",
          "14711427981/job/42149750838"
        )
      )
    ),
    data.frame(
      check = "ERROR",
      config = "source",
      r = "4.5.0",
      url = file.path(url, "15014431467/job/42432169155")
    ),
    data.frame(
      check = c("WARNING", "WARNING", "WARNING"),
      config = c(
        "windows-release", "macos-release-arm64", "linux-release-x86_64"
      ),
      r = c("4.5.0", "4.5.0", "4.5.0"),
      url = file.path(
        url,
        c(
          "14835604718/job/42161918184",
          "14835604718/job/42161918560",
          "14835604718/job/42161919749"
        )
      )
    ),
    data.frame(
      check = c("WARNING", "WARNING", "WARNING"),
      config = c(
        "linux-release-x86_64", "macos-release-arm64", "windows-release"
      ),
      r = c("4.5.0", "4.5.0", "4.5.0"),
      url = file.path(
        url,
        c(
          "14952288872/job/42165926505",
          "14952288872/job/42165926777",
          "14952288872/job/42165927735"
        )
      )
    ),
    data.frame(
      check = c("WARNING", "WARNING"),
      config = c("macos-release-x86_64", "macos-release-arm64"),
      r = c("4.5.0", "4.5.0"),
      url = file.path(
        url,
        c(
          "15128408401/job/42679091499",
          "15128408401/job/42679091642"
        )
      )
    ),
    data.frame(
      check = c("WARNING", "WARNING", "WARNING"),
      config = c(
        "windows-release", "macos-release-arm64", "linux-release-x86_64"
      ),
      r = c("4.5.0", "4.5.0", "4.5.0"),
      url = file.path(
        url,
        c(
          "14942941830/job/42166611691",
          "14942941830/job/42166612634",
          "14942941830/job/42166613147"
        )
      )
    ),
    data.frame(
      check = "ERROR",
      config = "source",
      r = "4.5.0",
      url = file.path(url, "15014459343/job/42432249096")
    ),
    data.frame(
      check = c("WARNING", "WARNING", "WARNING"),
      config = c("macos-release-arm64", "linux-release", "windows-release"),
      r = c("4.5.0", "4.5.0", "4.5.0"),
      url = file.path(
        url,
        c(
          "14782719153/job/42151614403",
          "14782719153/job/42151615228",
          "14782719153/job/42151617108"
        )
      )
    ),
    data.frame(
      check = "ERROR",
      config = "source",
      r = "4.5.0",
      url = file.path(url, "15014469226/job/42432235494")
    ),
    data.frame(
      check = c("ERROR", "WARNING", "WARNING", "WARNING"),
      config = c(
        "source", "macos-release-arm64",
        "linux-release-x86_64", "windows-release"
      ),
      r = c("4.5.0", "4.5.0", "4.5.0", "4.5.0"),
      url = file.path(
        url,
        c(
          "14991919293/job/42139781927",
          "14991919293/job/42139785853",
          "14991919293/job/42139786447",
          "14991919293/job/42139789083"
        )
      )
    ),
    data.frame(
      check = c("ERROR", "WARNING", "WARNING", "WARNING"),
      config = c(
        "source", "macos-release-arm64",
        "linux-release-x86_64", "windows-release"
      ),
      r = c("4.5.0", "4.5.0", "4.5.0", "4.5.0"),
      url = file.path(
        url,
        c(
          "14991949490/job/42146211963",
          "14991949490/job/42146215626",
          "14991949490/job/42146216138",
          "14991949490/job/42146217621"
        )
      )
    ),
    data.frame(
      check = "ERROR",
      config = "windows-release",
      r = "4.5.0",
      url = file.path(url, "14698253111/job/42149249218")
    ),
    data.frame(
      check = c("ERROR", "WARNING", "WARNING", "WARNING"),
      config = c(
        "source", "linux-release-x86_64",
        "macos-release-arm64", "windows-release"
      ),
      r = c("4.5.0", "4.5.0", "4.5.0", "4.5.0"),
      url = file.path(
        url,
        c(
          "15177117589/job/42679311345",
          "15177117589/job/42679418225",
          "15177117589/job/42679418740",
          "15177117589/job/42679418961"
        )
      )
    ),
    data.frame(
      check = c("WARNING", "WARNING", "WARNING"),
      config = c("linux-release", "macos-release-arm64", "windows-release"),
      r = c("4.5.0", "4.5.0", "4.5.0"),
      url = file.path(
        url,
        c(
          "14788234736/job/42153194900",
          "14788234736/job/42153195174",
          "14788234736/job/42153196053"
        )
      )
    ),
    data.frame(
      check = c("WARNING", "WARNING", "WARNING"),
      config = c("linux-release", "macos-release-arm64", "windows-release"),
      r = c("4.5.0", "4.5.0", "4.5.0"),
      url = file.path(
        url,
        c(
          "14817360103/job/42157252491",
          "14817360103/job/42157253094",
          "14817360103/job/42157253512"
        )
      )
    )
  )
  expect_equal(sort(colnames(status)), sort(c("package", "r_cmd_check")))
  expect_equal(status$package, package)
  expect_equal(status$r_cmd_check, r_cmd_check)
})

test_that("issues_r_cmd_check() on a small repo", {
  meta <- meta_packages(repo = "https://wlandau.r-universe.dev")
  status <- issues_r_cmd_check(meta = meta)
  expect_true(is.list(status))
})
