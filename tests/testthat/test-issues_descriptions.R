test_that("issues_descriptions() mocked", {
  issues <- issues_descriptions(meta = mock_meta_packages)
  expected <- list(
    audio.whisper = list(remotes = "bnosac/audio.vadwebrtc"),
    stantargets = list(
      remotes = c("hyunjimoon/SBC", "stan-dev/cmdstanr")
    ),
    tidypolars = list(remotes = "markvanderloo/tinytest/pkg")
  )
  expect_equal(issues, expected)
})

test_that("issues_descriptions() on a small repo", {
  meta <- meta_packages(repo = "https://wlandau.r-universe.dev")
  issues <- issues_descriptions(meta = meta)
  expect_true(is.list(issues))
})

test_that("issues_descriptions() with security advisories", {
  example <- mock_meta_packages$package == "nanonext"
  readxl <- mock_meta_packages[example,, drop = FALSE] # nolint
  readxl$package <- "readxl"
  readxl$version <- "1.4.1"
  meta <- rbind(mock_meta_packages, readxl)
  out <- issues_descriptions(meta)
  url <- file.path(
    "https://github.com/RConsortium/r-advisory-database",
    "blob/main/vulns/readxl/RSEC-2023-2.yaml"
  )
  exp <- list(
    audio.whisper = list(remotes = "bnosac/audio.vadwebrtc"), 
    readxl = list(advisory = url), 
    stantargets = list(remotes = c("hyunjimoon/SBC", "stan-dev/cmdstanr")),
    tidypolars = list(remotes = "markvanderloo/tinytest/pkg")
  )
  expect_equal(out, exp)
})
