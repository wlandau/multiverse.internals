test_that("status_descriptions() mocked", {
  status <- status_descriptions(meta = mock_meta_packages)
  expected <- list(
    asylum = list(cran = "1.1.2"),
    audio.whisper = list(remotes = "bnosac/audio.vadwebrtc"), 
    IMD = list(cran = "1.2.2"),
    stantargets = list(
      remotes = c("hyunjimoon/SBC", "stan-dev/cmdstanr")
    )
  )
  names <- sort(names(status))
  expect_equal(names, sort(names(expected)))
  expect_equal(status[names], expected[names])
})

test_that("status_descriptions() on a small repo", {
  meta <- meta_packages(repo = "https://wlandau.r-universe.dev")
  status <- status_descriptions(meta = meta)
  expect_true(is.list(status))
})

test_that("status_descriptions() with security advisories", {
  example <- mock_meta_packages$package == "nanonext"
  commonmark <- mock_meta_packages[example,, drop = FALSE] # nolint
  commonmark$package <- "commonmark"
  commonmark$version <- "0.2"
  readxl <- mock_meta_packages[example,, drop = FALSE] # nolint
  readxl$package <- "readxl"
  readxl$version <- "1.4.1"
  meta <- rbind(
    mock_meta_packages,
    commonmark,
    readxl
  )
  out <- status_descriptions(meta)
  expect_equal(
    out$commonmark$advisories,
    file.path(
      "https://github.com/RConsortium/r-advisory-database",
      "blob/main/vulns/commonmark",
      c("RSEC-2023-6.yaml", "RSEC-2023-7.yaml", "RSEC-2023-8.yaml")
    )
  )
})
