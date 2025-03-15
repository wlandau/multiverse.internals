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
