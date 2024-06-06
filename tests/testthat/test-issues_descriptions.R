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
