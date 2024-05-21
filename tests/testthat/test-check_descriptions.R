test_that("check_descriptions() mocked", {
  issues <- check_descriptions(mock = mock_descriptions)
  expected <- list(
    audio.whisper = list(remotes = list("bnosac/audio.vadwebrtc")), 
    stantargets = list(
      remotes = list(c("hyunjimoon/SBC", "stan-dev/cmdstanr"))
    ),
    tidypolars = list(remotes = list("markvanderloo/tinytest/pkg"))
  )
  expect_equal(issues, expected)
})

test_that("check_descriptions() on a small repo", {
  issues <- check_descriptions(repo = "https://wlandau.r-universe.dev")
  expect_true(is.list(issues))
  expect_named(issues)
})
