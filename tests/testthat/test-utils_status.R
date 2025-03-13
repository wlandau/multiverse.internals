test_that("status_list() handles missing and empty values correctly", {
  meta <- data.frame(
    package = c("audio.whisper", "readxl", "stantargets", "tidypolars"),
    advisory = c(
      NA_character_,
      "url",
      NA_character_,
      NA_character_
    )
  )
  meta$remotes <- list(
    "bnosac/audio.vadwebrtc",
    NULL,
    c("hyunjimoon/SBC", "stan-dev/cmdstanr"),
    "markvanderloo/tinytest/pkg"
  )
  out <- status_list(meta)
  exp <- list(
    audio.whisper = list(remotes = "bnosac/audio.vadwebrtc"),
    readxl = list(advisory = "url"),
    stantargets = list(remotes = c("hyunjimoon/SBC", "stan-dev/cmdstanr")),
    tidypolars = list(remotes = "markvanderloo/tinytest/pkg")
  )
  expect_equal(out, exp)
})
