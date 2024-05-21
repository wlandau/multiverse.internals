test_that("check_descriptions() in a mock case", {
  index <- structure(
    list(
      Package = c(
        "SBC", "audio.vadwebrtc", "audio.whisper", 
        "cmdstanr", "duckdb", "httpgd", "multitools", "multiverse.internals", 
        "nanonext", "polars", "secretbase", "stantargets", "string2path", 
        "targetsketch", "tidypolars", "tidytensor", "tinytest", "zstdlite"
      ),
      Version = c(
        "0.3.0.9000", "0.2", "0.4.1", "0.8.0", "0.10.1", 
        "2.0.1", "0.1.0", "0.1.4", "1.0.0", "0.16.4", "0.5.0", "0.1.1", 
        "0.1.6", "0.0.1", "0.7.0", "1.0.0", "1.4.1.1", "0.2.6"
      ),
      Remotes = list(
        NULL, NULL, "bnosac/audio.vadwebrtc", NULL, NULL, NULL, NULL, 
        NULL, NULL, NULL, NULL,
        c("hyunjimoon/SBC", "stan-dev/cmdstanr", ""),
        NULL, NULL, "markvanderloo/tinytest/pkg", NULL, NULL, NULL
      )
    ),
    row.names = c(NA, 18L),
    class = "data.frame"
  )
  issues <- check_descriptions(index = index)
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
