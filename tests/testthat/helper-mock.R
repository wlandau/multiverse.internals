mock_checks <- structure(
  list(
    Package = c(
      "tinytest", "tidytensor", "secretbase",
      "multiverse.internals", "SBC", "duckdb", "httpgd", "targetsketch",
      "stantargets", "zstdlite", "INLA", "audio.whisper", "tidypolars",
      "multitools", "audio.vadwebrtc", "nanonext", "polars", "cmdstanr",
      "string2path"
    ),
    "_user" = c(
      "r-multiverse", "r-multiverse", "r-multiverse",
      "r-multiverse", "r-multiverse", "r-multiverse", "r-multiverse",
      "r-multiverse", "r-multiverse", "r-multiverse", "r-multiverse",
      "r-multiverse", "r-multiverse", "r-multiverse", "r-multiverse",
      "r-multiverse", "r-multiverse", "r-multiverse", "r-multiverse"
    ),
    "_type" = c(
      "src", "src", "src", "src", "src", "src", "src",
      "src", "src", "src", "failure", "src", "src", "src", "src", "src",
      "src", "src", "src"
    ),
    "_status" = c(
      "success", "failure", "success",
      "success", "failure", "success", "success", "success", "success",
      "success", NA, "success", "success", "success", "success", "success",
      "success", "success", "success"
    ),
    "_winbinary" = c(
      "success", "success", "success", "success",
      "success", "success", "success",
      "success", "success", "success", NA, "success", "success", "success",
      "success", "success", "success", "success", "success"
    ),
    "_macbinary" = c(
      "success", "success", "success", "success",
      "success", "success", "success",
      "success", "success", "success", NA, "success", "success", "success",
      "success", "success", "arm64-failure", "success", "success"
    ),
    "_wasmbinary" = c(
      "success", "success", "success", "success",
      "success", "success", "none", "success", "success", "success",
      NA, "success", "success", "success", "success", "success",
      "none", "success", "none"
    ),
    "_linuxdevel" = c(
      "success", "failure", "success", "success",
      "failure", "success", "success",
      "success", "failure", "success", NA,
      "success", "success",
      "success", "success", "success",
      "failure", "success", "success"
    ),
    "_buildurl" = c(
      "https://github.com/r-universe/r-multiverse/actions/runs/8998731783",
      "https://github.com/r-universe/r-multiverse/actions/runs/8998732025",
      "https://github.com/r-universe/r-multiverse/actions/runs/8998731915",
      "https://github.com/r-universe/r-multiverse/actions/runs/8998732064",
      "https://github.com/r-universe/r-multiverse/actions/runs/8998731731",
      "https://github.com/r-universe/r-multiverse/actions/runs/8998731753",
      "https://github.com/r-universe/r-multiverse/actions/runs/8998732459",
      "https://github.com/r-universe/r-multiverse/actions/runs/8998732171",
      "https://github.com/r-universe/r-multiverse/actions/runs/8998732490",
      "https://github.com/r-universe/r-multiverse/actions/runs/8998732389",
      "https://github.com/r-universe/r-multiverse/actions/runs/8487512222",
      "https://github.com/r-universe/r-multiverse/actions/runs/8998732607",
      "https://github.com/r-universe/r-multiverse/actions/runs/8998732444",
      "https://github.com/r-universe/r-multiverse/actions/runs/8998731870",
      "https://github.com/r-universe/r-multiverse/actions/runs/8998732026",
      "https://github.com/r-universe/r-multiverse/actions/runs/8998732176",
      "https://github.com/r-universe/r-multiverse/actions/runs/9005231218",
      "https://github.com/r-universe/r-multiverse/actions/runs/9140511697",
      "https://github.com/r-universe/r-multiverse/actions/runs/8998732437"
    ),
    "_indexed" = c(
      FALSE, TRUE, FALSE, TRUE, TRUE, FALSE,
      FALSE, TRUE, FALSE, FALSE, NA, TRUE, TRUE, TRUE, FALSE, FALSE,
      TRUE, TRUE, FALSE
    ),
    "_binaries" = list(
      list(), list(), list(),
      list(), list(), list(), list(), list(), list(), list(),
      list(), list(), list(), list(), list(), list(), list(),
      list(), list()
    ),
    "_failure" = structure(
      list(
        buildurl = c(
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          paste0(
            "https://github.com/r-universe/r-multiverse",
            "/actions/runs/8487512222"
          ),
          NA, NA, NA, NA, NA, NA, NA, NA
        )
      ),
      class = "data.frame",
      row.names = c(NA, 19L)
    )
  ),
  class = "data.frame",
  row.names = c(NA, 19L)
)

mock_descriptions <- structure(
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

mock_versions <- function() {
  lines <- c(
    "[",
    " {",
    " \"package\": \"package_unmodified\",",
    " \"version_current\": \"1.0.0\",",
    " \"hash_current\": \"hash_1.0.0\",",
    " \"version_highest\": \"1.0.0\",",
    " \"hash_highest\": \"hash_1.0.0\"",
    " },",
    " {",
    " \"package\": \"version_decremented\",",
    " \"version_current\": \"0.0.1\",",
    " \"hash_current\": \"hash_0.0.1\",",
    " \"version_highest\": \"1.0.0\",",
    " \"hash_highest\": \"hash_1.0.0\"",
    " },",
    " {",
    " \"package\": \"version_incremented\",",
    " \"version_current\": \"2.0.0\",",
    " \"hash_current\": \"hash_2.0.0\",",
    " \"version_highest\": \"2.0.0\",",
    " \"hash_highest\": \"hash_2.0.0\"",
    " },",
    " {",
    " \"package\": \"version_unmodified\",",
    " \"version_current\": \"1.0.0\",",
    " \"hash_current\": \"hash_1.0.0-modified\",",
    " \"version_highest\": \"1.0.0\",",
    " \"hash_highest\": \"hash_1.0.0\"",
    " }",
    "]"
  )
  versions <- tempfile()
  writeLines(lines, versions)
  versions
}
