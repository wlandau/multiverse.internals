# parse(text = deparse(meta_checks(repo = "https://multiverse.r-multiverse.org"))) # nolint
mock_meta_checks <- structure(
  list(
    package = c(
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

# parse(text = deparse(meta_packages(repo = "https://multiverse.r-multiverse.org")) # nolint
mock_meta_packages <- structure(
  list(
    package = c(
      "SBC", "audio.vadwebrtc", "audio.whisper",
      "cmdstanr", "duckdb", "httpgd", "multitools", "multiverse.internals",
      "nanonext", "polars", "secretbase", "stantargets", "string2path",
      "targetsketch", "tidypolars", "tidytensor", "tinytest", "zstdlite"
    ),
    version = c(
      "0.3.0.9000", "0.2", "0.4.1", "0.8.0", "0.10.1",
      "2.0.1", "0.1.0", "0.1.4", "1.0.0", "0.16.4", "0.5.0", "0.1.1",
      "0.1.6", "0.0.1", "0.7.0", "1.0.0", "1.4.1.1", "0.2.6"
    ),
    remotes = list(
      NULL, NULL, "bnosac/audio.vadwebrtc", NULL, NULL, NULL, NULL,
      NULL, NULL, NULL, NULL,
      c("hyunjimoon/SBC", "stan-dev/cmdstanr", ""),
      NULL, NULL, "markvanderloo/tinytest/pkg", NULL, NULL, NULL
    )
  ),
  row.names = c(NA, 18L),
  class = "data.frame"
)

# parse(text = deparse(meta_packages(repo = "https://wlandau.r-universe.dev")) # nolint
mock_meta_packages_graph <- structure(
  list(
    "_id" = c(
      "666319a14e86770016661a28", "66580c6dd014ca0014e5afc2",
      "665179088d83a20014a4037a", "6666d6575e691000165322b1",
      "6666d6865e691000165322fe"
    ),
    package = c(
      "crew", "crew.aws.batch",
      "crew.cluster", "mirai", "nanonext"
    ),
    version = c(
      "0.9.3.9002", "0.0.5.9000", "0.3.1", "1.1.0.9000", "1.1.0.9000"
    ),
    license = c(
      "MIT + file LICENSE", "MIT + file LICENSE",
      "MIT + file LICENSE", "GPL (>= 3)", "GPL (>= 3)"
    ),
    remotesha = c(
      "eafad0276c06dec2344da2f03596178c754c8b5e",
      "4d9e5b44e2942d119af963339c48d134e84de458",
      "d4ac61fd9a1d9539088ffebdadcd4bb713c25ee1",
      "7015695b7ef82f82ab3225ac2d226b2c8f298097",
      "85dd672a44a92c890eb40ea9ebab7a4e95335c2f"
    ),
    needscompilation = c("no", "no", "no", "no", "yes"),
    md5sum = c(
      "642e9ac93d39d462ee6f95d8522afbe1",
      "23c2733447eae95614fd6af1889b57e8",
      "5707d786c1aa3848fd37e6bf4598ea4c",
      "3c92053c75031ec0b976b0a15185e3a0",
      "1507b3a27da7dff5d9acbf8ef181ad78"
    ),
    "_type" = c("src", "src", "src", "src", "src"),
    "_dependencies" = list(
      structure(
        list(
          package = c(
            "R", "cli", "data.table", "getip", "later", "mirai", "nanonext",
            "processx", "promises", "ps", "R6", "rlang", "stats", "tibble",
            "tidyselect", "tools", "utils", "knitr", "markdown", "rmarkdown",
            "testthat"
          ),
          version = c(
            ">= 4.0.0", ">= 3.1.0", NA, NA, NA, ">= 0.12.0", ">= 0.12.0",
            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ">= 1.30", ">= 1.1",
            ">= 2.4", ">= 3.0.0"
          ),
          role = c(
            "Depends", "Imports", "Imports", "Imports", "Imports", "Imports",
            "Imports", "Imports", "Imports", "Imports", "Imports", "Imports",
            "Imports", "Imports", "Imports", "Imports", "Imports", "Suggests",
            "Suggests", "Suggests", "Suggests"
          )
        ),
        class = "data.frame",
        row.names = c(NA, 21L)
      ),
      structure(
        list(
          package = c(
            "R", "cli", "crew", "paws.common", "paws.compute",
            "paws.management", "R6", "rlang", "tibble", "utils",
            "knitr", "markdown", "rmarkdown", "testthat"
          ),
          version = c(
            ">= 4.0.0", ">= 3.1.0", ">= 0.8.0", ">= 0.7.0",
            NA, NA, NA, NA, NA,  NA, ">= 1.30",
            ">= 1.1", ">= 2.4", ">= 3.0.0"
          ),
          role = c(
            "Depends", "Imports", "Imports", "Imports",
            "Imports", "Imports", "Imports", "Imports",
            "Imports", "Imports", "Suggests", "Suggests",
            "Suggests", "Suggests"
          )
        ),
        class = "data.frame",
        row.names = c(NA, 14L)
      ),
      structure(
        list(
          package = c(
            "R", "crew", "ps", "lifecycle", "R6", "rlang", "utils", "vctrs",
            "xml2", "yaml", "knitr", "markdown", "rmarkdown", "testthat"
          ),
          version = c(
            ">= 4.0.0", ">= 0.8.0", NA, NA, NA, NA, NA, NA, NA, NA,
            ">= 1.30", ">= 1.1", ">= 2.4", ">= 3.0.0"
          ),
          role = c(
            "Depends", "Imports", "Imports", "Imports", "Imports",
            "Imports", "Imports", "Imports", "Imports", "Imports",
            "Suggests", "Suggests", "Suggests", "Suggests"
          )
        ),
        class = "data.frame",
        row.names = c(NA, 14L)
      ),
      structure(
        list(
          package = c(
            "R", "nanonext", "knitr", "markdown", "parallel", "promises"
          ),
          version = c(">= 3.6", ">= 1.1.0.9000", NA, NA, NA, NA),
          role = c(
            "Depends", "Imports", "Suggests",
            "Suggests", "Enhances", "Enhances"
          )
        ),
        class = "data.frame",
        row.names = c(NA, 6L)
      ),
      structure(
        list(
          package = c("R", "later", "later", "knitr", "markdown"),
          version = c(">= 3.5", NA, NA, NA, NA),
          role = c("Depends", "LinkingTo", "Imports", "Suggests", "Suggests")
        ),
        class = "data.frame",
        row.names = c(NA, 5L)
      )
    ),
    distro = c("noble", "noble", "noble", "noble", "noble")
  ),
  class = "data.frame",
  row.names = c(NA, 5L)
)

# From tests/testthat/test-record_versions.R
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
