test_that("check_checks() mock index", {
  index <- structure(
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
        TRUE, TRUE, FALSE),
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
  issues <- check_checks(index = index)
  url <- "https://github.com/r-universe/r-multiverse/actions/runs"
  expected <- list(
    httpgd = list(
      "_linuxdevel" = "success", "_macbinary" = "success", 
      "_wasmbinary" = "none", "_winbinary" = "success", "_status" = "success", 
      "_buildurl" = file.path(url, "8998732459")
    ), 
    INLA = list(
      "_linuxdevel" = "src-failure", "_macbinary" = "src-failure", 
      "_wasmbinary" = "src-failure", "_winbinary" = "src-failure", 
      "_status" = "src-failure",
      "_buildurl" = file.path(url, "8487512222")
    ), 
    polars = list(
      "_linuxdevel" = "failure", "_macbinary" = "arm64-failure", 
      "_wasmbinary" = "none", "_winbinary" = "success", "_status" = "success", 
      "_buildurl" = file.path(url, "9005231218")
    ), 
    SBC = list(
      "_linuxdevel" = "failure", "_macbinary" = "success",
      "_wasmbinary" = "success", "_winbinary" = "success",
      "_status" = "failure",
      "_buildurl" = file.path(url, "8998731731")
    ), 
    stantargets = list(
      "_linuxdevel" = "failure", "_macbinary" = "success", 
      "_wasmbinary" = "success", "_winbinary" = "success", 
      "_status" = "success",
      "_buildurl" = file.path(url, "8998732490")
    ), 
    string2path = list(
      "_linuxdevel" = "success", "_macbinary" = "success", 
      "_wasmbinary" = "none", "_winbinary" = "success", "_status" = "success", 
      "_buildurl" = file.path(url, "8998732437")
    ), 
    tidytensor = list(
      "_linuxdevel" = "failure", "_macbinary" = "success", 
      "_wasmbinary" = "success", "_winbinary" = "success", 
      "_status" = "failure",
      "_buildurl" = file.path(url, "8998732025")
    )
  )
  expect_equal(issues[order(names(issues))], expected[order(names(expected))])
})
