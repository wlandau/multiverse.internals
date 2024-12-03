test_that("update_topics()", {
  path <- tempfile()
  on.exit(unlink(path))
  dir.create(path, recursive = TRUE)
  writeLines("bayesian description", file.path(path, "bayesian"))
  writeLines("hpc description", file.path(path, "hpc"))
  writeLines("empty description", file.path(path, "empty"))
  meta <- data.frame(
    package = c("nope", "crew", "stantargets", "jagstargets"),
    title = c("x", "crew-title", "stantargets-title", "jagstargets-title"),
    url = c(
      "https://asdf",
      "https://r-multiverse.org/topics/hpc.html, https://crew",
      "https://url, https://r-multiverse.org/topics/bayesian.html",
      "https://url,\nhttps://r-multiverse.org/topics/bayesian.html"
    )
  )
  update_topics(
    path = path,
    mock = list(meta = meta)
  )
  expect_equal(
    sort(list.files(path)),
    sort(
      c(
        "bayesian",
        "bayesian.html",
        "empty",
        "empty.html",
        "hpc",
        "hpc.html",
        "index.html"
      )
    )
  )
  out <- readLines(file.path(path, "index.html"))
  expect_true(any(grepl("bayesian.html", out, fixed = TRUE)))
  expect_true(any(grepl("hpc.html", out, fixed = TRUE)))
  out <- readLines(file.path(path, "hpc.html"))
  expect_false(any(grepl("nope", out, fixed = TRUE)))
  expect_true(any(grepl("crew", out, fixed = TRUE)))
  expect_false(any(grepl("stantargets", out, fixed = TRUE)))
  expect_false(any(grepl("jagstargets", out, fixed = TRUE)))
  out <- readLines(file.path(path, "bayesian.html"))
  expect_false(any(grepl("nope", out, fixed = TRUE)))
  expect_false(any(grepl("crew", out, fixed = TRUE)))
  expect_true(any(grepl("stantargets", out, fixed = TRUE)))
  expect_true(any(grepl("jagstargets", out, fixed = TRUE)))
  out <- readLines(file.path(path, "empty.html"))
  expect_false(any(grepl("nope", out, fixed = TRUE)))
  expect_false(any(grepl("crew", out, fixed = TRUE)))
  expect_false(any(grepl("stantargets", out, fixed = TRUE)))
  expect_false(any(grepl("jagstargets", out, fixed = TRUE)))
})
