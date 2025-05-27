test_that("update_topics()", {
  path <- tempfile()
  on.exit(unlink(path))
  dir.create(path)
  dir.create(file.path(path, "topics"))
  skip_if_offline()
  for (file in c("index.md", "topic.md")) {
    download.file(
      url = file.path(
        "https://raw.githubusercontent.com/r-multiverse",
        "topics/refs/heads/main",
        file
      ),
      destfile = file.path(path, file),
      quiet = TRUE
    )
  }
  writeLines("bayesian description", file.path(path, "topics", "bayesian"))
  writeLines("hpc description", file.path(path, "topics", "hpc"))
  writeLines("empty description", file.path(path, "topics", "empty"))
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
    sort(list.files(path, pattern = "\\.md$")),
    sort(
      c(
        "bayesian.md",
        "empty.md",
        "hpc.md",
        "index.md",
        "topic.md"
      )
    )
  )
  out <- readLines(file.path(path, "index.md"))
  expect_true(any(grepl("bayesian.html", out, fixed = TRUE)))
  expect_true(any(grepl("empty.html", out, fixed = TRUE)))
  expect_true(any(grepl("hpc.html", out, fixed = TRUE)))
  out <- readLines(file.path(path, "hpc.md"))
  expect_false(any(grepl("nope", out, fixed = TRUE)))
  expect_true(any(grepl("crew", out, fixed = TRUE)))
  expect_false(any(grepl("stantargets", out, fixed = TRUE)))
  expect_false(any(grepl("jagstargets", out, fixed = TRUE)))
  out <- readLines(file.path(path, "bayesian.md"))
  expect_false(any(grepl("nope", out, fixed = TRUE)))
  expect_false(any(grepl("crew", out, fixed = TRUE)))
  expect_true(any(grepl("stantargets", out, fixed = TRUE)))
  expect_true(any(grepl("jagstargets", out, fixed = TRUE)))
  out <- readLines(file.path(path, "empty.md"))
  expect_false(any(grepl("nope", out, fixed = TRUE)))
  expect_false(any(grepl("crew", out, fixed = TRUE)))
  expect_false(any(grepl("stantargets", out, fixed = TRUE)))
  expect_false(any(grepl("jagstargets", out, fixed = TRUE)))
})
