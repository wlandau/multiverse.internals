test_that("assert_package_description() no description", {
  skip_if_offline()
  out <- assert_package_description(
    name = "multiverse.internals",
    url = "https://github.com/r-multiverse/contributions"
  )
  expect_true(grepl("Could not read a DESCRIPTION file", out))
})

test_that("assert_package_description() GitHub", {
  skip_if_offline()
  expect_null(
    assert_package_description(
      name = "multiverse.internals",
      url = "https://github.com/r-multiverse/multiverse.internals"
    )
  )
})

test_that("assert_package_description() GitLab", {
  skip_if_offline()
  expect_null(
    assert_package_description(
      name = "test",
      url = "https://gitlab.com/wlandau/test"
    )
  )
})

test_that("assert_local_description() on bad DESCRIPTION file", {
  out <- assert_local_description("name", list())
  expect_true(grepl("DESCRPTION file could not be parsed", out))
})

test_that("assert_local_description() good result", {
  path <- system.file(
    "DESCRIPTION",
    package = "multiverse.internals",
    mustWork = TRUE
  )
  text <- paste(readLines(path), collapse = "\n")
  expect_null(
    assert_local_description(
      name = "multiverse.internals",
      text = text
    )
  )
})

test_that("assert_local_description() different name", {
  path <- system.file(
    "DESCRIPTION",
    package = "multiverse.internals",
    mustWork = TRUE
  )
  text <- paste(readLines(path), collapse = "\n")
  out <- assert_local_description(
    name = "different",
    text = text
  )
  expect_true(grepl("R-multiverse listing name", out))
})

test_that("assert_parsed_description() no author", {
  path <- system.file(
    "DESCRIPTION",
    package = "multiverse.internals",
    mustWork = TRUE
  )
  text <- paste(readLines(path), collapse = "\n")
  description <- parse_description(text)
  description[["Authors@R"]] <- NULL
  out <- assert_parsed_description(
    name = "multiverse.internals",
    description = description
  )
  expect_true(grepl("does not list an author or maintainer", out))
})

test_that("assert_parsed_description() no license", {
  path <- system.file(
    "DESCRIPTION",
    package = "multiverse.internals",
    mustWork = TRUE
  )
  text <- paste(readLines(path), collapse = "\n")
  description <- parse_description(text)
  description[["License"]] <- NULL
  out <- assert_parsed_description(
    name = "multiverse.internals",
    description = description
  )
  expect_true(grepl("has no license", out))
})

test_that("assert_parsed_description() uncommon license", {
  path <- system.file(
    "DESCRIPTION",
    package = "multiverse.internals",
    mustWork = TRUE
  )
  text <- paste(readLines(path), collapse = "\n")
  description <- parse_description(text)
  description[["License"]] <- "uncommon"
  out <- assert_parsed_description(
    name = "multiverse.internals",
    description = description
  )
  expect_true(grepl("Detected license", out))
})
