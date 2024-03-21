test_that("ordinary URLs can be written", {
  packages <- tempfile()
  dir.create(packages)
  writeLines("https://github.com/r-lib/gh", file.path(packages, "gh"))
  writeLines(
    "https://github.com/jeroen/jsonlite",
    file.path(packages, "jsonlite")
  )
  universe <- file.path(tempfile(), "out")
  suppressMessages(
    aggregate_contributions(
      input = packages,
      output = universe
    )
  )
  json <- jsonlite::read_json(universe)
  exp <- list(
    list(
      package = "gh",
      url = "https://github.com/r-lib/gh",
      branch = "*release"
    ),
    list(
      package = "jsonlite",
      url = "https://github.com/jeroen/jsonlite",
      branch = "*release"
    )
  )
  expect_true(identical(json, exp))
  unlink(packages, recursive = TRUE)
  unlink(universe)
})

test_that("\"branch\": \"release\" in certain defined cases", {
  packages <- tempfile()
  dir.create(packages)
  writeLines("https://github.com/r-lib/gh", file.path(packages, "gh"))
  writeLines(
    "https://github.com/jeroen/jsonlite",
    file.path(packages, "jsonlite")
  )
  writeLines(
    "https://github.com/wlandau/crew",
    file.path(packages, "crew")
  )
  writeLines(
    "https://github.com/cran/quarto",
    file.path(packages, "quarto")
  )
  universe <- file.path(tempfile(), "out")
  suppressMessages(
    aggregate_contributions(
      input = packages,
      output = universe,
      owner_exceptions = c(
        "https://github.com/cran",
        "https://github.com/wlandau"
      )
    )
  )
  json <- jsonlite::read_json(universe)
  exp <- list(
    list(
      package = "crew",
      url = "https://github.com/wlandau/crew"
    ),
    list(
      package = "gh",
      url = "https://github.com/r-lib/gh",
      branch = "*release"
    ),
    list(
      package = "jsonlite",
      url = "https://github.com/jeroen/jsonlite",
      branch = "*release"
    ),
    list(
      package = "quarto",
      url = "https://github.com/cran/quarto"
    )
  )
  expect_true(identical(json, exp))
  unlink(packages, recursive = TRUE)
  unlink(universe)
})

test_that("one URL is malformed", {
  packages <- tempfile()
  dir.create(packages)
  writeLines("https://github.com/r-lib/gh", file.path(packages, "gh"))
  writeLines(
    c("https://github.com/jeroen/jsonlite", "bad"),
    file.path(packages, "jsonlite")
  )
  universe <- file.path(tempfile(), "out")
  out <- try(
    suppressMessages(
      aggregate_contributions(
        input = packages,
        output = universe
      )
    ),
    silent = TRUE
  )
  expect_true(inherits(out, "try-error"))
  unlink(packages, recursive = TRUE)
  unlink(universe)
})

test_that("acceptable custom JSON", {
  packages <- tempfile()
  dir.create(packages)
  writeLines("https://github.com/r-lib/gh", file.path(packages, "gh"))
  writeLines(
    c(
      "{",
      "  \"package\": \"paws.analytics\",",
      "  \"url\": \"https://github.com/paws-r/paws\",",
      "  \"subdir\": \"cran/paws.analytics\",",
      "  \"branch\": \"*release\"",
      "}"
    ),
    file.path(packages, "paws.analytics")
  )
  universe <- file.path(tempfile(), "out")
  suppressMessages(
    aggregate_contributions(
      input = packages,
      output = universe
    )
  )
  out <- jsonlite::read_json(path = universe)
  exp <- list(
    list(
      package = "gh",
      url = "https://github.com/r-lib/gh",
      branch = "*release"
    ),
    list(
      package = "paws.analytics",
      url = "https://github.com/paws-r/paws",
      branch = "*release",
      subdir = "cran/paws.analytics"
    )
  )
  expect_true(identical(out, exp))
  unlink(packages, recursive = TRUE)
  unlink(universe)
})

test_that("malformed URL in JSON", {
  packages <- tempfile()
  dir.create(packages)
  writeLines("https://github.com/r-lib/gh", file.path(packages, "gh"))
  writeLines(
    c(
      "{",
      "  \"package\": \"paws.analytics\",",
      "  \"url\": \"b a d u r l\",",
      "  \"subdir\": \"cran/paws.analytics\",",
      "  \"branch\": \"*release\"",
      "}"
    ),
    file.path(packages, "paws.analytics")
  )
  universe <- file.path(tempfile(), "out")
  out <- try(
    suppressMessages(
      aggregate_contributions(
        input = packages,
        output = universe
      )
    ),
    silent = TRUE
  )
  expect_true(
    grepl(
      pattern = "Found malformed URL",
      x = try_message(out)
    )
  )
  unlink(packages, recursive = TRUE)
  unlink(universe)
})

test_that("missing branch field", {
  packages <- tempfile()
  dir.create(packages)
  writeLines("https://github.com/r-lib/gh", file.path(packages, "gh"))
  writeLines(
    c(
      "{",
      "  \"package\": \"paws.analytics\",",
      "  \"url\": \"https://github.com/paws-r/paws\",",
      "  \"subdir\": \"cran/paws.analytics\"",
      "}"
    ),
    file.path(packages, "paws.analytics")
  )
  universe <- file.path(tempfile(), "out")
  out <- try(
    suppressMessages(
      aggregate_contributions(
        input = packages,
        output = universe
      )
    ),
    silent = TRUE
  )
  expect_true(inherits(out, "try-error"))
  expect_true(
    grepl(
      pattern = "JSON listing for package",
      x = try_message(out),
      fixed = TRUE
    )
  )
  expect_true(
    grepl(
      pattern = "must have fields",
      x = try_message(out),
      fixed = TRUE
    )
  )
  unlink(packages, recursive = TRUE)
  unlink(universe)
})

test_that("disagreeing package field", {
  packages <- tempfile()
  dir.create(packages)
  writeLines("https://github.com/r-lib/gh", file.path(packages, "gh"))
  writeLines(
    c(
      "{",
      "  \"package\": \"paws.analytics2\",",
      "  \"url\": \"https://github.com/paws-r/paws\",",
      "  \"subdir\": \"cran/paws.analytics\",",
      "  \"branch\": \"*release\"",
      "}"
    ),
    file.path(packages, "paws.analytics")
  )
  universe <- file.path(tempfile(), "out")
  out <- try(
    suppressMessages(
      aggregate_contributions(
        input = packages,
        output = universe
      )
    ),
    silent = TRUE
  )
  expect_true(inherits(out, "try-error"))
  expect_true(
    grepl(
      pattern = "The 'packages' field disagrees with the package name",
      x = try_message(out),
      fixed = TRUE
    )
  )
  unlink(packages, recursive = TRUE)
  unlink(universe)
})

test_that("bad branch field", {
  packages <- tempfile()
  dir.create(packages)
  writeLines("https://github.com/r-lib/gh", file.path(packages, "gh"))
  writeLines(
    c(
      "{",
      "  \"package\": \"paws.analytics\",",
      "  \"url\": \"https://github.com/paws-r/paws\",",
      "  \"subdir\": \"cran/paws.analytics\",",
      "  \"branch\": \"development\"",
      "}"
    ),
    file.path(packages, "paws.analytics")
  )
  universe <- file.path(tempfile(), "out")
  out <- try(
    suppressMessages(
      aggregate_contributions(
        input = packages,
        output = universe
      )
    ),
    silent = TRUE
  )
  expect_true(inherits(out, "try-error"))
  expect_true(
    grepl(
      pattern = "The 'branch' field of package",
      x = try_message(out),
      fixed = TRUE
    )
  )
  unlink(packages, recursive = TRUE)
  unlink(universe)
})
