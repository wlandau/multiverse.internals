test_that("invalid package name with vector", {
  expect_true(
    grepl(
      "Invalid package name",
      r.releases.internals::assert_package(name = letters, url = "xy"),
      fixed = TRUE
    )
  )
})

test_that("invalid package name with dot", {
  expect_true(
    grepl(
      "Invalid package name",
      r.releases.internals::assert_package(
        name = ".gh",
        url = "https://github.com/r-lib/gh"
      ),
      fixed = TRUE
    )
  )
})

test_that("custom JSON", {
  expect_true(
    grepl(
      "looks like custom JSON",
      r.releases.internals::assert_package(name = "xy", url = "{"),
      fixed = TRUE
    )
  )
})

test_that("invalid vector URL", {
  expect_true(
    grepl(
      "Invalid package URL",
      r.releases.internals::assert_package(
        name = "xy",
        url = letters
      ),
      fixed = TRUE
    )
  )
})

test_that("malformed URL", {
  expect_true(
    grepl(
      "Found malformed URL",
      r.releases.internals::assert_package(
        name = "gh",
        url = "github.com/r-lib/gh"
      ),
      fixed = TRUE
    )
  )
})

test_that("package name/repo disagreement", {
  expect_true(
    grepl(
      "appears to disagree with the repository name in the URL",
      r.releases.internals::assert_package(
        name = "gh2",
        url = "https://github.com/r-lib/gh"
      ),
      fixed = TRUE
    )
  )
})

test_that("https", {
  expect_true(
    grepl(
      "is not https",
      r.releases.internals::assert_package(
        name = "gh",
        url = "http://github.com/r-lib/gh"
      ),
      fixed = TRUE
    )
  )
})

test_that("GitHub/GitLab URL", {
  expect_true(
    grepl(
      "is not a GitHub or GitLab URL",
      r.releases.internals::assert_package(
        name = "gh",
        url = "https://github.gov/r-lib/gh"
      ),
      fixed = TRUE
    )
  )
})

test_that("owner URL", {
  expect_true(
    grepl(
      "appears to be an owner",
      r.releases.internals::assert_package(
        name = "gh",
        url = "https://github.com/gh"
      ),
      fixed = TRUE
    )
  )
})

test_that("CRAN mirror", {
  expect_true(
    grepl(
      "appears to use a CRAN mirror",
      r.releases.internals::assert_package(
        name = "gh",
        url = "https://github.com/cran/gh"
      ),
      fixed = TRUE
    )
  )
})

test_that("CRAN URL alignment", {
  expect_true(
    grepl(
      "does not appear in its DESCRIPTION file published on CRAN",
      r.releases.internals::assert_cran_url(
        name = "gh",
        url = "https://github.com/r-lib/gha"
      ),
      fixed = TRUE
    )
  )
})

test_that("HTTP error", {
  expect_true(
    grepl(
      "returned HTTP error",
      r.releases.internals::assert_package(
        name = "afantasticallylongandimpossiblepackage",
        url = "https://github.com/r-lib/afantasticallylongandimpossiblepackage"
      ),
      fixed = TRUE
    )
  )
})

test_that("release URL", {
  expect_true(
    grepl(
      "No full release found at URL",
      r.releases.internals::assert_package(
        name = "test.no.release",
        url = "https://github.com/wlandau/test.no.release"
      ),
      fixed = TRUE
    )
  )
})

test_that("good GitHub registration", {
  expect_null(
    r.releases.internals::assert_package(
      name = "gh",
      url = "https://github.com/r-lib/gh"
    )
  )
})

test_that("good GitLab registration", {
  suppressMessages(
    expect_null(
      r.releases.internals::assert_package(
        name = "test",
        url = "https://gitlab.com/wlandau/test"
      )
    )
  )
})

test_that("good registration with trailing slash", {
  expect_null(
    r.releases.internals::assert_cran_url(
      name = "curl",
      url = "https://github.com/jeroen/curl/"
    )
  )
})

test_that("good alignment with CRAN URL", {
  expect_null(
    r.releases.internals::assert_cran_url(
      name = "jsonlite",
      url = "https://github.com/jeroen/jsonlite"
    )
  )
})

test_that("trivially good alignment with CRAN URL", {
  expect_null(
    r.releases.internals::assert_cran_url(
      name = "packageNOTonCRAN",
      url = "https://github.com/jeroen/jsonlite"
    )
  )
})
