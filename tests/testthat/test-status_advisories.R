test_that("status_advisories() on its own", {
  example <- mock_meta_packages$package == "nanonext"
  commonmark <- mock_meta_packages[example,, drop = FALSE] # nolint
  commonmark$package <- "commonmark"
  commonmark$version <- "0.2"
  readxl <- mock_meta_packages[example,, drop = FALSE] # nolint
  readxl$package <- "readxl"
  readxl$version <- "1.4.1"
  meta <- rbind(
    mock_meta_packages,
    commonmark,
    readxl
  )
  out <- status_advisories(meta)
  expect_equal(
    out$commonmark$advisories,
    file.path(
      "https://github.com/RConsortium/r-advisory-database",
      "blob/main/vulns/commonmark",
      c("RSEC-2023-6.yaml", "RSEC-2023-7.yaml", "RSEC-2023-8.yaml")
    )
  )
})

test_that("status_advisories() in record_status()", {
  output <- tempfile()
  lines <- c(
    "[",
    " {",
    " \"package\": \"nanonext\",",
    " \"version_current\": \"1.0.0\",",
    " \"hash_current\": \"hash_1.0.0-modified\",",
    " \"version_highest\": \"1.0.0\",",
    " \"hash_highest\": \"hash_1.0.0\"",
    " }",
    "]"
  )
  versions <- tempfile()
  on.exit(unlink(c(output, versions), recursive = TRUE))
  writeLines(lines, versions)
  example <- mock_meta_packages$package == "nanonext"
  commonmark <- mock_meta_packages[example,, drop = FALSE] # nolint
  commonmark$package <- "commonmark"
  commonmark$version <- "0.2"
  readxl <- mock_meta_packages[example,, drop = FALSE] # nolint
  readxl$package <- "readxl"
  readxl$version <- "1.4.1"
  meta <- rbind(
    mock_meta_packages,
    commonmark,
    readxl
  )
  output <- tempfile()
  record_status(
    versions = versions,
    output = output,
    mock = list(packages = meta)
  )
  status <- jsonlite::read_json(status, simplifyVector = TRUE)
  expect_equal(
    status$commonmark$advisories,
    file.path(
      "https://github.com/RConsortium/r-advisory-database",
      "blob/main/vulns/commonmark",
      c("RSEC-2023-6.yaml", "RSEC-2023-7.yaml", "RSEC-2023-8.yaml")
    )
  )
})
