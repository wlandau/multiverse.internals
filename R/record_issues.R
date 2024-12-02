#' @title Record package issues.
#' @export
#' @keywords package check data management
#' @description Record R-multiverse package issues in
#'   package-specific JSON files.
#' @section Package issues:
#'   Functions like [issues_versions()] and [issues_descriptions()]
#'   perform health checks for all packages in R-multiverse.
#'   For a complete list of checks, see
#'   the `issues_*()` functions listed at
#'   <https://r-multiverse.org/multiverse.internals/reference/index.html>.
#'   [record_versions()] updates the version number history
#'   of releases in R-multiverse, and [record_issues()] gathers
#'   together all the issues about R-multiverse packages.
#' @section Issue data:
#'   For each package with observed problems, [record_issues()] writes
#'   a JSON list entry in the output JSON file
#'   with one element for each type of failing check.
#'   Each check-specific element has an informative name
#'   (for example, `checks`, `descriptions`, or `versions`)
#'   and a list of diagnostic information. In addition, there is a `date`
#'   field to indicate when an issue was first detected. The `date`
#'   automatically resets the next time all the issues in the package
#'   are resolved.
#' @return `NULL` (invisibly).
#' @inheritParams issues_checks
#' @inheritParams issues_dependencies
#' @inheritParams issues_versions
#' @inheritParams meta_checks
#' @param output Character of length 1, file path to the JSON file to record
#'   new package issues. Each call to `record_issues()` overwrites the
#'   contents of the file.
#' @param mock For testing purposes only, a named list of data frames
#'   for inputs to various intermediate functions.
#' @examples
#'   repo <- "https://wlandau.r-universe.dev"
#'   output <- tempfile()
#'   versions <- tempfile()
#'   record_versions(
#'     versions = versions,
#'     repo = repo
#'   )
#'   record_issues(
#'     repo = repo,
#'     versions = versions,
#'     output = output
#'   )
#'   writeLines(readLines(output))
record_issues <- function(
  repo = "https://community.r-multiverse.org",
  versions = "versions.json",
  output = "issues.json",
  mock = NULL,
  verbose = FALSE
) {
  today <- mock$today %||% format(Sys.Date(), fmt = "yyyy-mm-dd")
  checks <- mock$checks %||% meta_checks(repo = repo)
  packages <- mock$packages %||% meta_packages(repo = repo)
  issues <- list() |>
    add_issues(issues_checks(meta = checks), "checks") |>
    add_issues(issues_descriptions(meta = packages), "descriptions") |>
    add_issues(issues_versions(versions = versions), "versions")
  issues <- issues |>
    add_issues(
      issues_dependencies(names(issues), packages, verbose = verbose),
      "dependencies"
    )
  overwrite_issues(
    issues = issues,
    output = output,
    today = today,
    packages = packages
  )
  invisible()
}

add_issues <- function(total, subset, category) {
  for (package in names(subset)) {
    total[[package]][[category]] <- subset[[package]]
  }
  total
}

overwrite_issues <- function(issues, output, today, packages) {
  previous <- list()
  if (file.exists(output)) {
    previous <- jsonlite::read_json(output, simplifyVector = TRUE)
  }
  for (package in names(issues)) {
    issues[[package]]$date <- previous[[package]]$date %||% today
    index <- packages$package == package
    issues[[package]]$version <- packages$version[index]
    issues[[package]]$remote_hash <- packages$remotesha[index]
  }
  jsonlite::write_json(x = issues, path = output, pretty = TRUE)
}
