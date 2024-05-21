#' @title Record package issues.
#' @export
#' @keywords package check data management
#' @description Record R-multiverse package issues in compact JSON files.
#' @section Package checks for production:
#'   Functions like [check_versions()] and [check_descriptions()]
#'   perform health checks for all packages in R-multiverse.
#'   Only packages that pass these checks go to the production repository at
#'   <https://production.r-multiverse.org>. For a complete list of checks, see
#'   the `check_*()` functions listed at
#'   <https://r-multiverse.org/multiverse.internals/reference.html>.
#'   [record_versions()] updates the version number history
#'   of releases in R-multiverse, and [record_issues()] gathers
#'   together all the issues about R-multiverse packages.
#' @return `NULL` (invisibly).
#' @inheritParams check_checks
#' @inheritParams check_versions
#' @param output Character of length 1, file path to the folder to record
#'   new package issues. Each call to `record_issues()` overwrites the
#'   contents of the repo.
#' @param mock For testing purposes only, a named list of data frames
#'   for the `mock` argument of each type of check.
#' @examples
#'   # R-multiverse uses https://multiverse.r-multiverse.org as the repo.
#'   repo <- "https://wlandau.r-universe.dev" # just for testing and examples
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
#'   files <- list.files(output)
#'   print(files)
#'   package <- head(files, n = 1)
#'   if (length(package)) {
#'     print(package)
#'   }
#'   if (length(package)) {
#'     print(readLines(file.path(output, package)))
#'   }
record_issues <- function(
  repo = "https://multiverse.r-multiverse.org",
  versions = "versions.json",
  output = "issues",
  mock = NULL
) {
  list() |>
    issues(check_checks(repo, mock$checks), "checks") |>
    issues(check_descriptions(repo, mock$descriptions), "descriptions") |>
    issues(check_versions(versions = versions), "versions") |>
    overwrite_issues(output = output)
  invisible()
}

issues <- function(total, subset, category) {
  for (package in names(subset)) {
    total[[package]][[category]] <- subset[[package]]
  }
  total
}

overwrite_issues <- function(issues, output) {
  unlink(output, recursive = TRUE)
  dir.create(output)
  for (package in names(issues)) {
    jsonlite::write_json(
      x = issues[[package]],
      path = file.path(output, package),
      pretty = TRUE
    )
  }
}
