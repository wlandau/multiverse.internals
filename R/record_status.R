#' @title Record package status.
#' @export
#' @keywords package check data management
#' @description Record R-multiverse package status in
#'   package-specific JSON files.
#' @section Package status:
#'   Functions like [status_versions()] and [status_descriptions()]
#'   perform health checks for all packages in R-multiverse.
#'   For a complete list of checks, see
#'   the `status_*()` functions listed at
#'   <https://r-multiverse.org/multiverse.internals/reference/index.html>.
#'   [record_versions()] updates the version number history
#'   of releases in R-multiverse, and [record_status()] gathers
#'   together all the status about R-multiverse packages.
#' @section Status data:
#'   For each package with observed problems, [record_status()] writes
#'   a JSON list entry in the output JSON file
#'   with one element for each type of failing check.
#'   Each check-specific element has an informative name
#'   (for example, `checks`, `descriptions`, or `versions`)
#'   and a list of diagnostic information. In addition, there is a `date`
#'   field to indicate when an issue was first detected. The `date`
#'   automatically resets the next time all the status in the package
#'   are resolved.
#' @return `NULL` (invisibly).
#' @inheritParams status_checks
#' @inheritParams status_dependencies
#' @inheritParams status_versions
#' @inheritParams meta_packages
#' @param output Character of length 1, file path to the JSON file to record
#'   new package status. Each call to `record_status()` overwrites the
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
#'   record_status(
#'     repo = repo,
#'     versions = versions,
#'     output = output
#'   )
#'   writeLines(readLines(output))
record_status <- function(
  repo = "https://community.r-multiverse.org",
  versions = "versions.json",
  output = "status.json",
  mock = NULL,
  verbose = FALSE
) {
  packages <- mock$packages %||% meta_packages(repo = repo)
  status <- list() |>
    add_status(status_checks(meta = packages), "checks") |>
    add_status(status_descriptions(meta = packages), "descriptions") |>
    add_status(status_versions(versions = versions), "versions")
  status <- status |>
    add_status(
      status_dependencies(names(status), packages, verbose = verbose),
      "dependencies"
    )
  for (name in names(status)) {
    status[[name]]$success <- FALSE
  }
  for (healthy in setdiff(packages$package, names(status))) {
    status[[healthy]] <- list(success = TRUE)
  }
  status <- status[sort(names(status))]
  overwrite_status(
    status = status,
    output = output,
    packages = packages
  )
  invisible()
}

add_status <- function(total, subset, category) {
  for (package in names(subset)) {
    total[[package]][[category]] <- subset[[package]]
  }
  total
}

overwrite_status <- function(status, output, packages) {
  for (index in seq_len(nrow(packages))) {
    package <- packages$package[index]
    status[[package]]$published <- packages$published[index]
    status[[package]]$version <- packages$version[index]
    status[[package]]$remote_hash <- packages$remotesha[index]
  }
  jsonlite::write_json(x = status, path = output, pretty = TRUE)
}
