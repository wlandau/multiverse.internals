#' @title Record package status.
#' @export
#' @keywords package check data management
#' @description Record R-multiverse package status in
#'   package-specific JSON files.
#' @section Package status:
#'   Functions like [issues_versions()] and [issues_r_cmd_check()]
#'   perform health checks for all packages in R-multiverse.
#'   For a complete list of checks, see
#'   the `issues_*()` functions listed at
#'   <https://r-multiverse.org/multiverse.internals/reference/index.html>.
#'   [record_versions()] updates the version number history
#'   of releases in R-multiverse, and [record_status()] gathers
#'   together all the status about R-multiverse packages.
#' @return `NULL` (invisibly).
#' @inheritParams issues_r_cmd_check
#' @inheritParams issues_dependencies
#' @inheritParams issues_versions
#' @inheritParams meta_packages
#' @param output Character of length 1, file path to the JSON file to record
#'   new package status. Each call to `record_status()` overwrites the
#'   contents of the file.
#' @param mock For testing purposes only, a named list of data frames
#'   for inputs to various intermediate functions.
#' @examples
#' \dontrun{
#' output <- tempfile()
#' versions <- tempfile()
#' record_versions(
#'   versions = versions,
#'   repo = repo
#' )
#' record_status(
#'   repo = repo,
#'   versions = versions,
#'   output = output
#' )
#' writeLines(readLines(output))
#' }
record_status <- function(
  repo = "https://community.r-multiverse.org",
  versions = "versions.json",
  output = "status.json",
  mock = NULL,
  verbose = FALSE
) {
  meta <- mock$packages %||% meta_packages(repo = repo)
  status <- Map(
    function(packages, published, version, remotesha) {
      list(
        packages = packages,
        success = TRUE,
        published = published,
        version = version,
        remote_hash = remotesha
      )
    },
    packages = meta$package,
    published = meta$published,
    version = meta$version,
    remotesha = meta$remotesha
  ) |>
    add_issues(issues_advisories(meta)) |>
    add_issues(issues_licenses(meta)) |>
    add_issues(issues_r_cmd_check(meta)) |>
    add_issues(issues_remotes(meta)) |>
    add_issues(issues_version_conflicts(meta, "cran")) |>
    add_issues(issues_versions(versions))
  issues <- names(Filter(\(x) !x$success, status))
  status <- add_issues(status, issues_dependencies(issues, meta, verbose))
  status <- status[order(names(status))]
  jsonlite::write_json(x = status, path = output, pretty = TRUE)
  invisible()
}

add_issues <- function(status, issues) {
  for (field in setdiff(colnames(issues), "package")) {
    for (index in seq_len(nrow(issues))) {
      package <- issues$package[index]
      status[[package]]$success <- FALSE
      status[[package]][[field]] <- issues[[field]][[index]]
    }
  }
  status
}
