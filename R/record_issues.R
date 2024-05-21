#' @title Record package issues.
#' @export
#' @description Record package check and version issues in individual JSON
#'   files.
#' @return `NULL` (invisibly).
#' @param output Character of length 1, file path to the folder to record
#'   new package issues. Each call to `record_issues()` overwrites the
#'   contents of the repo.
record_issues <- function(
  versions = "versions.json",
  output = "issues"
) {
  issues_versions <- check_versions(versions)
  issues <- issues_versions
  overwrite_issues(issues, output)
  invisible()
}

overwrite_issues <- function(issues, output) {
  unlink(output, recursive = TRUE)
  dir.create(output)
  for (index in seq_len(nrow(issues))) {
    row <- vctrs::vec_slice(x = issues, i = index)
    jsonlite::write_json(row, file.path(output, row$package))
  }
}
