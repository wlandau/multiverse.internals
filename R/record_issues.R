#' @title Record package issues.
#' @export
#' @description Record package check and version issues in individual JSON
#'   files.
#' @return `NULL` (invisibly).
#' @param manifest Character of length 1, file path to a JSON manifest
#'   tracking release versions of packages.
#' @param output Character of length 1, file path to the folder to record
#'   new package issues. Each call to `record_issues()` overwrites the
#'   contents of the repo.
record_issues <- function(
  manifest = "versions.json",
  output = "issues"
) {
  issues_version <- version_issues(manifest)
  issues <- issues_version # Will include check issues too later.
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

version_issues <- function(manifest) {
  manifest <- jsonlite::read_json(path = manifest, simplifyVector = TRUE)
  aligned <- (manifest$version_current == manifest$version_highest) &
    (manifest$hash_current == manifest$hash_highest)
  aligned[is.na(aligned)] <- TRUE
  out <- manifest[!aligned,, drop = FALSE] # nolint
  if (nrow(out)) {
    out$version_okay <- FALSE
  }
  out
}
