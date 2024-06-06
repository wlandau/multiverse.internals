#' @title Check package versions.
#' @export
#' @family issues
#' @description Check package version number history for compliance.
#' @details This function checks the version number history of packages
#'   in R-multiverse and reports any packages with issues. The current
#'   released version of a given package must be unique, and it must be
#'   greater than all the versions of all the previous package releases.
#' @inheritSection record_issues Package issues
#' @return A named list of information about packages which do not comply
#'   with version number history checks. Each name is a package name,
#'   and each element contains specific information about version
#'   non-compliance: the current version number, the current version hash,
#'   and the analogous versions and hashes of the highest-versioned
#'   release recorded.
#' @inheritParams record_versions
#' @examples
#'   # See https://github.com/r-multiverse/checks/blob/main/versions.json
#'   # for the official versions JSON for R-multiverse.
#'   lines <- c(
#'     "[",
#'     " {",
#'     " \"package\": \"package_unmodified\",",
#'     " \"version_current\": \"1.0.0\",",
#'     " \"hash_current\": \"hash_1.0.0\",",
#'     " \"version_highest\": \"1.0.0\",",
#'     " \"hash_highest\": \"hash_1.0.0\"",
#'     " },",
#'     " {",
#'     " \"package\": \"version_decremented\",",
#'     " \"version_current\": \"0.0.1\",",
#'     " \"hash_current\": \"hash_0.0.1\",",
#'     " \"version_highest\": \"1.0.0\",",
#'     " \"hash_highest\": \"hash_1.0.0\"",
#'     " },",
#'     " {",
#'     " \"package\": \"version_incremented\",",
#'     " \"version_current\": \"2.0.0\",",
#'     " \"hash_current\": \"hash_2.0.0\",",
#'     " \"version_highest\": \"2.0.0\",",
#'     " \"hash_highest\": \"hash_2.0.0\"",
#'     " },",
#'     " {",
#'     " \"package\": \"version_unmodified\",",
#'     " \"version_current\": \"1.0.0\",",
#'     " \"hash_current\": \"hash_1.0.0-modified\",",
#'     " \"version_highest\": \"1.0.0\",",
#'     " \"hash_highest\": \"hash_1.0.0\"",
#'     " }",
#'     "]"
#'   )
#'   versions <- tempfile()
#'   writeLines(lines, versions)
#'   out <- issues_versions(versions)
#'   str(out)
issues_versions <- function(versions) {
  history <- jsonlite::read_json(path = versions, simplifyVector = TRUE)
  aligned <- (history$version_current == history$version_highest) &
    (history$hash_current == history$hash_highest)
  aligned[is.na(aligned)] <- TRUE
  out <- history[!aligned,, drop = FALSE] # nolint
  issues_list(out)
}
