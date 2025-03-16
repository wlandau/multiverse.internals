#' @title Package version issues.
#' @export
#' @family status
#' @description Check package version number history for compliance.
#' @details This function checks the version number history of packages
#'   in R-multiverse and reports any packages with issues. The current
#'   released version of a given package must be unique, and it must be
#'   greater than all the versions of all the previous package releases.
#' @inheritSection record_status Package status
#' @return A data frame with one row for each problematic
#'   package and columns with
#'   the package names and version issues.
#' @inheritParams record_versions
#' @examples
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
  misaligned <- history[!aligned,, drop = FALSE] # nolint
  out <- data.frame(package = misaligned$package)
  out$versions <- Map(
    function(version_current, hash_current, version_highest, hash_highest) {
      list(
        version_current = version_current,
        hash_current = hash_current,
        version_highest = version_highest,
        hash_highest = hash_highest
      )
    },
    version_current = misaligned$version_current,
    hash_current = misaligned$hash_current,
    version_highest = misaligned$version_highest,
    hash_highest = misaligned$hash_highest
  ) |>
    unname()
  out
}
