#' @title Report packages with version conflicts in another repository.
#' @export
#' @family issues
#' @description Report packages with higher versions in
#'   different repositories.
#'   A higher version in a different repository could cause that repository
#'   to override R-multiverse in `install.packages()`.
#' @return A data frame with one row for each problematic package
#'   and columns with details.
#' @inheritParams issues_advisories
#' @param repo Character string naming the repository to compare versions.
#' @examples
#' \dontrun{
#' issues_version_conflicts()
#' }
issues_version_conflicts <- function(meta = meta_packages(), repo = "cran") {
  # NA is always lower than any version number.
  conflict <- vapply(
    X = seq_len(nrow(meta)),
    FUN = function(index) {
      utils::compareVersion(
        a = meta$version[index],
        b = meta[[repo]][index]
      ) < 0L
    },
    FUN.VALUE = logical(1L)
  )
  meta[conflict, c("package", repo), drop = FALSE]
}
