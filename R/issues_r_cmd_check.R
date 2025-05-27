#' @title R-universe package `R CMD check` issues.
#' @export
#' @family issues
#' @description Report issues from `R CMD check` on R-universe.
#' @details [issues_r_cmd_check()] reads output from
#'   the R-universe `R CMD check` results API
#'   to scan all R-multiverse packages for status that may have
#'   happened during building and testing.
#' @inheritSection record_status Package status
#' @return A data frame with one row for each problematic
#'   package and columns for the package
#'   names and `R CMD check` issues.
#' @inheritParams issues_advisories
#' @examples
#' \dontrun{
#' issues_r_cmd_check()
#' }
issues_r_cmd_check <- function(meta = meta_packages()) {
  is_problem <- lengths(meta$issues_r_cmd_check) > 0L
  out <- meta[is_problem, c("package", "issues_r_cmd_check"), drop = FALSE]
  colnames(out) <- c("package", "r_cmd_check")
  out
}
