#' @title R-universe package `R CMD check` issues.
#' @export
#' @family status
#' @description Report issues from `R CMD check` on R-universe.
#' @details [status_r_cmd_check()] reads output from
#'   the R-universe `R CMD check` results API
#'   to scan all R-multiverse packages for status that may have
#'   happened during building and testing.
#' @inheritSection record_status Package status
#' @return A data frame with one row for each problematic
#'   package and columns for the package
#'   names and `R CMD check` issues.
#' @param meta A data frame with R-universe package `R CMD check` results
#'   returned by [meta_packages()].
#' @examples
#'   meta <- meta_packages(repo = "https://wlandau.r-universe.dev")
#'   status <- issues_r_cmd_check(meta = meta)
#'   str(status)
issues_r_cmd_check <- function(meta = meta_packages()) {
  meta <- meta[lengths(meta$issues_r_cmd_check) > 0L,, drop = FALSE] # nolint
  out <- data.frame(package = meta$package)
  out$r_cmd_check <- Map(
    function(issues, url) {
      list(issues = issues, url = url)
    },
    issues = meta$issues_r_cmd_check,
    url = meta$url_r_cmd_check
  ) |>
    unname()
  out
}
