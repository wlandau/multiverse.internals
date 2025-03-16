#' @title Report status from R-universe package `R CMD check` results.
#' @export
#' @family status
#' @description Check R-universe package `R CMD check` results.
#' @details [status_r_cmd_check()] reads output from
#'   the R-universe `R CMD check` results API
#'   to scan all R-multiverse packages for status that may have
#'   happened during building and testing.
#' @inheritSection record_status Package status
#' @return A named list of information about packages which do not comply
#'   with `DESCRPTION` checks. Each name is a package name,
#'   and each element contains specific information about
#'   non-compliance.
#' @param meta A data frame with R-universe package `R CMD check` results
#'   returned by [meta_packages()].
#' @examples
#'   meta <- meta_packages(repo = "https://wlandau.r-universe.dev")
#'   status <- status_r_cmd_check(meta = meta)
#'   str(status)
status_r_cmd_check <- function(meta = meta_packages()) {
  meta <- meta[lengths(meta$issues_r_cmd_check) > 0L,, drop = FALSE] # nolint
  status_list(meta[, c("package", "url_r_cmd_check", "issues_r_cmd_check")])
}
