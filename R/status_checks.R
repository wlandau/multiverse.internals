#' @title Report status from R-universe package check results.
#' @export
#' @family status
#' @description Check R-universe package check results.
#' @details [status_checks()] reads output from
#'   the R-universe check API
#'   to scan all R-multiverse packages for status that may have
#'   happened during building and testing.
#' @inheritSection record_status Package status
#' @return A named list of information about packages which do not comply
#'   with `DESCRPTION` checks. Each name is a package name,
#'   and each element contains specific information about
#'   non-compliance.
#' @param meta A data frame with R-universe package check results
#'   returned by [meta_checks()].
#' @examples
#'   meta <- meta_checks(repo = "https://wlandau.r-universe.dev")
#'   status <- status_checks(meta = meta)
#'   str(status)
status_checks <- function(meta = meta_checks()) {
  meta <- meta[lengths(meta$status) > 0L,, drop = FALSE] # nolint
  status_list(meta[, c("package", "url", "status")])
}
