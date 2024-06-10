#' @title Report issues from R-universe package check results.
#' @export
#' @family issues
#' @description Check R-universe package check results.
#' @details [issues_checks()] reads output from
#'   the R-universe check API
#'   to scan all R-multiverse packages for issues that may have
#'   happened during building and testing.
#' @inheritSection record_issues Package issues
#' @return A named list of information about packages which do not comply
#'   with `DESCRPTION` checks. Each name is a package name,
#'   and each element contains specific information about
#'   non-compliance.
#' @param meta A data frame with R-universe package check results
#'   returned by [meta_checks()].
#' @examples
#'   meta <- meta_checks(repo = "https://wlandau.r-universe.dev")
#'   issues <- issues_checks(meta = meta)
#'   str(issues)
issues_checks <- function(meta = multiverse.internals::meta_checks()) {
  fields_check <- c(
    "_linuxdevel",
    "_macbinary",
    "_wasmbinary",
    "_winbinary",
    "_status"
  )
  fields_info <- c(
    "_buildurl"
  )
  fields <- c(fields_check, fields_info)
  for (field in fields) {
    meta[[field]][is.na(meta[[field]])] <- "src-failure"
  }
  success <- rep(TRUE, nrow(meta))
  for (field in fields_check) {
    success <- success & (meta[[field]] %in% c("success", "skipped"))
  }
  meta <- meta[!success,, drop = FALSE] # nolint
  issues_list(meta[, c("package", fields)])
}
