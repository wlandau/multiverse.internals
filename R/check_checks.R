#' @title Check R-universe package check results.
#' @export
#' @family package checks for production
#' @description Check R-universe package check results.
#' @details [check_checks()] function scrapes the R-universe check API
#'   to scan all R-multiverse packages for issues that may have
#'   happened during building and testing.
#' @inheritSection record_issues Package checks for production
#' @return A named list of information about packages which do not comply
#'   with `DESCRPTION` checks. Each name is a package name,
#'   and each element contains specific information about
#'   non-compliance.
#' @inheritParams check_descriptions
#' @examples
#'   str(check_descriptions(repo = "https://multiverse.r-multiverse.org"))
check_checks <- function(
  repo = "https://multiverse.r-multiverse.org",
  mock = NULL
) {
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
  index <- mock %|||% get_package_api(repo = repo, fields = fields)
  for (field in fields) {
    index[[field]][is.na(index[[field]])] <- "src-failure"
  }
  success <- rep(TRUE, nrow(index))
  for (field in fields_check) {
    success <- success & (index[[field]] %in% c("success", "skipped"))
  }
  index <- index[!success,, drop = FALSE] # nolint
  check_list(index[, c("Package", fields)])
}
