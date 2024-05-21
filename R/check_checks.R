#' @title Check R-universe package check results.
#' @export
#' @family checks
#' @description Check R-universe package check results.
#' @details This function scrapes the `src/contrib/PACKAGES.json` file
#'   of the universe to check the data in the `DESCRIPTION` files of packages
#'   for compliance. Right now, the only field checked is `Remotes:`.
#'   A packages with a `Remotes:` field in the `DESCRIPTION` file may
#'   depend on development versions of other packages and so are
#'   excluded from the production universe.
#' @inheritSection check_versions Checks
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
