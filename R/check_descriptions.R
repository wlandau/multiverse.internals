#' @title Check package `DESCRIPTION` files.
#' @export
#' @family package checks for production
#' @description Check the `DESCRIPTION` files of packages for specific
#'   issues.
#' @details This function scrapes the `src/contrib/PACKAGES.json` file
#'   of the universe to check the data in the `DESCRIPTION` files of packages
#'   for compliance. Right now, the only field checked is `Remotes:`.
#'   A packages with a `Remotes:` field in the `DESCRIPTION` file may
#'   depend on development versions of other packages and so are
#'   excluded from the production universe.
#' @inheritSection record_issues Package checks for production
#' @return A named list of information about packages which do not comply
#'   with `DESCRPTION` checks. Each name is a package name,
#'   and each element contains specific information about
#'   non-compliance.
#' @param repo Character of length 1, URL of the repository
#'   of R package candidates for production.
#' @param mock For testing purposes only,
#'   an optional pre-computed data frame with details about packages
#'   for this type of check.
#' @examples
#'   str(check_descriptions(repo = "https://multiverse.r-multiverse.org"))
check_descriptions <- function(
  repo = "https://multiverse.r-multiverse.org",
  mock = NULL
) {
  fields <- "Remotes"
  index <- mock %|||% get_package_index(repo = repo, fields = fields)
  index <- check_descriptions_remotes(index)
  index <- index[, c("Package", fields)]
  check_list(index)
}

check_descriptions_remotes <- function(index) {
  index$Remotes <- index$Remotes %|||% replicate(nrow(index), NULL)
  index$Remotes <- lapply(index$Remotes, function(x) x[nzchar(x)])
  index[vapply(index$Remotes, length, integer(1L)) > 0L, ]
}
