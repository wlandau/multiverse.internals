#' @title Check package `DESCRIPTION` files.
#' @export
#' @family issues
#' @description Check the `DESCRIPTION` files of packages for specific
#'   issues.
#' @details This function scrapes the `src/contrib/PACKAGES.json` file
#'   of the universe to check the data in the `DESCRIPTION` files of packages
#'   for compliance. Right now, the only field checked is `Remotes:`.
#'   A packages with a `Remotes:` field in the `DESCRIPTION` file may
#'   depend on development versions of other packages and so are
#'   excluded from the production universe.
#' @inheritSection record_issues Package issues
#' @return A named list of information about packages which do not comply
#'   with `DESCRPTION` checks. Each name is a package name,
#'   and each element contains specific information about
#'   non-compliance.
#' @param meta A data frame of package metadata returned by [meta_packages()].
#' @examples
#'   meta <- meta_packages(repo = "https://wlandau.r-universe.dev")
#'   issues <- issues_descriptions(meta = meta)
#'   str(issues)
issues_descriptions <- function(meta) {
  meta <- issues_descriptions_remotes(meta)
  fields <- "remotes"
  meta <- meta[, c("package", fields)]
  issues_list(meta)
}

issues_descriptions_remotes <- function(meta) {
  meta$remotes <- meta$remotes %|||% replicate(nrow(meta), NULL)
  meta$remotes <- lapply(meta$remotes, function(x) x[nzchar(x)])
  meta[vapply(meta$remotes, length, integer(1L)) > 0L, ]
}
