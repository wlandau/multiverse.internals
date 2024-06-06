#' @title Report `DESCRIPTION` file issues.
#' @export
#' @family issues
#' @description Report issues with the `DESCRIPTION` files of packages.
#' @details [issues_descriptions()] scans downloaded metadata from the
#'   `PACKAGES.json` file of an R universe and reports issues with a
#'   package's description file, such as the presence of a
#'   `"Remotes"` field.
#' @inheritSection record_issues Package issues
#' @return A named list of information about packages which do not comply
#'   with `DESCRPTION` checks. Each name is a package name,
#'   and each element contains specific information about
#'   non-compliance.
#' @param meta A data frame with R-universe package check results
#'   returned by [meta_checks()].
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
  meta[["remotes"]] <- meta[["remotes"]] %||% replicate(nrow(meta), NULL)
  meta$remotes <- lapply(meta$remotes, function(x) x[nzchar(x)])
  meta[vapply(meta$remotes, length, integer(1L)) > 0L, ]
}
