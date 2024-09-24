#' @title Report `DESCRIPTION` file issues.
#' @export
#' @family issues
#' @description Report issues with the `DESCRIPTION` files of packages.
#' @details [issues_descriptions()] scans downloaded metadata from the
#'   `PACKAGES.json` file of an R universe and scans for specific issues in a
#'   package's description file:
#'   1. The presence of a `"Remotes"` field.
#'   2. There is a security advisory at
#'     <https://github.com/RConsortium/r-advisory-database>
#'     for the given package version.
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
issues_descriptions <- function(meta = meta_packages()) {
  meta$issue <- FALSE
  meta <- issues_descriptions_advisories(meta)
  meta <- issues_descriptions_remotes(meta)
  meta <- meta[meta$issue,, drop = FALSE] # nolint
  issues_list(meta[, c("package", "advisory", "remotes")])
}

issues_descriptions_advisories <- function(meta) {
  advisories <- read_advisories()
  meta <- merge(
    x = meta,
    y = advisories,
    by = c("package", "version"),
    all.x = TRUE,
    all.y = FALSE
  )
  meta$issue <- meta$issue | !is.na(meta$advisory)
  meta
}

issues_descriptions_remotes <- function(meta) {
  meta[["remotes"]] <- meta[["remotes"]] %||% replicate(nrow(meta), NULL)
  meta$remotes <- lapply(meta$remotes, function(x) x[nzchar(x)])
  meta$issue <- meta$issue | vapply(meta$remotes, length, integer(1L)) > 0L
  meta
}

read_advisories <- function() {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE, force = TRUE))
  gert::git_clone(
    url = "https://github.com/RConsortium/r-advisory-database",
    path = path,
    verbose = FALSE
  )
  advisories <- list.files(
    file.path(path, "vulns"),
    recursive = TRUE,
    full.names = TRUE
  )
  out <- do.call(vctrs::vec_rbind, lapply(advisories, read_advisory))
  keep <- !duplicated(out[, c("package", "version")])
  out[keep,, drop = FALSE] # nolint
}

read_advisory <- function(path) {
  out <- lapply(
    yaml::read_yaml(file = path)$affected,
    advisory_entry,
    path = path
  )
  do.call(vctrs::vec_rbind, out)
}

advisory_entry <- function(entry, path) {
  data.frame(
    package = entry$package$name,
    version = entry$versions,
    advisory = file.path(
      "https://github.com/RConsortium/r-advisory-database/blob/main/vulns",
      entry$package$name,
      basename(path)
    )
  )
}
