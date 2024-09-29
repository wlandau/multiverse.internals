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
  issues_list(meta[, c("package", "advisories", "remotes")])
}

issues_descriptions_advisories <- function(meta) {
  advisories <- read_advisories(timeout = 60000L, retries = 3L)
  meta <- merge(
    x = meta,
    y = advisories,
    by = c("package", "version"),
    all.x = TRUE,
    all.y = FALSE
  )
  meta$issue <- meta$issue | !vapply(meta$advisories, anyNA, logical(1L))
  meta
}

issues_descriptions_remotes <- function(meta) {
  meta[["remotes"]] <- meta[["remotes"]] %||% replicate(nrow(meta), NULL)
  meta$remotes <- lapply(meta$remotes, function(x) x[nzchar(x)])
  meta$issue <- meta$issue | vapply(meta$remotes, length, integer(1L)) > 0L
  meta
}

read_advisories <- function(timeout, retries) {
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE, force = TRUE))
  zipfile <- file.path(path, "file.zip")
  for (i in seq_len(retries)) {
    res <- nanonext::ncurl(
      "https://github.com/RConsortium/r-advisory-database/zipball/main",
      convert = FALSE,
      follow = TRUE,
      timeout = timeout
    )
    res[["status"]] == 200L && break
    i == retries && stop(
      "Obtaining advisories from R Consortium database failed with status: ",
      status_code(res[["status"]]),
      call. = FALSE
    )
  }
  writeBin(res[["data"]], zipfile)
  unzip(zipfile, exdir = path, junkpaths = TRUE)
  advisories <- Sys.glob(file.path(path, "RSEC*.yaml"))
  out <- do.call(vctrs::vec_rbind, lapply(advisories, read_advisory))
  stats::aggregate(x = advisories ~ package + version, data = out, FUN = list)
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
    advisories = file.path(
      "https://github.com/RConsortium/r-advisory-database/blob/main/vulns",
      entry$package$name,
      basename(path)
    )
  )
}
