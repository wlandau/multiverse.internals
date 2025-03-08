#' @title Propose snapshot
#' @export
#' @family staging
#' @description Propose a Production snapshot of Staging.
#' @details [propose_snapshot()] proposes a snapshot of Staging
#'   to migrate to Production. The recommended snapshot is the list of
#'   packages for which (1) the build and check results of the current
#'   release are in Staging, and (2) there are no issues.
#'   Writes a `snapshot.url` file containing an R-universe snapshot API URL
#'   to download those packages. This files is written to the
#'   directory given by the `path_staging` argument.
#' @return `NULL` (invisibly). Called for its side effects.
#'   [propose_snapshot()] writes a
#'   `snapshot.url` file containing an R-universe snapshot API URL
#'   to download packages ready for Production.
#'   This file is written to the
#'   directory given by the `path_staging` argument.
#' @inheritParams update_staging
#' @param types Character vector, what to pass to the `types` field in the
#'   snapshot API URL. Controls the types of binaries and documentation
#'   included in the snapshot.
#' @examples
#' \dontrun{
#' url_staging = "https://github.com/r-multiverse/staging"
#' path_staging <- tempfile()
#' gert::git_clone(url = url_staging, path = path_staging)
#' propose_snapshot(path_staging = path_staging)
#' }
propose_snapshot <- function(
  path_staging,
  types = c("src", "win", "mac")
) {
  path_freeze <- file.path(path_staging, "freeze.json")
  freeze <- character(0L)
  if (file.exists(path_freeze)) {
    freeze <- jsonlite::read_json(path_freeze, simplifyVector = TRUE)
  }
  file_staging <- file.path(path_staging, "packages.json")
  json_staging <- jsonlite::read_json(file_staging, simplifyVector = TRUE)
  exclude <- setdiff(json_staging$package, freeze)
  r_version <- r_version_staging()
  binaries <- paste0("&binaries=", r_version$short)
  url <- paste0(
    "https://staging.r-multiverse.org/api/snapshot/tar",
    "?types=",
    paste(types, collapse = ","),
    binaries,
    "&skip_packages=",
    paste(exclude, collapse = ",")
  )
  writeLines(url, file.path(path_staging, "snapshot.url"))
  meta <- data.frame(
    r_version = r_version$short,
    date_staging = date_staging(),
    date_snapshot = date_snapshot(),
    cran = file.path(
      "https://packagemanager.posit.co/cran",
      date_staging()
    ),
    r_multiverse = file.path(
      "https://production.r-multiverse.org",
      date_snapshot()
    )
  )
  jsonlite::write_json(
    meta,
    file.path(path_staging, "meta.json"),
    pretty = TRUE
  )
  invisible()
}
