#' @title Stage release candidates
#' @export
#' @family staging
#' @description Stage release candidates for the targeted Production snapshot.
#' @details [stage_candidates()] implements the candidate freeze
#'   during the month-long period prior to the Production snapshot.
#'   Packages that pass R-multiverse checks are frozen
#'   (not allowed to update further) and staged for Production.
#'   Packages with at least one failing not staged for Production,
#'   and maintainers can update them with new GitHub/GitLab releases.
#'
#'   [stage_candidates()] writes `packages.json` to control
#'   contents of the Staging universe.
#' @return `NULL` (invisibly)
#' @inheritParams record_issues
#' @param path_staging Character string, directory path to the source
#'   files of the Staging universe.
#' @param repo_staging Character string, URL of the Staging universe.
#' @examples
#' \dontrun{
#' url_staging <- "https://github.com/r-multiverse/staging"
#' path_staging <- tempfile()
#' gert::git_clone(url = url_staging, path = path_staging)
#' stage_candidates(path_staging = path_staging)
#' }
stage_candidates <- function(
  path_staging,
  repo_staging = "https://staging.r-multiverse.org",
  mock = NULL
) {
  write_packages_json(path_staging, repo_staging, mock)
  write_snapshot_json(path_staging)
  write_config_json(path_staging)
  invisible()
}

write_packages_json <- function(path_staging, repo_staging, mock) {
  file_staging <- file.path(path_staging, "packages.json")
  meta_staging <- mock$staging %||% meta_packages(repo_staging)
  json_staging <- merge(
    x = jsonlite::read_json(file_staging, simplifyVector = TRUE),
    y = meta_staging[, c("package", "remotesha"), drop = FALSE],
    by = "package",
    all.x = TRUE,
    all.y = FALSE
  )
  json_staging$remotesha[is.na(json_staging$remotesha)] <- "*release"
  path_issues <- file.path(path_staging, "issues.json")
  json_issues <- jsonlite::read_json(path_issues, simplifyVector = TRUE)
  new_staged <- names(Filter(json_issues, f = \(issue) isTRUE(issue$success)))
  old_staged <- json_staging$package[json_staging$branch != "*release"]
  staged <- sort(base::union(new_staged, old_staged))
  is_staged <- json_staging$package %in% staged
  json_staging$branch[is_staged] <- json_staging$remotesha[is_staged]
  json_staging$branch[!is_staged] <- "*release"
  json_staging$remotesha <- NULL
  json_staging <- json_staging[order(json_staging$package),, drop = FALSE] # nolint
  jsonlite::write_json(json_staging, file_staging, pretty = TRUE)
}

write_snapshot_json <- function(path_staging) {
  jsonlite::write_json(
    meta_snapshot(),
    file.path(path_staging, "snapshot.json"),
    pretty = TRUE
  )
}
