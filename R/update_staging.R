#' @title Update staging
#' @export
#' @family staging
#' @description Update the staging universe.
#' @details [update_staging()] controls how packages enter and leave
#'   the staging universe. It updates the staging `packages.json`
#'   manifest depending on the contents of the community
#'   universe and issues with package checks.
#' @return `NULL` (invisibly)
#' @inheritParams record_issues
#' @param path_staging Character string, directory path to the source
#'   files of the staging universe.
#' @param path_community Character string, directory path to the source
#'   files of the community universe.
#' @param repo_community Character string, URL of the community universe.
#' @examples
#' \dontrun{
#' url_staging = "https://github.com/r-multiverse/staging"
#' url_community = "https://github.com/r-multiverse/community"
#' path_staging <- tempfile()
#' path_community <- tempfile()
#' gert::git_clone(url = url_staging, path = path_staging)
#' gert::git_clone(url = url_community, path = path_community)
#' update_staging(
#'   path_staging = path_staging,
#'   path_community = path_community,
#'   repo_community = "https://community.r-multiverse.org"
#' )
#' }
update_staging <- function(
  path_staging,
  path_community,
  repo_community = "https://community.r-multiverse.org",
  mock = NULL
) {
  file_staging <- file.path(path_staging, "packages.json")
  file_community <- file.path(path_community, "packages.json")
  meta_community <- mock$community %||% meta_packages(repo_community)
  meta_community <- meta_community[!is.na(meta_community$remotesha),, drop = FALSE] # nolint
  json_community <- merge(
    x = jsonlite::read_json(file_community, simplifyVector = TRUE),
    y = meta_community[, c("package", "remotesha"), drop = FALSE],
    by = "package",
    all.x = TRUE, # Must be in Community packages.json.
    all.y = TRUE # Must be built on the Community universe.
  )
  json_community$branch <- json_community$remotesha
  json_community$remotesha <- NULL
  path_issues <- file.path(path_staging, "issues.json")
  json_issues <- list()
  if (file.exists(path_issues)) {
    json_issues <- jsonlite::read_json(path_issues, simplifyVector = TRUE)
  }
  freeze <- names(
    Filter(
      x = json_issues,
      f = function(issue) isTRUE(issue$success)
    )
  )
  file_freeze <- file.path(path_staging, "freeze.json")
  # If a Staging cycle is already underway,
  if (file.exists(file_freeze) && file.exists(file_staging)) {
    # then the no packages can enter or leave the Staging universe,
    json_staging <- as.data.frame(
      jsonlite::read_json(file_staging, simplifyVector = TRUE),
      stringsAsFactors = FALSE
    )
    # and frozen packages stay frozen.
    freeze <- base::union(
      freeze,
      jsonlite::read_json(file_freeze, simplifyVector = TRUE)
    )
  } else  {
    json_staging <- json_community # Otherwise, refresh Staging from Community.
  }
  candidates <- json_staging$package
  update <- setdiff(candidates, freeze)
  should_freeze <- json_staging$package %in% freeze
  json_freeze <- json_staging[should_freeze,, drop = FALSE] # nolint
  json_update <- json_community[json_community$package %in% update,, drop = FALSE] # nolint
  json_freeze$subdir <- json_freeze$subdir %||%
    rep(NA_character_, nrow(json_freeze))
  json_update$subdir <- json_update$subdir %||%
    rep(NA_character_, nrow(json_update))
  json_new <- rbind(json_freeze, json_update)
  json_new <- json_new[order(json_new$package), ]
  jsonlite::write_json(json_new, file_staging, pretty = TRUE)
  file_config <- file.path(path_staging, "config.json")
  json_config <- list(cran_version = meta_snapshot()$staging)
  jsonlite::write_json(
    json_config,
    file_config,
    pretty = TRUE,
    auto_unbox = TRUE
  )
  jsonlite::write_json(freeze, file_freeze, pretty = TRUE)
  invisible()
}
