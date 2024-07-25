#' @title Update staging
#' @export
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
  meta_community <- mock$community %||% meta_packages(repo_community)
  packages <- promotions(path_community, meta_community)
  file_staging <- file.path(path_staging, "packages.json")
  file_community <- file.path(path_community, "packages.json")
  json_staging <- jsonlite::read_json(file_staging, simplifyVector = TRUE)
  json_community <- jsonlite::read_json(file_community, simplifyVector = TRUE)
  index_promote <- json_community$package %in% packages
  promote <- json_community[index_promote,, drop = FALSE] # nolint
  meta_community <- meta_community[, c("package", "remotesha")]
  promote <- merge(promote, meta_community, all.x = TRUE, all.y = FALSE)
  promote$branch <- promote$remotesha
  promote$remotesha <- NULL
  replace <- !(json_staging$package %in% packages)
  json_staging <- json_staging[replace,, drop = FALSE] # nolint
  json_staging <- rbind(json_staging, promote)
  json_staging <- json_staging[order(json_staging$package),, drop = FALSE ] # nolint
  jsonlite::write_json(json_staging, file_staging, pretty = TRUE)
  invisible()
}

promotions <- function(path_community, meta_community) {
  promotion_checks <- c(
    "descriptions",
    "versions"
  )
  issues <- Filter(
    x = list.files(file.path(path_community, "issues")),
    f = function(package) {
      json <- jsonlite::read_json(
        path = file.path(path_community, "issues", package)
      )
      any(names(json) %in% promotion_checks)
    }
  )
  file_community <- file.path(path_community, "packages.json")
  json <- jsonlite::read_json(file_community, simplifyVector = TRUE)
  candidates <- intersect(meta_community$package, json$package)
  setdiff(candidates, issues)
}
