#' @title Freeze dependencies
#' @export
#' @family staging
#' @description Freeze the targeted versions of base R and CRAN packages.
#' @details [freeze_dependencies()] runs during the month-long
#'   dependency freeze phase of Staging in which base R and CRAN packages
#'   are locked in the Staging universe until after the next Production
#'   snapshot. This establishes checks in the Staging universe
#'   using the exact set of dependencies that will be used in the
#'   candidate freeze (see [stage_candidates()]).
#'
#'   [freeze_dependencies()] copies the Community repository `packages.json`
#'   into the Staging repository to reset the Staging process.
#'   It also writes a `config.json` file with the date
#'   of the targeted CRAN snapshot.
#' @return `NULL` (invisibly)
#' @inheritParams stage_candidates
#' @param path_community Character string, local directory path
#'   to the clone of the Community universe GitHub repository.
#' @examples
#' \dontrun{
#' url_staging = "https://github.com/r-multiverse/staging"
#' url_community = "https://github.com/r-multiverse/community"
#' path_staging <- tempfile()
#' path_community <- tempfile()
#' gert::git_clone(url = url_staging, path = path_staging)
#' gert::git_clone(url = url_community, path = path_community)
#' freeze_dependencies(
#'   path_staging = path_staging,
#'   path_community = path_community
#' )
#' }
freeze_dependencies <- function(path_staging, path_community) {
  write_config_json(path_staging)
  file.copy(
    file.path(path_community, "packages.json"),
    file.path(path_staging, "packages.json"),
    overwrite = TRUE
  )
  invisible()
}

write_config_json <- function(path_staging) {
  file_config <- file.path(path_staging, "config.json")
  snapshot <- meta_snapshot()
  json_config <- list(cran_version = snapshot$dependency_freeze)
  jsonlite::write_json(
    json_config,
    file_config,
    pretty = TRUE,
    auto_unbox = TRUE
  )
}
