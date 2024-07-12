#' @title Update production
#' @export
#' @description Update the production universe.
#' @details [update_production()] controls how packages enter and leave
#'   the production universe. It updates the production `packages.json`
#'   manifest depending on the contents of the community
#'   universe and issues with package checks. There are 3 phases:
#'   1. Demote packages: packages with any check issues in production
#'     are given `days_notice` days to fix the problems. If the problems
#'     are fixed on time, then the package stays in
#'     production, and the notice period resets (the next problem is given
#'     the full `days_notice` notice period from scratch). Otherwise,
#'     if there are still issues after `days_notice` days, then the package
#'     is removed from the `packages.json` manifest and added to a special
#'     `removing.json` manifest in production. `removing.json` ensures
#'     that the builds are actually removed from production before the
#'     package can be promoted again. That way, if the next automatic
#'     promotion fails production checks, no build will be available
#'     to install from <https://production.r-multiverse.org>.
#'   2. Clear removals: a demoted package stays in `removing.json` until
#'     the builds are removed from <https://production.r-multiverse.org>.
#'     After the builds are gone, the package is removed from `removing.json`
#'     so it can be promoted again. No build will become available again
#'     until the package passes all production checks.
#'   3. Promote packages: a package in the community universe is moved
#'     to production if:
#'       * It passes description checks from [issues_descriptions()].
#'       * It passes version checks from [issues_versions()].
#'       * Builds are available at <https://community.r-multiverse.org>.
#'       * The package is not in `removing.json`.
#'     To promote the package, an entry is created in the production
#'     `packages.json` with the remote SHA of the latest release.
#' @return `NULL` (invisibly)
#' @inheritParams record_issues
#' @param path_production Character string, directory path to the source
#'   files of the production universe.
#' @param path_community Character string, directory path to the source
#'   files of the community universe.
#' @param repo_production Character string, URL of the production universe.
#' @param repo_community Character string, URL of the community universe.
#' @param days_notice Integer scalar, number of days between the
#'   detection of a production issue and removal from the production universe.
#' @examples
#' url_production = "https://github.com/r-multiverse/production"
#' url_community = "https://github.com/r-multiverse/community"
#' path_production <- tempfile()
#' path_community <- tempfile()
#' gert::git_clone(url = url_production, path = path_production)
#' gert::git_clone(url = url_community, path = path_community)
#' update_production(
#'   repo_production = "https://production.r-multiverse.org",
#'   repo_community = "https://community.r-multiverse.org",
#'   path_production = tempfile(),
#'   path_community = tempfile(),
#'   days_notice = 28L
#' )
update_production <- function(
  path_production,
  path_community,
  repo_production = "https://production.r-multiverse.org",
  repo_community = "https://community.r-multiverse.org",
  days_notice = 28L,
  mock = NULL
) {
  meta_production <- mock$production %|||% meta_packages(repo_production)
  meta_community <- mock$community %|||% meta_packages(repo_community)
  demote_packages(path = path_production, days_notice = days_notice)
  clear_removed(path = path_production, meta = meta_production)
  promote_packages(
    path_production = path_production,
    path_community = path_community,
    meta = meta_community
  )
  invisible()
}

demote_packages <- function(path, days_notice) {
  packages <- get_demote(path_production, days_notice)
  removing <- base::union(get_removing(path), packages)
  jsonlite::write_json(removing, file.path(path, "removing.json"))
  file <- file.path(path, "packages.json")
  json <- name_json(jsonlite::read_json(file))
  json[packages] <- NULL
  jsonlite::write_json(unname(json), file)
}

clear_removed <- function(path, meta) {
  removing <- intersect(get_removing(path), meta$package)
  jsonlite::write_json(removing, file.path(path, "removing.json"))
}

promote_packages <- function(
  path_production,
  path_community,
  meta
) {
  packages <- get_promote(path_community, meta_community)
  file_production <- file.path(path_production, "packages.json")
  file_community <- file.path(path_community, "packages.json")
  json_production <- name_json(jsonlite::read_json(file_production))
  json_community <- name_json(jsonlite::read_json(file_community))
  for (package in packages) {
    json_production[[package]] <- json_community[[package]]
    json_production[[package]][["branch"]] <- meta[package, "remotesha"]
  }
  jsonlite::write_json(unname(json_production), file_production)
}

get_demote <- function(path, days_notice) {
  flagged <- list.files(file.path(path, "issues"))
  Filter(
    x = flagged, 
    f = function(package) {
      json <- jsonlite::read_json(path = file.path(path, "issues", package))
      delay <- as.integer(Sys.Date() - as.Date(as.character(json$date)))
      delay > days_notice
    }
  )
}

get_promote <- function(path_production, path_community, meta) {
  issues <- Filter(
    x = list.files(file.path(path_community, "issues")), 
    f = function(package) {
      json <- jsonlite::read_json(
        path = file.path(path_community, "issues", package)
      )
      any(names(json) %in% c("descriptions", "versions"))
    }
  )
  removing <- get_removing(path_production)
  json <- name_json(jsonlite::read_json(file.path(path, "packages.json")))
  setdiff(intersect(meta$package, names(json)), c(issues, removing))
}

get_removing <- function(path) {
  file <- file.path(path, "removing.json")
  if (!file.exists(file)) {
    return(character(0L))
  }
  jsonlite::read_json(file, simplifyVector = TRUE)
}
