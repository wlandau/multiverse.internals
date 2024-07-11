update_production <- function(
  source_production = "https://github.com/r-multiverse/production",
  source_community = "https://github.com/r-multiverse/community",
  repo_production = "https://production.r-multiverse.org",
  repo_community = "https://community.r-multiverse.org",
  days_notice = 28L,
  mock = NULL
) {
  path_production <- tempfile()
  path_community <- tempfile()
  gert::git_clone(url = source_production, path = path_production)
  gert::git_clone(url = source_community, path = path_community)
  meta_packages_production <- meta_packages(repo = repo_production)
  meta_packages_community <- meta_packages(repo = repo_community)
  promote <- get_promote(path_community, meta_packages_community)
  demote <- get_demote(path_production, days_notice)
  unblock <- get_unblock(path_production, meta_packages_production)
  do_promote(
    packages = promote,
    path_production = path_production,
    path_community = path_community,
    meta = meta_packages_community
  )
  do_demote()
  do_unblock()
}

get_promote <- function(path, meta) {
  flagged <- list.files(file.path(path, "issues"))
  exclude <- Filter(
    x = flagged, 
    f = function(package) {
      json <- jsonlite::read_json(path = file.path(path, "issues", package))
      any(names(json) %in% c("descriptions", "versions"))
    }
  )
  setdiff(meta$package, exclude)
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

get_unblock <- function(path, meta) {
  file <- file.path(path, "demoting.json")
  if (!file.exists(file)) {
    return()
  }
  demoting <- as.character(jsonlite::read_json(file))
  demoted <- meta$package
  setdiff(demoting, demoted)
}

do_promote <- function(
  packages,
  path_production,
  path_community,
  meta
) {
  file_production <- file.path(path_production, "packages.json")
  file_community <- file.path(path_community, "packages.json")
  json_production <- name_json(jsonlite::read_json(file_production))
  json_community <- name_json(jsonlite::read_json(file_community))
  
  browser()
  stop('TEST HERE!!!')
  
  for (package in packages) {
    json_production[[package]] <- json_community[[package]]
    json_production[[package]][["branch"]] <- meta[package, "remotesha"]
  }
}
