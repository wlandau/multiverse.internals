#' @title Update the package status repository
#' @export
#' @family status
#' @description Update the repository which reports the status on individual
#'   packages.
#' @inheritParams stage_candidates
#' @param path_status Character string, directory path to the source files
#'   of the package status repository.
#' @param repo_staging Character string, URL of the staging universe.
#' @examples
#' \dontrun{
#' url_staging = "https://github.com/r-multiverse/staging"
#' url_community = "https://github.com/r-multiverse/community"
#' path_status <- tempfile()
#' path_staging <- tempfile()
#' path_community <- tempfile()
#' gert::git_clone(url = url_staging, path = path_staging)
#' gert::git_clone(url = url_community, path = path_community)
#' update_status(
#'   path_status = path_status,
#'   path_staging = path_staging,
#'   path_community = path_community,
#'   repo_staging = "https://staging.r-multiverse.org",
#'   repo_community = "https://community.r-multiverse.org"
#' )
#' writeLines(
#'   readLines(
#'     file.path(path_status, "community", "multiverse.internals.html")
#'   )
#' )
#' writeLines(
#'   readLines(
#'     file.path(path_status, "community", "multiverse.internals.xml")
#'   )
#' )
#' }
update_status <- function(
  path_status,
  path_staging,
  path_community,
  repo_staging = "https://staging.r-multiverse.org",
  repo_community = "https://community.r-multiverse.org",
  mock = NULL
) {
  meta_staging <- mock$staging %||% meta_packages(repo_staging)
  meta_community <- mock$community %||% meta_packages(repo_community)
  update_status_directory(
    output = path_status,
    input = path_staging,
    meta = meta_staging,
    directory = "staging"
  )
  update_status_directory(
    output = path_status,
    input = path_community,
    meta = meta_community,
    directory = "community"
  )
  update_status_production(
    output = path_status,
    input = path_staging
  )
}

update_status_production <- function(output, input) {
  path_issues <- file.path(input, "issues.json")
  path_staged <- file.path(input, "staged.json")
  path_snapshot <- file.path(input, "snapshot.json")
  if (!all(file.exists(c(path_issues, path_staged, path_snapshot)))) {
    return()
  }
  issues <- jsonlite::read_json(path_issues, simplifyVector = TRUE)
  staged <- jsonlite::read_json(path_staged, simplifyVector = TRUE)
  snapshot <- jsonlite::read_json(path_snapshot, simplifyVector = TRUE)
  issues <- as.data.frame(do.call(rbind, issues[staged]))
  url <- sprintf(
    "<a href=\"https://r-multiverse.org/status/staging/%s\">%s</a>",
    staged,
    staged
  )
  lines_packages <- paste(
    "<li>",
    url,
    issues$version,
    sprintf("(%s)", issues$date),
    "</li>"
  )
  lines_packages <- paste(lines_packages, collapse = "\n")
  file_template <- system.file(
    file.path("status", "production.html"),
    package = "multiverse.internals",
    mustWork = TRUE
  )
  lines_page <- gsub(
    pattern = "SNAPSHOT",
    replacement = snapshot$snapshot,
    x = readLines(file_template)
  )
  lines_page <- gsub(
    pattern = "PACKAGES",
    replacement = lines_packages,
    x = lines_page
  )
  writeLines(lines_page, file.path(output, "production.html"))
}

update_status_directory <- function(output, input, meta, directory) {
  path_directory <- file.path(output, directory)
  unlink(path_directory, recursive = TRUE, force = TRUE)
  dir.create(path_directory, recursive = TRUE)
  path_issues <- file.path(input, "issues.json")
  issues <- list()
  if (file.exists(path_issues)) {
    issues <- jsonlite::read_json(path_issues, simplifyVector = TRUE)
  }
  for (index in seq_len(nrow(meta))) {
    package <- meta$package[index]
    guid <- meta$remotesha[index]
    success <- issues[[package]]$success
    if (isTRUE(success)) {
      suffix <- "success"
    } else if (isFALSE(success)) {
      suffix <- "issues found"
    } else {
      suffix <- "status unknown"
    }
    title <- paste0(package, ": ", suffix)
    status <- interpret_status(package, issues)
    update_status_html(package, title, status, path_directory)
    update_status_xml(package, title, path_directory, guid)
  }
  failures <- names(Filter(issues, f = function(issue) isFALSE(issue$success)))
  update_issue_summary(output, directory, failures)
}

update_issue_summary <- function(output, directory, packages) {
  package_list <- ""
  if (length(packages)) {
    package_list <- sprintf(
      "<li><a href=\"https://r-multiverse.org/status/%s/%s.html\">%s</a>",
      directory,
      packages,
      packages
    )
    package_list <- paste(package_list, collapse = "\n")
  }
  template <- system.file(
    file.path("status", "repository.html"),
    package = "multiverse.internals",
    mustWork = TRUE
  )
  lines <- readLines(template)
  lines <- gsub(
    pattern = "DIRECTORY",
    replacement = tools::toTitleCase(directory),
    x = lines
  )
  lines <- gsub(pattern = "PACKAGES", replacement = package_list, x = lines)
  writeLines(lines, file.path(output, paste0(directory, ".html")))
}

update_status_html <- function(package, title, status, path_directory) {
  path_template <- system.file(
    file.path("status", "status.html"),
    package = "multiverse.internals",
    mustWork = TRUE
  )
  directory <- basename(path_directory)
  rss <- file.path(
    "https://r-multiverse.org/status",
    directory,
    paste0(package, ".xml")
  )
  text <- readLines(path_template)
  text <- gsub(pattern = "TITLE", replacement = title, x = text)
  text <- gsub(pattern = "PACKAGE", replacement = package, x = text)
  text <- gsub(pattern = "RSS", replacement = rss, x = text)
  text <- gsub(pattern = "STATUS", replacement = status, x = text)
  path <- file.path(path_directory, paste0(package, ".html"))
  writeLines(text, path)
}

update_status_xml <- function(package, title, path_directory, guid) {
  path_template <- system.file(
    file.path("status", "status.xml"),
    package = "multiverse.internals",
    mustWork = TRUE
  )
  directory <- basename(path_directory)
  text <- readLines(path_template)
  text <- gsub(pattern = "TITLE", replacement = title, x = text)
  text <- gsub(pattern = "PACKAGE", replacement = package, x = text)
  text <- gsub(pattern = "GUID", replacement = guid, x = text)
  text <- gsub(pattern = "DIRECTORY", replacement = directory, x = text)
  path <- file.path(path_directory, paste0(package, ".xml"))
  writeLines(text, path)
}
