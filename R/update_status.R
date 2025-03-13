#' @title Update the package status repository
#' @export
#' @family status
#' @description Update the repository which reports the status on individual
#'   packages.
#' @param path_status Character string, directory path to the source files
#'   of the package status repository.
#' @param path_staging Character string, local directory path
#'   to the clone of the Staging universe GitHub repository.
#' @param path_community Character string, local directory path
#'   to the clone of the Community universe GitHub repository.
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
#'   path_community = path_community
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
  path_community
) {
  update_status_directory(
    output = path_status,
    input = path_staging,
    directory = "staging"
  )
  update_status_directory(
    output = path_status,
    input = path_community,
    directory = "community"
  )
  update_status_production(
    output = path_status,
    input = path_staging
  )
}

update_status_production <- function(output, input) {
  path_status <- file.path(input, "status.json")
  path_snapshot <- file.path(input, "snapshot.json")
  status <- jsonlite::read_json(path_status, simplifyVector = TRUE)
  staged <- staged_packages(input)
  snapshot <- jsonlite::read_json(path_snapshot, simplifyVector = TRUE)
  status <- as.data.frame(do.call(rbind, status[staged]))
  url <- sprintf(
    "[`%s`](https://r-multiverse.org/status/staging/%s)",
    staged,
    staged
  )
  lines_packages <- paste0(
    "|",
    paste(url, status$version, status$date, sep = "|"),
    "|"
  )
  lines_packages <- paste(lines_packages, collapse = "\n")
  file_template <- system.file(
    file.path("status", "production.md"),
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
  writeLines(lines_page, file.path(output, "production.md"))
}

update_status_directory <- function(output, input, directory) {
  path_directory <- file.path(output, directory)
  unlink(path_directory, recursive = TRUE, force = TRUE)
  dir.create(path_directory, recursive = TRUE)
  path_status <- file.path(input, "status.json")
  json_status <- jsonlite::read_json(path_status, simplifyVector = TRUE)
  packages <- names(json_status)
  remote_hashes <- vapply(
    json_status,
    \(x) {
      if (length(x$remote_hash)) {
        as.character(x$remote_hash)
      } else {
        as.character(x$date)
      }
    },
    character(1L)
  )
  for (index in seq_along(packages)) {
    package <- packages[index]
    guid <- remote_hashes[index]
    success <- json_status[[package]]$success
    if (isTRUE(success)) {
      suffix <- "success"
    } else if (isFALSE(success)) {
      suffix <- "issues found"
    } else {
      suffix <- "status unknown"
    }
    title <- paste0(package, ": ", suffix)
    status <- interpret_status(package, json_status)
    update_status_html(package, title, status, path_directory)
    update_status_xml(package, title, path_directory, guid)
  }
  failures <- Filter(json_status, f = \(x) isFALSE(x$success))
  update_status_summary(output, directory, failures)
}

update_status_summary <- function(output, directory, status) {
  lines_packages <- ""
  if (length(status)) {
    package <- names(status)
    version <- vapply(
      status,
      \(x) as.character(x$version),
      FUN.VALUE = character(1L)
    )
    date <- vapply(
      status,
      \(x) as.character(x$date),
      FUN.VALUE = character(1L)
    )
    url <- sprintf(
      "[`%s`](https://r-multiverse.org/status/staging/%s)",
      package,
      package
    )
    lines_packages <- paste0(
      "|",
      paste(url, status$version, status$date, sep = "|"),
      "|"
    )
    lines_packages <- paste(lines_packages, collapse = "\n")
  }
  template <- system.file(
    file.path("status", "repository.md"),
    package = "multiverse.internals",
    mustWork = TRUE
  )
  lines <- readLines(template)
  lines <- gsub(
    pattern = "DIRECTORY",
    replacement = tools::toTitleCase(directory),
    x = lines
  )
  lines <- gsub(pattern = "PACKAGES", replacement = lines_packages, x = lines)
  writeLines(lines, file.path(output, paste0(directory, ".md")))
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
