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
  snapshot <- jsonlite::read_json(path_snapshot, simplifyVector = TRUE)
  staged <- staged_packages(input)
  rows <- status_rows(status[staged])
  file_template <- file.path(output, "staged.md")
  lines_page <- gsub(
    pattern = "SNAPSHOT",
    replacement = snapshot$snapshot,
    x = readLines(file_template)
  )
  lines_page <- gsub(
    pattern = "PACKAGES",
    replacement = rows,
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
    \(x) as.character(x$remote_hash),
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
    update_status_html(output, path_directory, package, title, status)
    update_status_xml(output, path_directory, package, title, guid)
  }
  failures <- Filter(json_status, f = \(x) isFALSE(x$success))
  update_status_summary(output, directory, failures)
}

update_status_summary <- function(output, directory, status) {
  rows <- status_rows(status)
  template <- file.path(output, "repository.md")
  lines <- readLines(template)
  lines <- gsub(
    pattern = "DIRECTORY",
    replacement = tools::toTitleCase(directory),
    x = lines
  )
  lines <- gsub(pattern = "PACKAGES", replacement = rows, x = lines)
  writeLines(lines, file.path(output, paste0(directory, ".md")))
}

update_status_html <- function(
  output,
  path_directory,
  package,
  title,
  status
) {
  path_template <- file.path(output, "status.html")
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

update_status_xml <- function(
  output,
  path_directory,
  package,
  title,
  guid
) {
  path_template <- file.path(output, "status.xml")
  directory <- basename(path_directory)
  text <- readLines(path_template)
  text <- gsub(pattern = "TITLE", replacement = title, x = text)
  text <- gsub(pattern = "PACKAGE", replacement = package, x = text)
  text <- gsub(pattern = "GUID", replacement = guid, x = text)
  text <- gsub(pattern = "DIRECTORY", replacement = directory, x = text)
  path <- file.path(path_directory, paste0(package, ".xml"))
  writeLines(text, path)
}

status_rows <- function(status) {
  if (!length(status)) {
    return("")
  }
  package <- names(status)
  version <- vapply(
    status,
    \(x) as.character(x$version),
    FUN.VALUE = character(1L)
  )
  published <- vapply(
    status,
    \(x) as.character(x$published),
    FUN.VALUE = character(1L)
  )
  url <- sprintf(
    "[%s](https://r-multiverse.org/status/staging/%s)",
    package,
    package
  )
  out <- paste0(
    "|",
    paste(url, version, published, sep = "|"),
    "|"
  )
  paste(out, collapse = "\n")
}
