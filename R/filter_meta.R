#' @title Filter `PACKAGES` and `PACKAGES.gz` metadata files.
#' @export
#' @family staging
#' @description Filter the `PACKAGES` and `PACKAGES.gz` files to
#'   only list certain packages.
#' @param path_meta Directory path where `PACKAGES` and `PACKAGES.gz` files
#'   reside locally.
#' @param path_staging Path to a GitHub clone of the Staging universe.
#' @examples
#' \dontrun{
#' path_meta <- tempfile()
#' dir.create(path_meta)
#' mock <- system.file(
#'   file.path("mock", "meta"),
#'   package = "multiverse.internals",
#'   mustWork = TRUE
#' )
#' file.copy(mock, path_meta, recursive = TRUE)
#' path_staging <- tempfile()
#' url_staging <- "https://github.com/r-multiverse/staging"
#' gert::git_clone(url = url_staging, path = path_staging)
#' filter_meta(path_meta, path_staging)
#' }
filter_meta <- function(path_meta, path_staging) {
  listings <- list.files(
    path_meta,
    recursive = TRUE,
    pattern = "PACKAGES$",
    full.names = TRUE
  )
  lapply(
    listings,
    filter_packages_file,
    staged = staged_packages(file.path(path_staging, "packages.json"))
  )
  invisible()
}

filter_packages_file <- function(path, staged) {
  data <- read.dcf(file = path)
  data <- data[data[, "Package"] %in% staged,, drop = FALSE] # nolint
  write.dcf(x = data, file = path)
  R.utils::gzip(
    filename = path,
    destname = file.path(dirname(path), "PACKAGES.gz"),
    overwrite = TRUE,
    remove = FALSE
  )
}

staged_packages <- function(path) {
  json_staging <- jsonlite::read_json(path, simplifyVector = TRUE)
  json_staging$package[json_staging$branch != "*release"]
}
