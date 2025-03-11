#' @title Filter `PACKAGES` and `PACKAGES.gz` files.
#' @export
#' @family staging
#' @description Filter the `PACKAGES` and `PACKAGES.gz` files to
#'   only list certain packages.
#' @param path Directory path where `PACKAGES` and `PACKAGES.gz` files
#'   reside locally.
#' @param staged Path to the `"staged.json"` file which lists staged packages
#'   in the Staging universe.
#' @examples
#' \dontrun{
#'   path <- tempfile()
#'   dir.create(path)
#'   mock <- system.file(
#'     "packages",
#'     package = "multiverse.internals",
#'     mustWork = TRUE
#'   )
#'   file.copy(mock, path, recursive = TRUE)
#'   staged <- tempfile()
#'   jsonlite::write_json(c("crew", "mirai"), staged, pretty = TRUE)
#'   filter_packages(path, staged)
#' }
filter_packages <- function(path, staged) {
  include <- jsonlite::read_json(staged, simplifyVector = TRUE)
  listings <- list.files(
    path,
    recursive = TRUE,
    pattern = "PACKAGES$",
    full.names = TRUE
  )
  lapply(listings, filter_packages_file, include = include)
  invisible()
}

filter_packages_file <- function(path, include) {
  data <- read.dcf(file = path)
  data <- data[data[, "Package"] %in% include,, drop = FALSE] # nolint
  write.dcf(x = data, file = path)
  R.utils::gzip(
    filename = path,
    destname = file.path(dirname(path), "PACKAGES.gz"),
    overwrite = TRUE,
    remove = FALSE
  )
}
