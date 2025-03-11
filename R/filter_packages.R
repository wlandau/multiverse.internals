#' @title Filter `PACKAGES` and `PACKAGES.gz` files.
#' @export
#' @family staging
#' @description Filter the `PACKAGES` and `PACKAGES.gz` files to
#'   only list certain packages.
#' @param path Directory path where `PACKAGES` and `PACKAGES.gz` files
#'   reside locally.
#' @param include Character vector of names of packages to include.
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
#'   filter_packages(path, include = c("crew", "mirai"))
#' }
filter_packages <- function(path, include) {
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
