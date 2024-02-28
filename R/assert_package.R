#' @title Validate a package entry.
#' @export
#' @keywords internal
#' @description Validate a package entry.
#' @return An character string if there is a problem with the package entry,
#'   otherwise `NULL` if there are no issues.
assert_package <- function(path) {
  if (!is_character_scalar(path)) {
    stop("Invalid package file path")
  }
  if (!file.exists(path)) {
    stop(paste("file", shQuote(path), "does not exist"))
  }
  name <- basename(path)
  url <- try(readLines(path, warn = FALSE), silent = TRUE)
  if (inherits(url, "try-error")) {
    stop(paste("Problem reading file", shQuote(path)))
  }
  assert_package_contents(name = name, url = url)
}

assert_package_contents <- function(name, url) {
  good_package_name <- grepl(
    pattern = "^[a-zA-Z][a-zA-Z0-9.]*[a-zA-Z0-9]$",
    x = name
  )
  if (!isTRUE(good_package_name)) {
    stop(paste("Found invalid package name: ", shQuote(name)))
  }
  if (!is_character_scalar(url)) {
    stop("Invalid package URL")
  }
  good_url <- grepl(
    pattern = "^https?://[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,3}(/\\S*)?$",
    x = url
  )
  if (!isTRUE(good_url)) {
    stop(
      paste("Found malformed URL", shQuote(url), "of package", shQuote(name))
    )
  }
}
