#' @title Validate a package entry.
#' @export
#' @keywords internal
#' @description Validate a package entry.
#' @return An character string if there is a problem with the package entry,
#'   otherwise `NULL` if there are no issues.
assert_package <- function(path) {
  if (!is_character_scalar(path)) {
    return("Invalid package file path.")
  }
  if (!file.exists(path)) {
    return(paste("file", shQuote(path), "does not exist."))
  }
  name <- basename(path)
  url <- try(readLines(path, warn = FALSE), silent = TRUE)
  if (inherits(url, "try-error")) {
    return(paste("Problem reading file", shQuote(path)))
  }
  assert_package_contents(name = name, url = url)
}

assert_package_contents <- function(name, url) {
  good_package_name <- grepl(
    pattern = "^[a-zA-Z][a-zA-Z0-9.]*[a-zA-Z0-9]$",
    x = name
  )
  if (!isTRUE(good_package_name)) {
    return(paste("Found invalid package name: ", shQuote(name)))
  }
  if (!is_character_scalar(url)) {
    return("Invalid package URL.")
  }
  good_url <- grepl(
    pattern = "^https?://[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,3}(/\\S*)?$",
    x = url
  )
  if (!isTRUE(good_url)) {
    return(
      paste("Found malformed URL", shQuote(url), "of package", shQuote(name))
    )
  }
  NULL
}
