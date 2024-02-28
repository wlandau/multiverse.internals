#' @title Validate a Package Entry
#' @export
#' @keywords internal
#' @description Validate a package entry.
#' @return A character string if there is a problem with the package entry,
#'   otherwise `NULL` if there are no issues.
assert_package <- function(path) {
  if (!is_character_scalar(path)) {
    return("Invalid package file path")
  }
  if (!file.exists(path)) {
    return(paste("file", shQuote(path), "does not exist"))
  }
  name <- trimws(basename(path))
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
    return("Invalid package URL")
  }
  url <- trimws(url)
  good_url <- grepl(
    pattern = "^https?://[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,3}(/\\S*)?$",
    x = url
  )
  if (!isTRUE(good_url)) {
    return(
      paste("Found malformed URL", shQuote(url), "of package", shQuote(name))
    )
  }
}

#' @title Validate a Package URL
#' @export
#' @keywords internal
#' @description Validate that the package URL is in the description file if on
#'   CRAN.
#' @return A character string if there is a problem with the URL for the given
#'   package name, otherwise `NULL` if there are no issues.
assert_package_url <- function(name, url) {
  
  res <- ps(name, size = 1L)
  pkg <- res[["package"]]
  if (length(pkg) && name == pkg) {
    purl <- parse_url(sub("/$", "", url, perl = TRUE))
    urls <- strsplit(res[["url"]], ",\n|, |\n", perl = TRUE)[[1L]]
    for (u in urls) {
      pu <- parse_url(sub("/$", "", u, perl = TRUE))
      purl[["host"]] == pu[["host"]] && purl[["path"]] == pu[["path"]] &&
        return(invisible())
    }
    burl <- parse_url(res[["bugreports"]])
    purl[["host"]] == burl[["host"]] &&
      purl[["path"]] == sub("/issues/*$", "", burl[["path"]], perl = TRUE) &&
      return(invisible())
    return(
      paste("CRAN package", shQuote(name), "does not have URL", shQuote(url))
    )
  }
  
}
