#' @title Validate a Package Entry
#' @export
#' @keywords internal
#' @description Validate a package entry.
#' @return A character string if there is a problem with the package entry,
#'   otherwise `NULL` if there are no issues.
#' @param name Character of length 1, package name.
#' @param url Usually a character of length 1 with the package URL.
assert_package <- function(name, url) {
  if (any(grepl(pattern = "\\}|\\{", x = url))) {
    return(
      paste("Entry of package", shQuote(name), "looks like custom JSON")
    )
  }
  out <- assert_package_lite(name = name, url = url)
  if (!is.null(out)) {
    return(out)
  }
  name <- trimws(name)
  url <- trimws(trim_trailing_slash(url))
  parsed_url <- try(url_parse(url), silent = TRUE)
  if (!identical(name, basename(parsed_url[["path"]]))) {
    return(
      paste(
        "Package name",
        shQuote(name),
        "appears to disagree with the repository name in the URL",
        shQuote(url)
      )
    )
  }
  if (!(parsed_url[["hostname"]] %in% c("github.com", "gitlab.com"))) {
    return(paste("URL", shQuote(url), "is not a GitHub or GitLab URL."))
  }
  splits <- strsplit(parsed_url[["path"]], split = "/", fixed = TRUE)[[1L]]
  splits <- splits[nzchar(splits)]
  if (length(splits) < 2L) {
    return(
      paste(
        "URL",
        shQuote(url),
        "appears to be an owner, not a repository."
      )
    )
  }
  owner <- tolower(splits[nzchar(splits)][1L])
  if (identical(owner, "cran")) {
    return(paste("URL", shQuote(url), "appears to use a CRAN mirror."))
  }
  assert_cran_url(name = name, url = url)
  status <- nanonext::ncurl(url)[["status"]]
  if (status != 200L) {
    return(
      paste(
        "URL",
        shQuote(url),
        "returned HTTP error",
        nanonext::status_code(status)
      )
    )
  }
}

assert_package_lite <- function(name, url) {
  if (!is_package_name(name)) {
    return("Invalid package name.")
  }
  if (!is_character_scalar(url)) {
    return("Invalid package URL.")
  }
  name <- trimws(name)
  url <- trimws(trim_trailing_slash(url))
  parsed_url <- try(url_parse(url), silent = TRUE)
  if (inherits(parsed_url, "try-error")) {
    return(
      paste("Found malformed URL", shQuote(url), "of package", shQuote(name))
    )
  }
  if (!identical(parsed_url[["scheme"]], "https")) {
    return(paste("Scheme of URL", shQuote(url), "is not https."))
  }
}

is_package_name <- function(name) {
  is_character_scalar(name) && grepl(
    pattern = "^[a-zA-Z][a-zA-Z0-9.]*[a-zA-Z0-9]$",
    x = trimws(name)
  )
}
