#' @title Validate a Package Entry
#' @export
#' @keywords internal
#' @description Validate a package entry.
#' @return A character string if there is a problem with the package entry,
#'   otherwise `NULL` if there are no issues.
assert_package <- function(name, url) {
  if (!is_character_scalar(name)) {
    return("Invalid package name.")
  }
  if (!is_character_scalar(url)) {
    return("Invalid package URL.")
  }
  name <- trimws(name)
  url <- trimws(trim_trailing_slash(url))
  valid_package_name <- grepl(
    pattern = "^[a-zA-Z][a-zA-Z0-9.]*[a-zA-Z0-9]$",
    x = name
  )
  if (!isTRUE(valid_package_name)) {
    return(paste("Found invalid package name:", shQuote(name)))
  }
  parsed_url <- try(url_parse(url), silent = TRUE)
  if (inherits(parsed_url, "try-error")) {
    return(
      paste("Found malformed URL", shQuote(url), "of package", shQuote(name))
    )
  }
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
  if (!identical(parsed_url[["scheme"]], "https")) {
    return(paste("Scheme of URL", shQuote(url), "is not https."))
  }
  host <- tolower(parsed_url[["hostname"]])
  if (!(host %in% c("github.com", "gitlab.com"))) {
    return(paste("URL", shQuote(url), "is not a GitHub or GitLab URL."))
  }
  splits <- strsplit(parsed_url[["path"]], split = "/", fixed = TRUE)[[1L]]
  splits <- splits[nzchar(splits)]
  if (length(splits) < 1L) {
    return(invisible())
  }
  owner <- tolower(splits[nzchar(splits)][1L])
  if (identical(owner, "cran")) {
    return(paste("URL", shQuote(url), "appears to use a CRAN mirror."))
  }
  assert_cran_url(name = name, url = url)
}
