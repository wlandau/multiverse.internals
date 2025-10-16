#' @title Validate the URL of a package on CRAN.
#' @export
#' @keywords internal
#' @description If the package is on CRAN, validate that the contributed URL
#'   appears in the CRAN-hosted DESCRIPTION file.
#' @return A character string if there is a problem with the package entry,
#'   otherwise `NULL` if there are no issues.
#' @param name Character of length 1, contributed name of the package.
#' @param url Character of length 1, contributed URL of the package.
assert_cran_url <- function(name, url) {
  result <- try(
    pkgsearch::cran_package(name = name),
    silent = TRUE
  )
  if (inherits(result, "try-error")) {
    return(invisible())
  }
  package <- result[["Package"]]
  main_urls <- unlist(
    strsplit(
      as.character(result[["URL"]]),
      ",\n|, |\n",
      perl = TRUE
    )
  )
  bugs_url <- sub(
    pattern = "/issues/*$",
    replacement = "",
    x = result[["BugReports"]],
    perl = TRUE
  )
  cran_urls <- c(main_urls, bugs_url)
  urls_agree <- lapply(
    X = cran_urls,
    FUN = url_agrees_with_cran,
    reference = url
  )
  if (any(unlist(urls_agree))) {
    return(invisible())
  }
  paste(
    "URL of package",
    shQuote(name),
    "is given as",
    shQuote(url),
    "but does not appear in its DESCRIPTION file published on CRAN."
  )
}

url_agrees_with_cran <- function(cran, reference) {
  parsed_cran <- url_parse(cran)
  parsed_reference <- url_parse(reference)
  (parsed_cran[["hostname"]] == parsed_reference[["hostname"]]) &&
    (parsed_cran[["path"]] == parsed_reference[["path"]])
}
