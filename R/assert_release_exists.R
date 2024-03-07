#' @title Check for a release.
#' @export
#' @keywords internal
#' @description Check for a release.
#' @return A character string if there is a problem with the package entry,
#'   otherwise `NULL` if there are no issues.
#' @inheritParams assert_package
assert_release_exists <- function(url) {
  host <- url_parse(url)[["host"]]
  if (host == "github.com") {
    assert_release_github(url)
  } else if (host == "gitlab.com") {
    assert_release_gitlab(url)
  }
}

assert_release_github <- function(url) {
  parsed_url <- url_parse(url)
  releases <- try(
    gh::gh(
      endpoint = "/repos/:owner/:repo/releases",
      owner = basename(dirname(parsed_url[["path"]])),
      repo = basename(parsed_url[["path"]])
    ),
    silent = TRUE
  )
  if (inherits(releases, "try-error")) {
    return(try_message(releases))
  }
  releases <- Filter(
    f = function(release) {
      !isTRUE(release$prerelease)
    },
    x = releases
  )
  if (!length(releases)) {
    return(paste("No full release found at URL", shQuote(url)))
  }
}

assert_release_gitlab <- function(url) {
  parsed_url <- url_parse(url)
  owner <- basename(dirname(parsed_url[["path"]]))
  repo <- basename(parsed_url[["path"]])
  endpoint <- sprintf(
    "https://gitlab.com/api/v4/projects/%s%s%s/releases",
    owner,
    "%2F",
    repo
  )
  releases <- try(
    suppressWarnings(
      jsonlite::stream_in(
        con = gzcon(url(endpoint)),
        simplifyVector = TRUE,
        simplifyDataFrame = TRUE
      )
    ),
    silent = TRUE
  )
  if (inherits(releases, "try-error")) {
    return(try_message(releases))
  }
  if (!nrow(releases) || !any(!releases$upcoming_release)) {
    return(paste("No full release found at URL", shQuote(url)))
  }
}
