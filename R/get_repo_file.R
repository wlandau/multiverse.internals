#' @title Get repo file
#' @keywords internal
#' @description Get the contents of a file from a GitHub or GitLab repo.
#' @return A character string with the contents of the file.
#' @param url Character string, URL of a GitHub or GitLab repository.
#' @param path Character string, path to the file in the repository.
get_repo_file <- function(url, path) {
  host <- url_parse(url)[["hostname"]]
  if (host == "github.com") {
    get_repo_file_github(url, path)
  } else if (host == "gitlab.com") {
    get_repo_file_gitlab(url, path)
  }
}

get_repo_file_github <- function(url, path) {
  response <- gh::gh(
    "/repos/{owner}/{repo}/contents/{path}",
    owner = basename(dirname(url)),
    repo = basename(url),
    path = path
  )
  rawToChar(base64enc::base64decode(response$content))
}

get_repo_file_gitlab <- function(url, path) {
  query <- sprintf(
    "https://gitlab.com/api/v4/projects/%s%s%s/repository/files/%s/raw",
    basename(dirname(url)),
    "%2F",
    basename(url),
    path
  )
  response <- nanonext::ncurl(url = query)
  stopifnot(response$response == 200L)
  response$data
}
