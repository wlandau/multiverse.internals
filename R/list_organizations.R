#' @title List trusted GitHub organizations.
#' @keywords internal
#' @description List trusted GitHub organizations for the purposes
#'   of automated contribution reviews.
#' @details The R-multiverse contribution review bot flags contributions
#'   for manual review whose pull request authors are not public members of
#'   one of the trusted GitHub organizations.
#' @return A character vector of the names of trusted GitHub organizations.
#' @param owner Character string, name of the R-multiverse GitHub
#'   organization.
#' @param repo Character string, name of the R-multiverse contribution
#'   GitHub repository.
list_organizations <- function(
  owner = "r-multiverse",
  repo = "contributions"
) {
  response <- gh::gh(
    "/repos/{owner}/{repo}/contents/organizations",
    owner = owner,
    repo = repo,
    ref = "main"
  )
  text <- rawToChar(base64enc::base64decode(response$content))
  trimws(unlist(strsplit(text, split = "\n")))
}
