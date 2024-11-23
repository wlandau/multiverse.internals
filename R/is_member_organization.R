#' @title Check GitHub organization membership
#' @export
#' @keywords internal
#' @description Check if a GitHub user is a member of at least one
#'   of the organizations given.
#' @return `TRUE` if the user is a member of at least one of the given
#'   GitHub organizations, `FALSE` otherwise.
#' @param user Character string, GitHub user name.
#' @param organizations Character vector of names of GitHub organizations.
is_member_organization <- function(user, organizations) {
  response <- gh::gh("/users/{user}/orgs", user = user)
  membership <- unlist(lapply(response, function(entry) entry$login))
  any(membership %in% organizations)
}
