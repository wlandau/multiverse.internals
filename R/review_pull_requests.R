#' @title Review pull requests.
#' @export
#' @family pull request reviews
#' @description Review pull requests which add packages to `packages.json`.
#' @inheritSection review_pull_request Testing
#' @return `NULL` (invisibly).
#' @inheritParams review_pull_request
review_pull_requests <- function(
  owner = "r-multiverse",
  repo = "contributions"
) {
  assert_character_scalar(owner)
  assert_character_scalar(repo)
  message("Listing pull requests...")
  pull_requests <- gh::gh(
    "/repos/:owner/:repo/pulls",
    owner = owner,
    repo = repo,
    state = "open",
    .limit = Inf
  )
  skip <- rep(FALSE, length(pull_requests))
  for (index in seq_along(pull_requests)) {
    labels <- pull_request_labels(pull_requests[[index]])
    skip[index] <- label_manual_review %in% labels
  }
  pull_requests <- pull_requests[!skip]
  message("About to review ", length(pull_requests), " open pull requests.")
  for (pull_request in pull_requests) {
    review_pull_request(
      owner = owner,
      repo = repo,
      number = pull_request$number
    )
  }
  invisible()
}
