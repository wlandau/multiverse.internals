#' @title Review R-multiverse contribution pull requests.
#' @export
#' @family Automated package reviews
#' @description Review pull requests which add packages to `packages.json`.
#' @return `NULL` (invisibly).
#' @inheritParams review_pull_request
review_pull_requests <- function(
  owner = "r-multiverse",
  repo = "contributions"
) {
  defaults <- list(
    cli.ansi = getOption("cli.ansi"),
    cli.num_colors = getOption("cli.num_colors"),
    cli.unicode = getOption("cli.unicode")
  )
  options(cli.ansi = FALSE, cli.num_colors = 1L, cli.unicode = FALSE)
  on.exit(do.call(what = options, args = defaults))
  assert_character_scalar(owner, "owner must be a character string")
  assert_character_scalar(repo, "repo must be a character string")
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
  message("Skimming the R Consortium Advisory Database...")
  advisories <- unique(read_advisories()$package)
  message("Listing trusted GitHub organizations...")
  organizations <- list_organizations(owner = owner, repo = repo)
  message("About to review ", length(pull_requests), " open pull requests.")
  for (pull_request in pull_requests) {
    review_pull_request(
      owner = owner,
      repo = repo,
      number = pull_request$number,
      advisories = advisories,
      organizations = organizations
    )
  }
  invisible()
}
