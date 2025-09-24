#' @title Review an R-multiverse contribution
#' @export
#' @family pull request reviews
#' @description Review a pull request to add packages to R-multiverse.
#' @return `NULL` (invisibly).
#' @inheritParams meta_packages
#' @param owner Character of length 1, name of the package repository owner.
#' @param number Positive integer of length 1, index of the pull request
#'   in the repo.
#' @param advisories Character vector of names of packages with advisories
#'   in the R Consortium Advisory Database.
#'   If `NULL`, the function reads the database.
#' @param organizations Character vector of names of GitHub organizations.
#'   Pull requests from authors who are not members of at least one of
#'   these organizations will be flagged for manual review.
#'   If `NULL`, the function reads the list of trusted organizations.
review_pull_request <- function(
  owner = "r-multiverse",
  repo = "contributions",
  number,
  advisories = NULL,
  organizations = NULL
) {
  assert_character_scalar(owner, "owner must be a character string")
  assert_character_scalar(repo, "repo must be a character string")
  assert_positive_scalar(number, "number must be a positive integer")
  if (is.null(advisories)) {
    message("Skimming the R Consortium Advisory Database...")
    advisories <- unique(read_advisories()$package)
  }
  if (is.null(organizations)) {
    message("Listing trusted GitHub organizations...")
    organizations <- list_organizations(owner = owner, repo = repo)
  }
  message("Reviewing pull request ", number)
  merge <- review_pull_request_integrity(owner, repo, number, organizations) &&
    review_pull_request_content(owner, repo, number, advisories)
  if (isTRUE(merge)) {
    pull_request_merge(
      owner = owner,
      repo = repo,
      number = number
    )
  }
  invisible()
}

review_pull_request_integrity <- function(owner, repo, number, organizations) {
  pull <- gh::gh(
    "/repos/:owner/:repo/pulls/:number",
    owner = owner,
    repo = repo,
    number = number
  )
  commit <- gh::gh(
    "/repos/:owner/:repo/git/commits/:sha",
    owner = owner,
    repo = repo,
    sha = pull$head$sha
  )
  from_web_ui <- isTRUE(commit$verification$verified) &&
    identical(commit$committer$name, "GitHub") &&
    identical(commit$committer$email, "noreply@github.com")
  if (!from_web_ui) {
    pull_request_defer(
      owner = owner,
      repo = repo,
      number = number,
      message = paste0(
        "The latest commit (",
        pull$head$sha,
        ") of pull request ",
        number,
        " was not created using the point-and-click ",
        "web user interface on GitHub.com. ",
        "For security reasons, the R-multiverse bot only ",
        "merges commits created manually through a web browser ",
        "as described at https://r-multiverse.org/contributors.html."
      )
    )
    return(FALSE)
  }
  if (!is_member_organization(pull$user$login, organizations)) {
    pull_request_defer(
      owner = owner,
      repo = repo,
      number = number,
      message = paste0(
        "Author ",
        shQuote(pull$user$login),
        " of pull request ",
        number,
        " is not a public member of one of the GitHub organizations ",
        "listed at ",
        file.path(
          "https://github.com",
          owner,
          repo,
          "blob/main/organizations"
        ),
        ". For security, R-multiverse requires manual review ",
        "by a moderator in such cases. \n\n",
        "Or, if you would like to add an organization to the list, ",
        "please submit a new pull request (different from this one) to ",
        file.path(
          "https://github.com",
          owner,
          repo
        ),
        " to add it to the ",
        "'organizations' file. ",
        "If you are already a member of one of the listed ",
        "organizations, you may need to edit settings in GitHub ",
        "to [make your membership publicly visible](",
        file.path(
          "https://docs.github.com/en/account-and-profile",
          "setting-up-and-managing-your-personal-account-on-github",
          "managing-your-membership-in-organizations",
          "publicizing-or-hiding-organization-membership)."
        )
      )
    )
    return(FALSE)
  }
  TRUE
}

review_pull_request_content <- function(owner, repo, number, advisories) {
  response <- gh::gh(
    "/repos/:owner/:repo/pulls/:number/files",
    owner = owner,
    repo = repo,
    number = number
  )
  for (file in response) {
    close <- !identical(dirname(file$filename), "packages") ||
      !identical(dirname(dirname(file$filename)), ".")
    if (close) {
      pull_request_defer(
        owner = owner,
        repo = repo,
        number = number,
        message = paste0(
          "Pull request ",
          number,
          " attempts to modify files outside the 'packages/' folder ",
          "or in a subdirectory of 'packages/'. "
        )
      )
      return(FALSE)
    }
    if (!identical(file$status, "added")) {
      pull_request_defer(
        owner = owner,
        repo = repo,
        number = number,
        message = paste0(
          "Pull request ",
          number,
          " attempts an action other than adding files in the 'packages/' ",
          "folder."
        )
      )
      return(FALSE)
    }
    name <- basename(file$filename)
    if (file$additions != 1L) {
      pull_request_defer(
        owner = owner,
        repo = repo,
        number = number,
        message = paste(
          "Text file",
          shQuote(name),
          "in pull request",
          number,
          "has",
          file$additions,
          "lines. The file must have exactly 1 line",
          "unless it contains custom JSON (which is uncommon)."
        )
      )
      return(FALSE)
    }
    if (!is_character_scalar(file$patch)) {
      pull_request_defer(
        owner = owner,
        repo = repo,
        number = number,
        message = paste0(
          "Pull request ",
          number,
          " wrote something different than 1 URL for the file of package ",
          shQuote(name),
          "."
        )
      )
      return(FALSE)
    }
    url <- gsub(pattern = "^.*\\+", replacement = "", x = file$patch)
    url <- gsub(pattern = "\\s.*$", replacement = "", x = url)
    result <- assert_package(name = name, url = url, advisories = advisories)
    if (!is.null(result)) {
      pull_request_defer(
        owner = owner,
        repo = repo,
        number = number,
        message = paste0(
          "Pull request ",
          number,
          " automated checks returned findings: ",
          result
        )
      )
      return(FALSE)
    }
  }
  TRUE
}

pull_request_defer <- function(owner, repo, number, message) {
  gh::gh(
    "POST /repos/:owner/:repo/issues/:number/labels",
    owner = owner,
    repo = repo,
    number = number,
    labels = list(label_manual_review)
  )
  gh::gh(
    "POST /repos/:owner/:repo/issues/:number/comments",
    owner = owner,
    repo = repo,
    number = number,
    body = paste0(
      message,
      "\n\nThis pull request has been marked for manual review. ",
      "Please either wait for an R-multiverse moderator to review, ",
      "or close this pull request and open a different one ",
      "which passes automated checks."
    )
  )
}

pull_request_merge <- function(owner, repo, number) {
  out <- try(
    gh::gh(
      "PUT /repos/:owner/:repo/pulls/:number/merge",
      owner = owner,
      repo = repo,
      number = number,
      commit_title = paste("Merge pull request", number),
      commit_message = paste("Merge pull request", number)
    ),
    silent = TRUE
  )
  if (inherits(out, "try-error")) {
    pull_request <- gh::gh(
      "/repos/:owner/:repo/pulls/:number",
      owner = owner,
      repo = repo,
      number = number,
      state = "open",
      .limit = Inf
    )
    labels <- pull_request_labels(pull_request)
    if (!(label_retry_review %in% labels)) {
      gh::gh(
        "POST /repos/:owner/:repo/issues/:number/comments",
        owner = owner,
        repo = repo,
        number = number,
        body = paste0(
          "Pull request ",
          number,
          " is approved in its current state, ",
          "but the bot encountered an error trying to merge it. ",
          "The bot will repeat the review and retry the merge ",
          "next time it reviews pull requests."
        )
      )
      gh::gh(
        "POST /repos/:owner/:repo/issues/:number/labels",
        owner = owner,
        repo = repo,
        number = number,
        labels = list(label_retry_review)
      )
    }
  } else {
    gh::gh(
      "POST /repos/:owner/:repo/issues/:number/comments",
      owner = owner,
      repo = repo,
      number = number,
      body = paste(
        "This pull request was automatically merged",
        "to incorporate new packages into R-multiverse.",
        "An automated GitHub actions job will deploy the packages.",
        "Thank you for your contribution."
      )
    )
  }
}

pull_request_labels <- function(pull_request) {
  out <- lapply(
    X = pull_request$labels,
    FUN = function(x) {
      x$name
    }
  )
  as.character(unlist(out))
}

label_manual_review <- "manual-review"

label_retry_review <- "retry-review"
