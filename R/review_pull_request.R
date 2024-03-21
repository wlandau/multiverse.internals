#' @title Review a pull request.
#' @export
#' @family pull request reviews
#' @description Review a pull request to add packages to `packages.json`.
#' @section Testing:
#'   Testing of this function unfortunately needs to be manual. Test cases:
#'   1. Add a package correctly (automatically merge).
#'   2. Add a bad URL (manual review).
#'   3. Change a URL (manual review).
#'   4. Add a file in a forbidden place (close).
#'   5. Add a custom JSON file which can be parsed (manual review).
#' @return `NULL` (invisibly).
#' @param owner Character of length 1, name of the universe repository owner.
#' @param repo Character of length 1, name of the universe repository.
#' @param number Positive integer of length 1, index of the pull request
#'   in the repo.
review_pull_request <- function(
  owner = "r-multiverse",
  repo = "contributions",
  number
) {
  assert_character_scalar(owner)
  assert_character_scalar(repo)
  assert_positive_scalar(number)
  message("Reviewing pull request ", number)
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
      pull_request_close(
        owner = owner,
        repo = repo,
        number = number,
        message = paste0(
          "Pull request ",
          number,
          " attempts to modify files outside the 'packages/' folder ",
          "or in a subdirectory of 'packages/'. ",
          "Please open a different pull request that simply adds one or ",
          "more text files directly inside 'packages/' with package URLs."
        )
      )
      return(invisible())
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
      return(invisible())
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
      return(invisible())
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
      return(invisible())
    }
    url <- gsub(pattern = "^.*\\+", replacement = "", x = file$patch)
    url <- gsub(pattern = "\\s.*$", replacement = "", x = url)
    result <- assert_package(name = name, url = url)
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
      return(invisible())
    }
  }
  pull_request_merge(
    owner = owner,
    repo = repo,
    number = number
  )
  invisible()
}

pull_request_close <- function(owner, repo, number, message) {
  gh::gh(
    "PATCH /repos/:owner/:repo/pulls/:number",
    owner = owner,
    repo = repo,
    number = number,
    state = "closed"
  )
  gh::gh(
    "POST /repos/:owner/:repo/issues/:number/comments",
    owner = owner,
    repo = repo,
    number = number,
    body = message
  )
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
        "An automated GitHub actions job will deploy the packages",
        "as described at https://r-multiverse.github.io/.",
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
