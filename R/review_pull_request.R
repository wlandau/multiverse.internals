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
#' @return `NULL` (invisibly).
#' @param owner Character of length 1, name of the universe repository owner.
#' @param repo Character of length 1, name of the universe repository.
#' @param number Positive integer of length 1, index of the pull request
#'   in the repo.
review_pull_request <- function(
  owner = "r-releases",
  repo = "r-releases",
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
          " attempts to modify files outside the 'packages/' folder. ",
          "Please open a different PR that simply adds one or ",
          "more files in the 'packages/' folder with their URLs."
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
          "folder. Manual review required."
        )
      )
      return(invisible())
    }
    name <- basename(file$filename)
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
          ". Manual review required."
        )
      )
      return(invisible())
    }
    url <- gsub(pattern = "^.*\\+", replacement = "", x = file$patch)
    result <- assert_package_contents(name = name, url = url)
    if (!is.null(result)) {
      pull_request_defer(
        owner = owner,
        repo = repo,
        number = number,
        message = paste0(
          "Pull request ",
          number,
          " automated diagnostics failed: ",
          result,
          ". Manual review required."
        )
      )
      return(invisible())
    }
    result <- assert_package_url(name = name, url = url)
    if (!is.null(result)) {
      pull_request_defer(
        owner = owner,
        repo = repo,
        number = number,
        message = paste0(
          "Pull request ",
          number,
          " automated diagnostics failed: ",
          result,
          ". Manual review required."
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
    body = message
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
    if (!(label_manual_review %in% labels)) {
      gh::gh(
        "POST /repos/:owner/:repo/issues/:number/comments",
        owner = owner,
        repo = repo,
        number = number,
        body = paste0(
          "There was a problem merging pull request ",
          number,
          ". Manual review required. "
        )
      )
      gh::gh(
        "POST /repos/:owner/:repo/issues/:number/labels",
        owner = owner,
        repo = repo,
        number = number,
        labels = list(label_manual_review)
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
        "to incorporate new packages in the r-releases universe.",
        "An automated GitHub actions job will migrate the packages to",
        "https://github.com/r-releases/r-releases.r-universe.dev",
        "If you have not done so already,",
        "please create a new GitHub tag and release of each",
        "package to ensure it gets published in the universe.",
        "You can check status and progress at",
        "https://r-releases.r-universe.dev."
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
