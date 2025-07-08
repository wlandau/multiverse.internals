#' @title Report package check synchronization issues.
#' @export
#' @family issues
#' @description Ensure the reported R-universe checks are synchronized.
#'   Report the packages whose checks have not been synchronized.
#' @details R-universe automatically rechecks downstream packages
#'   if an upstream dependency increments its version number.
#'   R-multiverse needs to wait for these downstream checks to finish
#'   before it makes decisions about accepting packages into Production.
#'   [issues_synchronization()] scrapes the GitHub Actions API to find out
#'   if any R-universe checks are still running for a package.
#'   In addition, to give rechecks enough time to post on GitHub Actions,
#'   it flags packages published within the last 5 minutes.
#' @return A `tibble` with one row per package and the following columns:
#'   * `package`: Name of the package.
#'   * `synchronization`: Synchronization status: `"success"` if
#'     the checks are synchronized, `"incomplete"` if checks are still
#'     running on R-universe GitHub Actions, and `"recent"` if the
#'     package was last published so recently that downstream checks
#'     may not have started yet.
#' @inheritParams issues_r_cmd_check
#' @examples
#'   \dontrun{
#'   meta <- meta_packages(repo = "https://wlandau.r-universe.dev")
#'   issues_synchronization(meta)
#'   }
issues_synchronization <- function(meta = meta_packages()) {
  recent <- issues_synchronization_recent(meta) #Needs to run first.
  monorepo <- utils::head(meta$monorepo, n = 1L)
  incomplete <- issues_synchronization_incomplete(monorepo)
  out <- data.frame(
    package = meta$package,
    synchronization = NA_character_
  )
  out$synchronization[out$package %in% recent] <- "recent"
  out$synchronization[out$package %in% recent] <- "incomplete"
  out[!is.na(out$synchronization), ]
}

issues_synchronization_recent <- function(meta) {
  now <- format_time_stamp(Sys.time())
  meta$package[difftime(now, meta$published, units = "mins") < 5]
}

# Monorepo is the name of the repo in https://github.com/r-universe.
# Examples: "r-multiverse, "r-multiverse-staging", "ropensci"
issues_synchronization_incomplete <- function(monorepo) {
  incomplete <- character(0L)
  statuses <- c("in_progress", "queued", "requested", "waiting", "pending")
  for (status in statuses) {
    # Unfortunately the query could be so long that we need manual pagination.
    page <- 1L
    per_page <- 100L
    n_runs <- per_page
    while (n_runs == per_page) {
      runs <- gh(
        "GET /repos/r-universe/{repo}/actions/runs",
        repo = monorepo,
        status = status,
        page = page,
        .per_page = per_page
      )
      names <- unlist(
        lapply(
          runs$workflow_runs,
          function(run) {
            if (basename(run$path) == "build.yml") {
              utils::head(strsplit(run$name, split = " ")[[1L]], n = 1L)
            } else {
              NA_character_
            }
          }
        )
      )
      incomplete <- c(incomplete, stats::na.omit(names))
      n_runs <- length(runs$workflow_runs)
      page <- page + 1L
    }
  }
  unique(incomplete)
}
