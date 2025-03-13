#' @title Record the manifest of package versions.
#' @export
#' @family package check data management
#' @description Record the manifest of versions of packages
#'   and their hashes.
#' @details This function tracks a manifest containing the current version,
#'   the current hash, the highest version ever released, and
#'   the hash of the highest version ever released.
#'   [status_versions()] uses this information
#'   to determine whether the package complies with best
#'   practices for version numbers.
#' @inheritSection record_status Package status
#' @return `NULL` (invisibly). Writes version information to a JSON file.
#' @inheritParams meta_checks
#' @param versions Character of length 1, file path to a JSON manifest
#'   tracking the history of released versions of packages.
#' @param current A data frame of current versions and hashes of packages
#'   in `repo`. This argument is exposed for testing only.
#' @examples
#'   # R-multiverse uses https://community.r-multiverse.org as the repo.
#'   repo <- "https://wlandau.r-universe.dev" # just for testing and examples
#'   output <- tempfile()
#'   versions <- tempfile()
#'   # First snapshot:
#'   record_versions(
#'     versions = versions,
#'     repo = repo
#'   )
#'   readLines(versions)
#'   # In subsequent snapshots, we have historical information about versions.
#'   record_versions(
#'     versions = versions,
#'     repo = repo
#'   )
#'   readLines(versions)
record_versions <- function(
  versions = "versions.json",
  repo = "https://community.r-multiverse.org",
  current = multiverse.internals::get_current_versions(repo = repo)
) {
  if (!file.exists(versions)) {
    jsonlite::write_json(x = current, path = versions, pretty = TRUE)
    return(invisible())
  }
  previous <- read_versions_previous(versions = versions)
  new <- update_versions(current = current, previous = previous)
  jsonlite::write_json(x = new, path = versions, pretty = TRUE)
  invisible()
}

#' @title Get the current versions of packages
#' @export
#' @keywords internal
#' @description Get the current versions of packages in the repo.
#' @return A data frame of packages with their current versions and hashes.
#' @inheritParams record_versions
get_current_versions <- function(
  repo = "https://community.r-multiverse.org"
) {
  meta <- meta_packages(repo = repo)
  meta <- meta[, c("package", "version", "remotesha")]
  colnames(meta) <- c("package", "version_current", "hash_current")
  rownames(meta) <- NULL
  meta
}

read_versions_previous <- function(versions) {
  out <- jsonlite::read_json(path = versions, simplifyVector = TRUE)
  if (is.null(out$version_highest)) {
    out$version_highest <- out$version_current
  }
  if (is.null(out$hash_highest)) {
    out$hash_highest <- out$hash_current
  }
  out$version_current <- NULL
  out$hash_current <- NULL
  out
}

update_versions <- function(current, previous) {
  new <- merge(
    x = current,
    y = previous,
    by = "package",
    all.x = TRUE,
    all.y = FALSE
  )
  incremented <- compare_versions(versions = new) == 1L
  new$version_highest[incremented] <- new$version_current[incremented]
  new$hash_highest[incremented] <- new$hash_current[incremented]
  new
}

compare_versions <- function(versions) {
  apply(
    X = versions,
    MARGIN = 1L,
    FUN = function(row) {
      utils::compareVersion(
        a = .subset2(row, "version_current"),
        b = .subset2(row, "version_highest")
      )
    }
  )
}
