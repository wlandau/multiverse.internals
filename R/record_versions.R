#' @title Record the manifest of package versions.
#' @export
#' @description Record the manifest of versions of packages
#'   and their hashes.
#' @details This function tracks a manifest containing the current version,
#'   the current hash, the highest version ever released, and
#'   the hash of the highest version ever released. It uses this information
#'   to determine whether the package complies with best
#'   practices for version numbers.
#' @return `NULL` (invisibly). Writes a package version manifest
#'   and a manifest of version issues as JSON files.
#' @param manifest Character of length 1, file path to the JSON manifest.
#' @param issues Character of length 1, file path to a JSON file
#'   which records packages with version issues.
#' @param repos Character string of package repositories to track.
#' @param current A data frame of current versions and hashes of packages
#'   in `repos`. This argument is exposed for testing only.
#' @param check_hash Logical of length 1, check hashes when judging package
#'   version compliance. This allows [record_versions()] to flag packages
#'   that create new releases but keep the same version number.
record_versions <- function(
  manifest = "versions.json",
  issues = "version_issues.json",
  repos = "https://r-releases.r-universe.dev",
  current = r.releases.utils::get_current_versions(repos = repos),
  check_hash = TRUE
) {
  if (!file.exists(manifest)) {
    jsonlite::write_json(x = current, path = manifest, pretty = TRUE)
    return(invisible())
  }
  previous <- read_versions_previous(manifest = manifest)
  new <- update_version_manifest(current = current, previous = previous)
  jsonlite::write_json(x = new, path = manifest, pretty = TRUE)
  aligned <- versions_aligned(manifest = new, check_hash = check_hash)
  new_issues <- new[!aligned,, drop = FALSE] # nolint
  jsonlite::write_json(x = new_issues, path = issues, pretty = TRUE)
  invisible()
}

#' @title Get the current versions of packages
#' @export
#' @keywords internal
#' @description Get the current versions of packages in the repos.
#' @return A data frame of packages with their current versions and hashes.
#' @inheritParams record_versions
get_current_versions <- function(
  repos = "https://r-releases.r-universe.dev"
) {
  out <- utils::available.packages(repos = repos)
  out <- out[, c("Package", "Version", "MD5sum")]
  out <- as.data.frame(out)
  colnames(out) <- c("package", "version_current", "hash_current")
  rownames(out) <- NULL
  out
}

read_versions_previous <- function(manifest) {
  out <- jsonlite::read_json(path = manifest)
  out <- do.call(what = vctrs::vec_rbind, args = out)
  out <- lapply(out, as.character)
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

update_version_manifest <- function(current, previous) {
  new <- merge(x = current, y = previous, all = TRUE)
  incremented <- manifest_compare_versions(manifest = new) == 1L
  new$version_highest[incremented] <- new$version_current[incremented]
  new$hash_highest[incremented] <- new$hash_current[incremented]
  new
}

manifest_compare_versions <- function(manifest) {
  apply(
    X = manifest,
    MARGIN = 1L,
    FUN = function(row) {
      utils::compareVersion(
        a = .subset2(row, "version_current"),
        b = .subset2(row, "version_highest")
      )
    }
  )
}

versions_aligned <- function(manifest, check_hash) {
  aligned <- manifest$version_current == manifest$version_highest
  if (check_hash) {
    aligned <- aligned & (manifest$hash_current == manifest$hash_highest)
  }
  aligned
}