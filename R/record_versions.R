#' @title Record the manifest of package versions.
#' @export
#' @keywords internal
#' @description Record the manifest of versions of packages
#'   and their MD5 hashes.
#' @details This function tracks a manifest containing the current version,
#'   the current MD5 hash, the highest version ever released, and
#'   the MD5 hash of the highest version ever released. Each time it runs,
#'   it reads scrapes the package repository for the current releases,
#'   reads the old manifest, and updates recorded highest version and MD5
#'   for all packages which incremented their version numbers.
#'   After recording these incremented versions, the current version should
#'   be the highest version, and the current and highest-version
#'   MD5 hashes should agree. Packages
#'   that fall out of alignment are recorded in a small JSON with only
#'   the packages with version issues.
#' @return `NULL` (invisibly). Writes a package version manifest
#'   and a manifest of version issues as JSON files.
#' @param manifest Character of length 1, file path to the JSON manifest.
#' @param issues Character of length 1, file path to a JSON file
#'   which records packages with version issues.
#' @param repos Character string of package repositories to track.
record_versions <- function(
  manifest = "versions.json",
  issues = "version_issues.json",
  repos = "https://r-releases.r-universe.dev"
) {
  current <- get_current_versions(repos = repos)
  if (!file.exists(manifest)) {
    jsonlite::write_json(x = current, path = manifest, pretty = TRUE)
    return(invisible())
  }
  previous <- read_versions_previous(manifest = manifest)
  new <- update_version_manifest(current = current, previous = previous)
  jsonlite::write_json(x = new, path = manifest, pretty = TRUE)
  new_issues <- new[!versions_aligned(new = new),, drop = FALSE] # nolint
  jsonlite::write_json(x = new_issues, path = issues, pretty = TRUE)
  invisible()
}

get_current_versions <- function(repos) {
  out <- available.packages(repos = "https://r-releases.r-universe.dev")
  out <- as.data.frame(out)
  out <- out[, c("Package", "Version", "MD5sum")]
  colnames(out) <- c("package", "version_current", "md5_current")
  rownames(out) <- NULL
  out
}

read_versions_previous <- function(manifest) {
  out <- jsonlite::read_json(path = manifest)
  out <- do.call(what = vctrs::vec_rbind, args = out)
  for (field in colnames(out)) {
    out[[field]] <- as.character(out[[field]])
  }
  if (is.null(out$version_highest)) {
    out$version_highest <- out$version_current
  }
  if (is.null(out$md5_highest)) {
    out$md5_highest <- out$md5_current
  }
  out$version_current <- NULL
  out$md5_current <- NULL
  out
}

update_version_manifest <- function(current, previous) {
  new <- merge(x = current, y = previous, all = TRUE)
  incremented <- manifest_compare_versions(manifest = new) > 0.5
  new$version_highest[incremented] <- new$version_current[incremented]
  new$md5_highest[incremented] <- new$md5_current[incremented]
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

versions_aligned <- function(manifest) {
  versions_agree <- manifest$version_current == manifest$version_highest
  hashes_agree <- manifest$md5_current == manifest$md5_highest
  versions_agree & hashes_agree
}
