#' @title Record the manifest of package versions.
#' @export
#' @keywords internal
#' @description Record the manifest of versions of packages
#'   and their MD5 hashes.
#' @details As well as their versions and MD5 hashes, this function
#'   records the highest version ever recorded and the MD5 of that
#'   version. This information helps check if a package complies with
#'   best practices for version numbers: the version number should increment
#'   on each new release.
#' @return `NULL` (invisibly). Writes a package manifest as a JSON file.
#' @param manifest Character of length 1, file path to the JSON manifest.
#' @param repos Character string of package repositories to track.
record_versions <- function(
  manifest = "versions.json",
  repos = "https://r-releases.r-universe.dev"
) {
  current <- get_versions(repos = repos)
  if (!file.exists(manifest)) {
    jsonlite::write_json(x = current, path = manifest, pretty = TRUE)
    return(invisible())
  }
  previous <- read_previous(manifest = manifest)
  new <- update_manifest(current = current, previous = previous)
  jsonlite::write_json(x = new, path = manifest, pretty = TRUE)
}

get_versions <- function(repos) {
  out <- available.packages(repos = "https://r-releases.r-universe.dev")
  out <- as.data.frame(out)
  out <- out[, c("Package", "Version", "MD5sum")]
  colnames(out) <- c("package", "version_current", "md5_current")
  rownames(out) <- NULL
  out
}

read_previous <- function(manifest) {
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

update_manifest <- function(current, previous) {
  new <- merge(x = current, y = previous, all = TRUE)
  incremented <- apply(
    X = new,
    MARGIN = 1L,
    FUN = function(row) {
      print(.subset2(row, "version_current"))
      print(.subset2(row, "version_highest"))
      utils::compareVersion(
        a = .subset2(row, "version_current"),
        b = .subset2(row, "version_highest")
      ) > 0.5
    }
  )
  new$version_highest[incremented] <- new$version_current[incremented]
  new$md5_highest[incremented] <- new$md5_current[incremented]
  new
}
