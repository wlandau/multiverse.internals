#' @title Write Rclone includes.
#' @export
#' @family staging
#' @description Write text files to pass to the `--include-from` flag
#'   in Rclone when uploading snapshots.
#' @inheritParams stage_candidates
#' @param repo_staging Character string, URL of the Staging universe.
#' @examples
#' \dontrun{
#' url_staging = "https://github.com/r-multiverse/staging"
#' path_staging <- tempfile()
#' path_community <- tempfile()
#' gert::git_clone(url = url_staging, path = path_staging)
#' stage_candidates(
#'   path_staging = path_staging,
#'   repo_staging = "https://staging.r-multiverse.org"
#' )
#' rclone_includes(
#'   path_staging,
#'   repo_staging = "https://staging.r-multiverse.org"
#' )
#' }
rclone_includes <- function(
  path_staging,
  repo_staging = "https://staging.r-multiverse.org",
  mock = NULL
) {
  write_include_packages(path_staging, repo_staging, mock)
  write_include_meta(path_staging)
}

write_include_packages <- function(path_staging, repo_staging, mock) {
  meta_staging <- mock$staging %||% meta_packages(repo_staging)
  staged <- staged_packages(path_staging)
  is_staged <- meta_staging$package %in% staged
  package <- meta_staging$package[is_staged]
  version <- meta_staging$version[is_staged]
  release <- paste0(package, "_", version)
  r <- meta_snapshot()$r
  source <- sprintf("src/contrib/%s.tar.gz", release)
  mac <- sprintf("bin/macosx/*/contrib/%s/%s.tgz", r, release)
  windows <- sprintf("bin/windows/contrib/%s/%s.zip", r, release)
  lines <- c(source, mac, windows)
  writeLines(lines, file.path(path_staging, "include-packages.txt"))
}

write_include_meta <- function(path_staging) {
  r <- meta_snapshot()$r
  source <- sprintf("src/contrib/PACKAGES")
  mac <- sprintf("bin/macosx/*/contrib/%s/PACKAGES", r)
  windows <- sprintf("bin/windows/contrib/%s/PACKAGES", r)
  lines <- c(source, mac, windows)
  lines <- c(lines, paste0(lines, ".gz"))
  writeLines(lines, file.path(path_staging, "include-meta.txt"))
}
