#' @title Write Rclone includes.
#' @export
#' @family staging
#' @description Write text files to pass to the `--include-from` flag
#'   in Rclone when uploading snapshots.
#' @inheritParams stage_candidates
#' @examples
#' \dontrun{
#' url_staging = "https://github.com/r-multiverse/staging"
#' path_staging <- tempfile()
#' path_community <- tempfile()
#' gert::git_clone(url = url_staging, path = path_staging)
#' stage_candidates(path_staging = path_staging)
#' rclone_includes(path_staging)
#' }
rclone_includes <- function(path_staging) {
  write_include_packages(path_staging)
  write_include_meta(path_staging)
}

write_include_packages <- function(path_staging) {
  file_issues <- file.path(path_staging, "issues.json")
  json_issues <- jsonlite::read_json(file_issues, simplifyVector = TRUE)
  staged <- staged_packages(path_staging)
  json_staged <- json_issues[staged]
  package <- names(json_staged)
  version <- vapply(json_staged, \(x) x$version, character(1L))
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
