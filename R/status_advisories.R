#' @title Report security advisories
#' @export
#' @family status
#' @description Report security advisories for package versions
#'   noted in the R Consortium R Advisory Database:
#'   <https://github.com/RConsortium/r-advisory-database>.
#' @return A named list of security advisories about packages.
#' @inheritSection record_status Package status
#' @inheritParams status_dependencies
#' @examples
#'   meta <- meta_packages(repo = "https://wlandau.r-universe.dev")
#'   status <- status_advisories(meta = meta)
#'   str(status)
status_advisories <- function(meta = meta_packages()) {
  advisories <- read_advisories(timeout = 600000, retries = 3L)
  meta <- merge(
    x = meta,
    y = advisories,
    by = c("package", "version"),
    all.x = TRUE,
    all.y = FALSE
  )
  meta <- meta[!vapply(meta$advisories, anyNA, logical(1L)),, drop = FALSE] # nolint
  status_list(meta[, c("package", "advisories")])
}
