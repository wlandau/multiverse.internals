#' @title Report package advisories
#' @export
#' @family issues
#' @description Report packages whose current versions have advisories
#'   in the R Consortium Advisory Database:
#'   <https://github.com/RConsortium/r-advisory-database>
#' @return A data frame with one row for each problematic package
#'   and columns with details.
#' @param meta Package metadata from [meta_packages()].
#' @examples
#' \dontrun{
#' issues_advisories()
#' }
issues_advisories <- function(meta = meta_packages()) {
  advisories <- read_advisories(timeout = 600000, retries = 3L)
  meta <- merge(
    x = meta,
    y = advisories,
    by = c("package", "version"),
    all.x = TRUE,
    all.y = FALSE
  )
  which <- !vapply(meta$advisories, anyNA, logical(1L))
  meta[which, c("package", "advisories"), drop = FALSE]
}
