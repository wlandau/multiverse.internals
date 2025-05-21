#' @title Report package license issues
#' @export
#' @family issues
#' @description Report packages without standard free and open-source
#'   licenses.
#' @inheritParams issues_advisories
#' @examples
#' \dontrun{
#' issues_licenses()
#' }
issues_licenses <- function(meta = meta_packages()) {
  meta$foss[is.na(meta$foss)] <- FALSE # deliberately redundant
  meta[!meta$foss, c("package", "license"), drop = FALSE]
}
