#' @title Report packages with `Remotes:` fields.
#' @export
#' @family issues
#' @description Report packages with `Remotes:` fields in the `DESCRIPTION`
#'   file.
#' @inheritParams issues_advisories
#' @examples
#' \dontrun{
#' issues_remotes()
#' }
issues_remotes <- function(meta = meta_packages()) {
  meta[["remotes"]] <- meta[["remotes"]] %||% replicate(nrow(meta), NULL)
  meta$remotes <- lapply(meta$remotes, function(x) x[nzchar(x) & !is.na(x)])
  which <- vapply(meta$remotes, length, integer(1L)) > 0L
  meta[which, c("package", "remotes"), drop = FALSE]
}
