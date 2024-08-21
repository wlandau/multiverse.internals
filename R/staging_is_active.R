#' @title Check if the stating universe is active.
#' @export
#' @family staging
#' @description Check if the stating universe is active.
#' @return `TRUE` if the staging universe is active, `FALSE` otherwise.
#' @param thresholds Character vector of `"%m-%d"` dates that the
#'   staging universe becomes active. Staging will then last for a full calendar
#'   month until, but not including, the same date a month later.
#' @param today Character string with today's date in `"%Y-%m-%d"` format or an
#'   object convertible to POSIXlt format.
#' @examples
#' staging_is_active()
staging_is_active <- function(
  thresholds = c("01-15", "04-15", "07-15", "10-15"),
  today = Sys.Date()
) {
  today <- as.POSIXlt(today, tz = "UTC")
  within_freeze <- function(x, today) {
    mon <- today$mon + 1L
    day <- today$mday
    mon == x[1L] && day >= x[2L] || mon == x[1L] + 1L && day < x[2L]
  }
  thresholds <- strsplit(thresholds, split = "-", fixed = TRUE)
  thresholds <- lapply(thresholds, as.integer)
  within <- lapply(thresholds, within_freeze, today = today)
  any(as.logical(within))
}
