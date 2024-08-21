#' @title Check if the stating universe is active.
#' @export
#' @family staging
#' @description Check if the stating universe is active.
#' @return `TRUE` if the staging universe is active, `FALSE` otherwise.
#' @param start Character vector of `"%m-%d"` dates that the
#'   staging universe becomes active. Staging will then last for a full
#'   calendar month. For example, if you supply a start date of `"01-15"`,
#'   then the staging period will include all days from `"01-15"`
#'   through `"02-14"` and not include `"02-15"`.
#' @param today Character string with today's date in `"%Y-%m-%d"` format or an
#'   object convertible to POSIXlt format.
#' @examples
#' staging_is_active()
staging_is_active <- function(
  start = c("01-15", "04-15", "07-15", "10-15"),
  today = Sys.Date()
) {
  today <- as.POSIXlt(today, tz = "UTC")
  start <- strsplit(start, split = "-", fixed = TRUE)
  start <- lapply(start, as.integer)
  within <- lapply(start, within_staging, today = today)
  any(as.logical(within))
}

within_staging <- function(start, today) {
  month <- today$mon + 1L
  day <- today$mday
  if (start[1L] > 28L) {
    stop(
      "a staging start date cannot be later than day 28 of the given month.",
      call. = FALSE
    )
  }
  (month == start[1L] && day >= start[2L]) ||
    (month == start[1L] + 1L && day < start[2L])
}
