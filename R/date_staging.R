#' @title Staging date
#' @export
#' @family snapshot metadata
#' @description Get the start date of the current or most recent
#'   Staging period.
#' @return Character string in `yyyy-mm-dd` format.
#' @param today A `"Date"` object with today's date, or an object
#'   that [as.Date()] can convert to a `"Date"` object.
#' @examples
#'   date_staging()
date_staging <- function(today = Sys.Date()) {
  today <- as.Date(today)
  year <- as.integer(format(today, "%Y"))
  years <- as.character(rep(c(year - 1L, year, year + 1L), each = 4L))
  months <- rep(c("02-15", "05-15", "08-15", "11-15"), times = 2L)
  dates <- as.Date(paste(years, months, sep = "-"))
  as.character(max(dates[dates <= today]))
}
