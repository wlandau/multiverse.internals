#' @title Snapshot date
#' @export
#' @family snapshot metadata
#' @description Get the target snapshot date of the current or most recent
#'   Staging period.
#' @return Character string in `yyyy-mm-dd` format.
#' @inheritParams date_staging
#' @examples
#'   date_snapshot()
date_snapshot <- function(today = Sys.Date()) {
  staging <- as.Date(date_staging(today = today)) +
    as.difftime(30, units = "days")
  year <- format(staging, "%Y")
  month <- format(staging, "%m")
  paste(year, month, "15", sep = "-")
}
