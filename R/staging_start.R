#' @title Staging date
#' @export
#' @description Get the date of the start of the
#'   most recent Staging period.
#' @return Character string in `yyyy-mm-dd` format.
#' @examples
#'   staging_start()
staging_start <- function() {
  today <- Sys.Date()
  year <- as.integer(format(today, "%Y"))
  years <- as.character(rep(c(year - 1L, year), each = 4L))
  months <- rep(c("02-15", "05-15", "08-15", "11-15"), times = 2L)
  freezes <- as.Date(paste(years, months, sep = "-"))
  as.character(max(freezes[freezes <= today]))
}
