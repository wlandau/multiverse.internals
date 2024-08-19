#' @title Check if the stating universe is active.
#' @export
#' @family staging
#' @description Check if the stating universe is active.
#' @return `TRUE` if the staging universe is active, `FALSE` otherwise.
#' @param thresholds Character vector of `"%m-%d"` dates that the
#'   staging universe becomes active.
#' @param duration Positive integer, number of days that the staging
#'   universe remains active after each threshold.
#' @examples
#' staging_is_active()
staging_is_active <- function(
  thresholds = c("01-15", "04-15", "07-15", "10-15"),
  duration = 30L
) {
  year <- format(Sys.Date(), "%Y")
  thresholds <- paste0(year, "-", thresholds)
  spec <- "%Y-%m-%d"
  spans <- lapply(
    thresholds,
    function(first) {
      format(
        seq(from = as.Date(first), by = "day", length.out = duration),
        spec
      )
    }
  )
  format(Sys.Date(), spec) %in% unlist(spans)
}
