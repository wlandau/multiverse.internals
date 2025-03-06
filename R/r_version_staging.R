#' @title Staging R version
#' @export
#' @family snapshot metadata
#' @description Get the version of R from the date of the start of the
#'   most recent Staging period.
#' @return A data frame with one row and columns for the `POSIXct` date,
#'   patch version, and minor version of the targeted version of R.
#' @examples
#'   r_version_staging()
r_version_staging <- function() {
  date <- date_staging()
  versions <- r_version_list()
  versions <- versions[versions$date <= as.POSIXct(date), ]
  utils::tail(versions, n = 1L)
}
