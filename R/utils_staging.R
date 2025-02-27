#' @title Staging R version
#' @export
#' @description Get the version of R from the date of the start of the
#'   most recent Staging period.
#' @return A data frame with one row and columns for the `POSIXct` date,
#'   patch version, and minor version of the targeted version of R.
#' @examples
#'   staging_r_version()
staging_r_version <- function() {
  date <- staging_start()
  versions <- r_version_list()
  versions <- versions[versions$date <= as.POSIXct(date), ]
  utils::tail(versions, n = 1L)
}
