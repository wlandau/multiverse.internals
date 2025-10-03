#' @title Review a package license.
#' @export
#' @family Manual package reviews
#' @description Review a package license string from the `DESCRIPTION` file
#'   to make sure the package has a valid free open-source (FOSS) license.
#'   This is vital to make sure R-multiverse has legal permission
#'   to distribute the package source code.
#' @return Invisibly returns
#'   `TRUE` if the package has a valid free open-source (FOSS)
#'   license according to [tools::analyze_license()]. `FALSE` otherwise.
#'   [review_license()] also prints an R console message
#'   to the communicate the result.
#'   Licenses for which [review_license()] returns `FALSE` are
#'   prohibited in R-multiverse.
#' @param license Character string with the `"License:"` field
#'   of the `DESCRIPTION` file of the package in question.
#' @examples
#'   review_license("MIT + file LICENSE")
#'   review_license("just file LICENSE")
review_license <- function(license) {
  assert_character_scalar(
    license,
    message = "license must be a single character string"
  )
  is_okay <- license_is_okay(license)
  if (is_okay) {
    cli::cli_alert_success("License \"{license}\" is okay.")
  } else {
    cli::cli_alert_danger("License \"{license}\" is prohibited.")
  }
  invisible(is_okay)
}

license_is_okay <- function(license) {
  license_data <- tools::analyze_license(license)
  isTRUE(license_data$is_canonical) &&
    (isTRUE(license_data$is_FOSS) || isTRUE(license_data$is_verified))
}

license_okay <- Vectorize(license_is_okay, "license", USE.NAMES = FALSE)
