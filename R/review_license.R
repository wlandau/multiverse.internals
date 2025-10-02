#' @title Review a package license
#' @export
#' @family package reviews
#' @description Review a package license string from the `DESCRIPTION` file
#'   to make sure the package has a valid free open-source (FOSS) license.
#'   This is vital to make sure R-multiverse has legal permission
#'   to distribute the package source code.
#' @return `TRUE` if the package has a valid free open-source (FOSS)
#'   license according to [tools::analyze_license()]. `FALSE` otherwise.
#'   Licenses for which [review_license()] returns `FALSE` are
#'   prohibited in R-multiverse.
#' @param license Character string with the `"License:"` field
#'   of the `DESCRIPTION` file of the package in question.
#' @examples
#'   review_license("MIT + file LICENSE")
#'   review_license("just file LICENSE")
review_license <- Vectorize(
  function(license) {
    license_data <- tools::analyze_license(license)
    isTRUE(license_data$is_canonical) &&
      (isTRUE(license_data$is_FOSS) || isTRUE(license_data$is_verified))
  },
  "license",
  USE.NAMES = FALSE
)
