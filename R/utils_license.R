license_okay <- Vectorize(
  function(license) {
    license_data <- tools::analyze_license(license)
    isTRUE(license_data$is_canonical) &&
      (isTRUE(license_data$is_FOSS) || isTRUE(license_data$is_verified))
  },
  "license",
  USE.NAMES = FALSE
)
