#' @title Record nonstandard licenses
#' @export
#' @description R-multiverse packages must have valid free and
#'   open-source (FOSS) licenses to protect the intellectual
#'   property rights of the package owners
#'   (c.f. <https://en.wikipedia.org/wiki/Free_and_open-source_software>).
#'   [record_nonstandard_licenses()] records packages with nonstandard
#'   licenses.
#' @return `NULL` (invisibly). Called for its side effects.
#' @param repo Character string, URL of the repository.
#' @param path Character string, output path to write JSON data with the names
#'   and licenses of packages with non-standard licenses.
record_nonstandard_licenses <- function(
  repo = "https://community.r-multiverse.org",
  path = "nonstandard_licenses.json"
) {
  url <- utils::contrib.url(trim_url(repo), type = "source")
  all <- utils::available.packages(contriburl = url)
  foss <- utils::available.packages(contriburl = url, filters = "license/FOSS")
  package <- as.character(setdiff(all[, "Package"], foss[, "Package"]))
  license <- as.character(all[package, "License"])
  jsonlite::write_json(
    x = data.frame(package = package, license = license),
    path = path,
    pretty = TRUE
  )
}
