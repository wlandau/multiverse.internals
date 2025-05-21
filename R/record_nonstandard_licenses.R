#' @title Record nonstandard licenses
#' @export
#' @description R-multiverse packages must have valid free and
#'   open-source (FOSS) licenses to protect the intellectual
#'   property rights of the package owners
#'   (c.f. <https://en.wikipedia.org/wiki/Free_and_open-source_software>).
#'   [record_nonstandard_licenses()] records packages with nonstandard
#'   licenses.
#' @return `NULL` (invisibly). Called for its side effects.
#' @param path_status Character string, local path to the `status.json` file
#'   of the repository.
#' @param path_nonstandard_licenses Character string,
#'   output path to write JSON data with the names
#'   and licenses of packages with non-standard licenses.
record_nonstandard_licenses <- function(
  path_status = "status.json",
  path_nonstandard_licenses = "nonstandard_licenses.json"
) {
  json_status <- jsonlite::read_json(path_status, simplifyVector = TRUE)
  nonstandard <- Filter(function(json) !is.null(json$license), json_status)
  package <- names(nonstandard)
  license <- as.character(lapply(nonstandard, function(json) json$license))
  jsonlite::write_json(
    x = data.frame(package = package, license = license),
    path = path_nonstandard_licenses,
    pretty = TRUE
  )
}
