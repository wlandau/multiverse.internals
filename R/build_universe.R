#' @title Build the universe.
#' @export
#' @keywords internal
#' @description Create the `r-universe` `packages.json` file
#'   from constituent text files with URLs.
#' @return NULL (invisibly)
#' @param input Character of length 1, directory path with the
#'   text file listings of R releases.
#' @param output Character of length 1, file path where the
#'   `r-universe` `packages.json` file will be written.
build_universe <- function(input = getwd(), output = "packages.json") {
  assert_character_scalar(input, "invalid input")
  assert_character_scalar(output, "invalid output")
  assert_file(input)
  packages <- list.files(input, all.files = FALSE, full.names = TRUE)
  lapply(packages, assert_package)
  urls <- vapply(
    X = packages,
    FUN = readLines,
    FUN.VALUE = character(1L),
    USE.NAMES = FALSE,
    warn = FALSE
  )
  out <- data.frame(
    package = basename(packages),
    url = urls,
    branch = "*release"
  )
  if (!file.exists(dirname(output))) {
    dir.create(dirname(output))
  }
  jsonlite::write_json(x = out, path = output)
  invisible()
}
