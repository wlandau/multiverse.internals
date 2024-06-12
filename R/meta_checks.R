#' @title List metadata about R-universe package checks
#' @export
#' @family list
#' @description List package checks results reported by the
#'   R-universe package API.
#' @return A data frame with one row per package and columns with
#'   package check results.
#' @param repo Character of length 1, URL of the package repository.
#'   R-multiverse uses `"https://multiverse.r-multiverse.org"`.
#' @examples
#' meta_checks(repo = "https://wlandau.r-universe.dev")
meta_checks <- function(repo = "https://multiverse.r-multiverse.org") {
  fields <- c(
    "_buildurl",
    "_linuxdevel",
    "_macbinary",
    "_wasmbinary",
    "_winbinary",
    "_status"
  )
  listing <- file.path(
    trim_url(repo),
    "api",
    paste0("packages?stream=true&fields=", paste(fields, collapse = ","))
  )
  out <- jsonlite::stream_in(
    con = gzcon(url(listing)),
    verbose = FALSE,
    simplifyVector = TRUE,
    simplifyDataFrame = TRUE,
    simplifyMatrix = TRUE
  )
  colnames(out) <- tolower(colnames(out))
  out
}
