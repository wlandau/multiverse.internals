#' @title List package metadata
#' @export
#' @family meta
#' @description List package metadata in an R universe.
#' @return A data frame with one row per package and columns with package
#'   metadata.
#' @inheritParams meta_checks
#' @examples
#' meta_packages(repo = "https://wlandau.r-universe.dev")
meta_packages <- function(repo = "https://multiverse.r-multiverse.org") {
  fields <- c("Version", "Remotes", "RemoteSha")
  listing <- file.path(
    trim_trailing_slash(repo),
    "src",
    "contrib",
    paste0("PACKAGES.json?fields=", paste(fields, collapse = ","))
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
