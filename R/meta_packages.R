#' @title List package metadata
#' @export
#' @family meta
#' @description List package metadata in an R universe.
#' @return A data frame with one row per package and columns with package
#'   metadata.
#' @inheritParams meta_checks
#' @param fields Character string of fields to query.
#' @examples
#' meta_packages(repo = "https://wlandau.r-universe.dev")
meta_packages <- function(
  repo = "https://community.r-multiverse.org",
  fields = c("Version", "License", "Remotes", "RemoteSha")
) {
  repo <- trim_url(repo)
  listing <- file.path(
    utils::contrib.url(repos = repo, type = "source"),
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
  rownames(out) <- out$package
  foss <- utils::available.packages(repos = repo, filters = "license/FOSS")
  out$foss <- FALSE
  out[as.character(foss[, "Package"]), "foss"] <- TRUE
  out
}
