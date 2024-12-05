#' @title List metadata about R-universe package checks
#' @export
#' @family meta
#' @description List package checks results reported by the
#'   R-universe package API.
#' @return A data frame with one row per package and columns with
#'   package check results.
#' @param repo Character of length 1, URL of the package repository.
#'   R-multiverse uses `"https://community.r-multiverse.org"`.
#' @examples
#' meta_checks(repo = "https://wlandau.r-universe.dev")
meta_checks <- function(repo = "https://community.r-multiverse.org") {
  base <- file.path(trim_url(repo), "api", "packages?stream=true&fields=")
  out <- jsonlite::stream_in(
    con = gzcon(url(paste0(base, "_buildurl,_binaries"))),
    verbose = FALSE,
    simplifyVector = TRUE,
    simplifyDataFrame = TRUE,
    simplifyMatrix = TRUE
  )
  out$build_url <- out[["_buildurl"]]
  out$issues <- as.character(
    lapply(out[["_binaries"]], meta_checks_issues)
  )
  colnames(out) <- tolower(colnames(out))
  rownames(out) <- out$package
  out[, c("package", "build_url", "issues")]
}

meta_checks_issues <- function(binaries) {
  build <- .subset2(binaries, "status")
  check <- .subset2(binaries, "check")
  status <- .subset2(binaries, "status")
  no_check <- is.na(check)
  check[no_check] <- status[no_check]
  os <- .subset2(binaries, "os")
  is_failure <- (os != "wasm") &
    ((status != "success") | (check %in% c("WARNING", "ERROR")))
  if (!any(is_failure)) {
    return(NA_character_)
  }
  build <- build[is_failure]
  check <- check[is_failure]
  os <- os[is_failure]
  arch <- .subset2(binaries, "arch")
  if (!is.null(arch)) {
    os <- paste0(os, "_", arch[is_failure])
  }
  r <- .subset2(binaries, "r")[is_failure]
  paste(paste0(os, " R-", r, " ", check), collapse = ", ")
}
