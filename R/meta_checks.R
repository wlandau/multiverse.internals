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
  check <- .subset2(binaries, "check")
  os <- .subset2(binaries, "os")
  r <- .subset2(binaries, "r")
  devel <- is_r_devel(r)
  release <- is_r_release(r)
  results <- c(
    target_check("linux", "R-devel", os, r, devel, check),
    target_check("mac", "R-release", os, r, release, check),
    target_check("win", "R-release", os, r, release, check)
  )
  if (length(results)) {
    paste(results, collapse = "\n")
  } else {
    NA_character_
  }
}

target_check <- function(target_os, target_r, os, r, is_target_r, check) {
  is_target <- (target_os == os) & is_target_r
  if (!any(is_target)) {
    return(paste0(target_os, " ", target_r, ": MISSING"))
  }
  is_failure <- is_target & check %in% c("WARNING", "ERROR")
  if (!any(is_failure)) {
    return(character(0L))
  }
  paste0(os[is_failure], " R-", r[is_failure], ": ", check[is_failure])
}

is_r_release <- function(r) {
  if (is.null(r_versions_envir$release)) {
    r_versions_envir$release <- rversions::r_release(dots = TRUE)$version
  }
  r == r_versions_envir$release
}

is_r_devel <- function(r) {
  if (is.null(r_versions_envir$all)) {
    history <- rversions::r_versions(dots = TRUE)
    cutoff <- as.POSIXct(Sys.Date() - as.difftime(104, units = "weeks"))
    r_versions_envir$all <- history$version[history$date > cutoff]
  }
  !(r %in% r_versions_envir$all)
}

r_versions_envir <- new.env(parent = emptyenv())
