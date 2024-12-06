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
  is_failure <- failed_check(check) & r_enforced(os, r)
  if (!any(is_failure)) {
    return(NA_character_)
  }
  check <- check[is_failure]
  os <- os[is_failure]
  arch <- .subset2(binaries, "arch")
  if (!is.null(arch)) {
    os <- paste0(os, "_", arch[is_failure])
  }
  r <- r[is_failure]
  paste(paste0(os, " R-", r, " ", check), collapse = ", ")
}

failed_check <- function(check) {
  check %in% c("WARNING", "ERROR")
}

r_enforced <- function(os, r) {
  is_release <- is_r_release(r)
  is_devel <- is_r_devel(r)
  (os == "linux" & is_devel) |
    (os == "mac" & is_release) |
    (os == "win" & is_release)
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
    r_versions_envir$all <- history$version[history$date > two_years_ago]
  }
  !(r %in% r_versions_envir$all)
}

r_versions_envir <- new.env(parent = emptyenv())
