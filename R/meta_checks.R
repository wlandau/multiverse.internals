#' @title List metadata about R-universe package checks
#' @export
#' @family meta
#' @description List package checks results reported by the
#'   R-universe package API.
#' @return A data frame with one row per package and columns with
#'   package check details.
#' @param repo Character of length 1, URL of the package repository.
#'   R-multiverse uses `"https://community.r-multiverse.org"`.
#' @examples
#' meta_checks(repo = "https://wlandau.r-universe.dev")
meta_checks <- function(repo = "https://community.r-multiverse.org") {
  base <- file.path(trim_url(repo), "api", "packages?stream=true&fields=")
  fields <- "_buildurl,_binaries,_failure,_published,Version,RemoteSha"
  json <- jsonlite::stream_in(
    con = gzcon(url(paste0(base, fields))),
    verbose = FALSE,
    simplifyVector = TRUE,
    simplifyDataFrame = TRUE,
    simplifyMatrix = TRUE
  )
  meta_checks_process_json(json)
}

meta_checks_process_json <- function(json) {
  is_failure <- json$`_type` == "failure"
  json$url <- json[["_buildurl"]]
  failure <- json[["_failure"]]
  if (!is.null(failure)) {
    json$url[is_failure] <- failure$buildurl[is_failure]
    json$Version[is_failure] <- failure$version[is_failure]
    json$RemoteSha[is_failure] <- failure$commit$id[is_failure]
  }
  json$issues <- lapply(
    json[["_binaries"]],
    meta_checks_issues_binaries,
    snapshot = meta_snapshot()
  )
  for (index in which(is_failure)) {
    json$issues[[index]]$source <- "FAILURE"
  }
  colnames(json) <- tolower(colnames(json))
  colnames(json) <- gsub("^_", "", colnames(json))
  rownames(json) <- json$package
  json[, c("package", "url", "issues", "published", "version", "remotesha")]
}

meta_checks_issues_binaries <- function(binaries, snapshot) {
  check <- .subset2(binaries, "check")
  os <- .subset2(binaries, "os")
  arch <- .subset2(binaries, "arch")
  r <- .subset2(binaries, "r")
  is_release <- r_version_short(r) == snapshot$r
  results <- c(
    target_check("linux", os, arch, r, is_release, check),
    target_check("mac", os, arch, r, is_release, check),
    target_check("win", os, arch, r, is_release, check)
  )
  as.list(results)
}

target_check <- function(
  target_os,
  os,
  arch,
  r,
  is_target_r,
  check
) {
  is_target <- (target_os == os) & is_target_r
  if (!any(is_target) || all(is.na(check[is_target]))) {
    out <- "MISSING"
    names(out) <- target_os
    return(out)
  }
  is_failure <- is_target & check %in% c("WARNING", "ERROR")
  if (!any(is_failure)) {
    return()
  }
  check <- check[is_failure]
  os <- os[is_failure]
  r <- paste0("R-", r[is_failure])
  if (!is.null(arch)) {
    arch <- arch[is_failure]
    os <- paste(os, arch, sep = " ")
  }
  names(check) <- paste(os, r, sep = " ")
  check
}
