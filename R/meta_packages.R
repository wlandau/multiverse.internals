#' @title List package metadata
#' @export
#' @family meta
#' @description List package metadata in an R universe.
#' @return A data frame with one row per package and columns with package
#'   metadata.
#' @param repo URL of the repository to query.
#' @examples
#' \dontrun{
#' meta_packages()
#' }
meta_packages <- function(repo = "https://community.r-multiverse.org") {
  merge(
    x = get_meta_api(repo),
    y = get_meta_cran(),
    all.x = TRUE,
    all.y = FALSE
  )
}

get_meta_api <- function(repo) {
  base <- file.path(trim_url(repo), "api", "packages?stream=true&fields=")
  fields <- paste0(
    "_buildurl,_binaries,_failure,_published,_dependencies,",
    "Version,License,Remotes,RemoteSha,Title,URL"
  )
  data <- jsonlite::stream_in(
    con = gzcon(url(paste0(base, fields))),
    verbose = FALSE,
    simplifyVector = TRUE,
    simplifyDataFrame = TRUE,
    simplifyMatrix = TRUE
  )
  data <- meta_api_postprocess(data)
  data$published <- format_time_stamp(data$published)
  data
}

get_meta_cran <- function() {
  cran  <- utils::available.packages(repos = meta_snapshot()$cran)
  data.frame(
    package = as.character(cran[, "Package"]),
    cran = as.character(cran[, "Version"])
  )
}

meta_api_postprocess <- function(data) {
  data$License[is.na(data$License)] <- "NOT FOUND"
  data$foss <- get_foss(data$License)
  if (is.null(data$remotes)) {
    data$remotes <- replicate(nrow(data), NULL, simplify = FALSE)
  }
  data$url_description <- data$URL
  data$URL <- NULL
  data$url_r_cmd_check <- data[["_buildurl"]]
  failure <- data[["_failure"]]
  is_failure <- data$`_type` == "failure"
  if (!is.null(failure)) {
    data$url_r_cmd_check[is_failure] <- failure$buildurl[is_failure]
    data$Version[is_failure] <- failure$version[is_failure]
    data$RemoteSha[is_failure] <- failure$commit$id[is_failure]
  }
  data$issues_r_cmd_check <- lapply(
    data[["_binaries"]],
    meta_api_issues_binaries,
    snapshot = meta_snapshot()
  )
  for (index in which(is_failure)) {
    data$issues_r_cmd_check[[index]]$source <- "FAILURE"
  }
  colnames(data) <- tolower(colnames(data))
  colnames(data) <- gsub("^_", "", colnames(data))
  fields <- c(
    "dependencies",
    "foss",
    "issues_r_cmd_check",
    "license",
    "package",
    "published",
    "remotes",
    "remotesha",
    "title",
    "url_description",
    "url_r_cmd_check",
    "version"
  )
  data[, fields]
}

get_foss <- function(license) {
  license <- trimws(license)
  foss <- rep(FALSE, length(license))
  # Pre-compute most common license types because tools::analyze_license()
  # is slow to iterate on large vectors of license specifications.
  common <- c(
    "MIT + file LICENSE",
    "GPL-3",
    "GPL-2"
  )
  is_common <- foss %in% common
  foss[is_common] <- TRUE
  foss[!is_common] <- license_okay(license[!is_common])
  foss
}

meta_api_issues_binaries <- function(binaries, snapshot) {
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

format_time_stamp <- function(time) {
  time <- as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  format(time, format = "%Y-%m-%d %H:%M:%OS3 %Z")
}
