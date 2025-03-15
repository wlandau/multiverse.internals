#' @title List package metadata
#' @export
#' @family meta
#' @description List package metadata in an R universe.
#' @return A data frame with one row per package and columns with package
#'   metadata.
#' @param repo URL of the repository to query.
#' @examples
#'   meta_packages(repo = "https://wlandau.r-universe.dev")
meta_packages <- function(repo = "https://community.r-multiverse.org") {
  meta_api <- get_meta_api(repo)
  meta_json <- get_meta_json(repo)
  meta_cran <- get_meta_cran()
  foss <- get_foss(repo)
  data <- merge(x = meta_api, y = meta_json, all.x = TRUE, all.y = FALSE)
  data <- merge(x = data, y = meta_cran, all.x = TRUE, all.y = FALSE)
  data$foss[data$package %in% foss] <- TRUE
  data
}

get_meta_json <- function(repo) {
  fields <- "Remotes"
  listing <- file.path(
    utils::contrib.url(repos = trim_url(repo), type = "source"),
    paste0("PACKAGES.json?fields=", fields)
  )
  data <- jsonlite::stream_in(
    con = gzcon(url(listing)),
    verbose = FALSE,
    simplifyVector = TRUE,
    simplifyDataFrame = TRUE,
    simplifyMatrix = TRUE
  )
  data <- clean_meta(data)
  data$foss <- FALSE
  if (is.null(data$remote)) {
    data$remotes <- replicate(nrow(data), NULL, simplify = FALSE)
  }
  data[, c("package", "remotes", "foss")]
}

get_foss <- function(repo) {
  data <- utils::available.packages(
    repos = trim_url(repo),
    filters = "license/FOSS"
  )
  as.character(data[, "Package"])
}

get_meta_cran <- function() {
  cran  <- utils::available.packages(repos = meta_snapshot()$cran)
  data.frame(
    package = as.character(cran[, "Package"]),
    cran = as.character(cran[, "Version"])
  )
}

get_meta_api <- function(repo) {
  base <- file.path(trim_url(repo), "api", "packages?stream=true&fields=")
  fields <- paste0(
    "_buildurl,_binaries,_failure,_published,",
    "_dependencies,Version,License,RemoteSha,Title,URL"
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

meta_api_postprocess <- function(data) {
  is_failure <- data$`_type` == "failure"
  data$description_url <- data$URL
  data$URL <- NULL
  data$url <- data[["_buildurl"]]
  failure <- data[["_failure"]]
  if (!is.null(failure)) {
    data$url[is_failure] <- failure$buildurl[is_failure]
    data$Version[is_failure] <- failure$version[is_failure]
    data$RemoteSha[is_failure] <- failure$commit$id[is_failure]
  }
  data$issues <- lapply(
    data[["_binaries"]],
    meta_api_issues_binaries,
    snapshot = meta_snapshot()
  )
  for (index in which(is_failure)) {
    data$issues[[index]]$source <- "FAILURE"
  }
  data <- clean_meta(data)
  fields <- c(
    "package", "url", "issues", "published", "dependencies",
    "version", "remotesha", "title", "description_url"
  )
  data[, fields]
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

clean_meta <- function(data) {
  colnames(data) <- tolower(colnames(data))
  colnames(data) <- gsub("^_", "", colnames(data))
  rownames(data) <- data$package
  data
}
