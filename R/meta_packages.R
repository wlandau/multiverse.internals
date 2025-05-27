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
    "_binaries,_buildurl,_dependencies,_failure,_jobs,_published,",
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

meta_api_postprocess <- function(data) {
  data$License[is.na(data$License)] <- "NOT FOUND"
  data$foss <- get_foss(data$License)
  if (is.null(data$remotes)) {
    data$remotes <- replicate(nrow(data), NULL, simplify = FALSE)
  }
  failure <- data[["_failure"]]
  is_failure <- data$`_type` == "failure"
  if (!is.null(failure)) {
    data$Version[is_failure] <- failure$version[is_failure]
    data$RemoteSha[is_failure] <- failure$commit$id[is_failure]
  }
  data$issues_r_cmd_check <- lapply(
    seq_len(nrow(data)),
    function(index) {
      meta_api_issues_r_cmd_check(
        jobs = data[["_jobs"]][[index]],
        url = data[["_buildurl"]][index],
        snapshot = meta_snapshot()
      )
    }
  )
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
    "url",
    "version"
  )
  data[, fields]
}

meta_api_issues_r_cmd_check <- function(jobs, url, snapshot) {
  check <- .subset2(jobs, "check")
  r <- .subset2(jobs, "r")
  is_failure <- check %in% c("WARNING", "ERROR")
  is_release <- r_version_short(r) == .subset2(snapshot, "r")
  is_problem <- is_failure & is_release
  if (!any(is_problem)) {
    return(list())
  }
  data.frame(
    check = check[is_problem],
    config = .subset2(jobs, "config")[is_problem],
    r = r[is_problem],
    url = file.path(url, "job", .subset2(jobs, "job")[is_problem])
  )
}

format_time_stamp <- function(time) {
  time <- as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  format(time, format = "%Y-%m-%d %H:%M:%OS3 %Z")
}
