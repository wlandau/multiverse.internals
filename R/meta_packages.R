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
  meta_api_postprocess(data)
}

get_meta_cran <- function() {
  cran <- utils::available.packages(repos = meta_snapshot()$cran)
  data.frame(
    package = as.character(cran[, "Package"]),
    cran = as.character(cran[, "Version"])
  )
}

get_foss <- function(license) {
  license <- trimws(license)
  foss <- rep(NA, length(license))
  # Pre-compute most common license types because tools::analyze_license()
  # is slow to iterate on large vectors of license specifications.
  common <- c(
    "Apache License (>= 2)",
    "Apache License (>= 2.0)",
    "MIT + file LICENSE",
    "GNU General Public License",
    "GPL-3",
    "GPL-2",
    "GPL (>= 3)",
    "GPL (>= 2)",
    "LGPL-3",
    "BSD_3_clause + file LICENSE",
    "BSD_2_clause + file LICENSE"
  )
  is_common <- foss %in% common
  foss[is_common] <- TRUE
  is_remainder <- !is_common & !is.na(license)
  foss[is_remainder] <- license_okay(license[is_remainder])
  foss
}

meta_api_postprocess <- function(data) {
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
  data[["_published"]] <- format_time_stamp(data[["_published"]])
  run <- utils::head(stats::na.omit(data[["_buildurl"]]), n = 1L)
  splits <- strsplit(run, split = "/", fixed = TRUE)[[1L]]
  data$monorepo <- splits[which(splits == "r-universe") + 1L]
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
    "version",
    "monorepo"
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
