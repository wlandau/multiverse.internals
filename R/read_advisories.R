#' @title Read R Consortium advisories
#' @keywords internal
#' @description Read the R Consortium R Advisory database
#' @param timeout Number of milliseconds until the database download times
#'   out.
#' @param retries Number of retries to download the database.
read_advisories <- function(timeout = 600000, retries = 3L) {
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE, force = TRUE))
  zipfile <- file.path(path, "file.zip")
  for (try in seq_len(retries)) {
    response <- nanonext::ncurl(
      "https://github.com/RConsortium/r-advisory-database/zipball/main",
      convert = FALSE,
      follow = TRUE,
      timeout = timeout
    )
    if (all(response[["status"]] == 200L)) {
      break
    }
    if (all(try == retries)) {
      stop(
        "Failed to download R Consortium advisories database. Status: ",
        status_code(response[["status"]]),
        call. = FALSE
      )
    }
  }
  writeBin(response[["data"]], zipfile)
  unzip(zipfile, exdir = path, junkpaths = TRUE)
  advisories <- Sys.glob(file.path(path, "RSEC*.yaml"))
  out <- do.call(vctrs::vec_rbind, lapply(advisories, read_advisory))
  stats::aggregate(x = advisories ~ package + version, data = out, FUN = list)
}

read_advisory <- function(path) {
  out <- lapply(
    yaml::read_yaml(file = path)$affected,
    advisory_entry,
    path = path
  )
  do.call(vctrs::vec_rbind, out)
}

advisory_entry <- function(entry, path) {
  data.frame(
    package = entry$package$name,
    version = entry$versions,
    advisories = file.path(
      "https://github.com/RConsortium/r-advisory-database/blob/main/vulns",
      entry$package$name,
      basename(path)
    )
  )
}
