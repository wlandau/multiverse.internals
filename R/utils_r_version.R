r_version_list <- function() {
  if (!is.null(cache[["r_versions"]])) {
    return(cache[["r_versions"]])
  }
  versions <- rversions::r_versions()
  versions <- versions[versions$date > as.POSIXct("2024-01-01"), ]
  versions <- data.frame(
    r = r_version_short(versions$version),
    date = versions$date
  )
  cache[["r_versions"]] <- versions
  versions
}

r_version_short <- function(version) {
  gsub(pattern = "\\.[0-9]+$", replacement = "", x = version)
}
