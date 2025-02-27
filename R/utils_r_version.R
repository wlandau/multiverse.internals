r_version_list <- function() {
  if (!is.null(cache[["r_versions"]])) {
    return(cache[["r_versions"]])
  }
  versions <- rversions::r_versions()
  versions <- versions[versions$date > as.POSIXct("2024-01-01"), ]
  versions$full <- versions$version
  versions$short <- r_version_short(versions$full)
  parsed <- lapply(versions$full, package_version)
  versions$major <- as.integer(lapply(parsed, \(x) unlist(x)[1L]))
  versions$minor <- as.integer(lapply(parsed, \(x) unlist(x)[2L]))
  versions$patch <- as.integer(lapply(parsed, \(x) unlist(x)[3L]))
  versions$version <- NULL
  versions$nickname <- NULL
  cache[["r_versions"]] <- versions
  versions
}

r_version_short <- function(version) {
  gsub(pattern = "\\.[0-9]+$", replacement = "", x = version)
}

r_version_major <- function(version) {
  out <- gsub(pattern = "\\.[0-9]+\\.[0-9]+$", replacement = "", x = version)
  as.integer(out)
}

r_version_minor <- function(version) {
  out <- gsub(pattern = "\\.[0-9]+$", replacement = "", x = version)
  out <- gsub(pattern = "^[0-9]+\\.", replacement = "", x = out)
  as.integer(out)
}

r_version_patch <- function(version) {
  out <- gsub(pattern = "^[0-9]+\\.[0-9]+\\.", replacement = "", x = version)
  as.integer(out)
}
