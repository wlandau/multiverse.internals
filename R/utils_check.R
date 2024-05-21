check_list <- function(x) {
  package <- x$package
  x$package <- NULL
  out <- lapply(split(x, seq_len(nrow(x))), as.list)
  names(out) <- package
  out
}

get_package_index <- function(
  repo = "https://multiverse.r-multiverse.org",
  fields = character(0L)
) {
  listing <- file.path(
    contrib.url(repos = repo, type = "source"),
    paste0("PACKAGES.json?fields=", paste(fields, collapse = ","))
  )
  jsonlite::stream_in(
    con = gzcon(url(listing)),
    verbose = FALSE,
    simplifyVector = TRUE,
    simplifyDataFrame = TRUE,
    simplifyMatrix = TRUE
  )
}
