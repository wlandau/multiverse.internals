expect_error <- function(x, e = "") {
  invisible(
    grepl(e, tryCatch(x, error = identity)[["message"]], fixed = TRUE) ||
      stop("Error '", e, "' not generated")
  )
}

expect_error(
  r.releases.utils::assert_package(path = c(1L, 2L)),
  "Invalid package file path"
)

expect_error(
  r.releases.utils::assert_package(path = tempfile()),
  "does not exist"
)

path <- file.path(tempfile(), "hy-phens")
dir.create(dirname(path))
file.create(path)
expect_error(
  r.releases.utils::assert_package(path = path),
  "invalid package name"
)
unlink(dirname(path), recursive = TRUE)

path <- file.path(tempfile(), "package")
dir.create(dirname(path))
writeLines(letters, path)
expect_error(
  r.releases.utils::assert_package(path = path),
  "Invalid package URL"
)
unlink(dirname(path), recursive = TRUE)

path <- file.path(tempfile(), "package")
dir.create(dirname(path))
writeLines("b a d", path)
expect_error(
  r.releases.utils::assert_package(path = path),
  "Found malformed URL"
)
unlink(dirname(path), recursive = TRUE)

path <- file.path(tempfile(), "package")
dir.create(dirname(path))
writeLines("https://github.com/owner/package", path)
stopifnot(is.null(r.releases.utils::assert_package(path = path)))
unlink(dirname(path), recursive = TRUE)
