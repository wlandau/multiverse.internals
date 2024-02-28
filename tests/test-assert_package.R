stopifnot(
  grepl(
    "Invalid package file path",
    r.releases.utils::assert_package(path = c(1L, 2L)),
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "does not exist",
    r.releases.utils::assert_package(path = tempfile()),
    fixed = TRUE
  )
)

path <- file.path(tempfile(), "hy-phens")
dir.create(dirname(path))
file.create(path)
stopifnot(
  grepl(
    "invalid package name",
    r.releases.utils::assert_package(path = path),
    fixed = TRUE
  )
)
unlink(dirname(path), recursive = TRUE)

path <- file.path(tempfile(), "package")
dir.create(dirname(path))
writeLines(letters, path)
stopifnot(
  grepl(
    "Invalid package URL",
    r.releases.utils::assert_package(path = path),
    fixed = TRUE
  )
)
unlink(dirname(path), recursive = TRUE)

path <- file.path(tempfile(), "package")
dir.create(dirname(path))
writeLines("b a d", path)
stopifnot(
  grepl(
    "Found malformed URL",
    r.releases.utils::assert_package(path = path),
    fixed = TRUE
  )
)
unlink(dirname(path), recursive = TRUE)

path <- file.path(tempfile(), "package")
dir.create(dirname(path))
writeLines("https://github.com/owner/package", path)
stopifnot(is.null(r.releases.utils::assert_package(path = path)))
unlink(dirname(path), recursive = TRUE)

stopifnot(
  grepl(
    "does not have URL",
    r.releases.utils::assert_package_url(
      name = "gh",
      url = "https://github.com/r-lib/gha"
    ),
    fixed = TRUE
  )
)

stopifnot(
  is.null(
    r.releases.utils::assert_package_url(
      name = "gh",
      url = "https://github.com/r-lib/gh"
    )
  )
)

stopifnot(
  is.null(
    r.releases.utils::assert_package_url(
      name = "curl",
      url = "https://github.com/jeroen/curl/"
    )
  )
)

stopifnot(
  is.null(
    r.releases.utils::assert_package_url(
      name = "curl",
      url = "https://github.com/jeroen/curl/"
    )
  )
)

stopifnot(
  is.null(
    r.releases.utils::assert_package_url(
      name = "jsonlite",
      url = "https://github.com/jeroen/jsonlite"
    )
  )
)

stopifnot(
  is.null(
    r.releases.utils::assert_package_url(
      name = "packageNOTonCRAN",
      url = "https://github.com/jeroen/jsonlite"
    )
  )
)
