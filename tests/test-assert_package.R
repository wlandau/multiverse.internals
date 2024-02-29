stopifnot(
  grepl(
    "Invalid package name",
    r.releases.utils::assert_package(name = letters, url = "xy"),
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "Invalid package name",
    r.releases.utils::assert_package(
      name = ".gh",
      url = "https://github.com/r-lib/gh"
    ),
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "looks like custom JSON",
    r.releases.utils::assert_package(
      name = "xy",
      url = "{\"branch\": \"release\"}"
    ),
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "Invalid package URL",
    r.releases.utils::assert_package(
      name = "xy",
      url = letters
    ),
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "Invalid package URL",
    r.releases.utils::assert_package(name = "xy", url = letters),
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "Found malformed URL",
    r.releases.utils::assert_package(
      name = "gh",
      url = "github.com/r-lib/gh"
    ),
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "Found malformed URL",
    r.releases.utils::assert_package(
      name = "gh",
      url = "github.com/r-lib/gh"
    ),
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "appears to disagree with the repository name in the URL",
    r.releases.utils::assert_package(
      name = "gh2",
      url = "https://github.com/r-lib/gh"
    ),
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "is not https",
    r.releases.utils::assert_package(
      name = "gh",
      url = "http://github.com/r-lib/gh"
    ),
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "is not a GitHub or GitLab URL",
    r.releases.utils::assert_package(
      name = "gh",
      url = "https://github.gov/r-lib/gh"
    ),
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "appears to be an owner",
    r.releases.utils::assert_package(
      name = "gh",
      url = "https://github.com/gh"
    ),
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "appears to use a CRAN mirror",
    r.releases.utils::assert_package(
      name = "gh",
      url = "https://github.com/cran/gh"
    ),
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "does not appear in its DESCRIPTION file published on CRAN",
    r.releases.utils::assert_cran_url(
      name = "gh",
      url = "https://github.com/r-lib/gha"
    ),
    fixed = TRUE
  )
)

stopifnot(
  is.null(
    r.releases.utils::assert_package(
      name = "gh",
      url = "https://github.com/r-lib/gh"
    )
  )
)

stopifnot(
  is.null(
    r.releases.utils::assert_cran_url(
      name = "gh",
      url = "https://github.com/r-lib/gh"
    )
  )
)

stopifnot(
  is.null(
    r.releases.utils::assert_cran_url(
      name = "curl",
      url = "https://github.com/jeroen/curl/"
    )
  )
)

stopifnot(
  is.null(
    r.releases.utils::assert_cran_url(
      name = "curl",
      url = "https://github.com/jeroen/curl/"
    )
  )
)

stopifnot(
  is.null(
    r.releases.utils::assert_cran_url(
      name = "jsonlite",
      url = "https://github.com/jeroen/jsonlite"
    )
  )
)

stopifnot(
  is.null(
    r.releases.utils::assert_cran_url(
      name = "packageNOTonCRAN",
      url = "https://github.com/jeroen/jsonlite"
    )
  )
)
