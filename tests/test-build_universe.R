packages <- tempfile()
dir.create(packages)
writeLines("https://github.com/r-lib/gh", file.path(packages, "gh"))
writeLines(
  "https://github.com/jeroen/jsonlite",
  file.path(packages, "jsonlite")
)
universe <- file.path(tempfile(), "out")
r.releases.utils::build_universe(input = packages, output = universe)
json <- jsonlite::read_json(universe)
exp <- list(
  list(
    package = "gh",
    url = "https://github.com/r-lib/gh",
    branch = "*release"
  ),
  list(
    package = "jsonlite",
    url = "https://github.com/jeroen/jsonlite",
    branch = "*release"
  )
)
stopifnot(identical(json, exp))
unlink(packages, recursive = TRUE)
unlink(universe)
