#' @title Build the universe.
#' @export
#' @keywords internal
#' @description Create the `r-universe` `packages.json` file
#'   from constituent text files with URLs.
#' @return NULL (invisibly)
#' @param input Character of length 1, directory path with the
#'   text file listings of R releases.
#' @param output Character of length 1, file path where the
#'   `r-universe` `packages.json` file will be written.
#' @param release_exceptions Character vector of URLs of GitHub owners
#'   where `"branch": "*release"` should be omitted. Example:
#'   `"https://github.com/cran"`.
build_universe <- function(
  input = getwd(),
  output = "packages.json",
  release_exceptions = character(0L)
) {
  assert_character_scalar(input, "invalid input")
  assert_character_scalar(output, "invalid output")
  assert_file(input)
  packages <- list.files(input, all.files = FALSE, full.names = TRUE)
  message("Processing ", length(packages), " package entries.")
  entries <- lapply(
    X = packages,
    FUN = read_package_entry,
    release_exceptions = release_exceptions
  )
  message("Aggregating ", length(entries), " package entries.")
  aggregated <- do.call(what = vctrs::vec_rbind, args = entries)
  if (!file.exists(dirname(output))) {
    dir.create(dirname(output))
  }
  message("Writing ", output)
  jsonlite::write_json(x = aggregated, path = output, pretty = TRUE)
  invisible()
}

read_package_entry <- function(package, release_exceptions) {
  message("Processing package entry ", package)
  name <- trimws(basename(package))
  lines <- readLines(con = package, warn = FALSE)
  json <- try(jsonlite::parse_json(lines), silent = TRUE)
  if (inherits(json, "try-error")) {
    json <- package_entry_url(name = name, url = lines)
  } else {
    json <- package_entry_json(name = name, json = json)
  }
  decide_release_exceptions(
    json = json,
    release_exceptions = release_exceptions
  )
}

package_entry_url <- function(name, url) {
  message <- assert_package_lite(name = name, url = url)
  if (!is.null(message)) {
    stop(message, call. = FALSE)
  }
  data.frame(
    package = trimws(name),
    url = trimws(url),
    branch = "*release"
  )
}

package_entry_json <- function(name, json) {
  fields <- names(json)
  good_fields <- identical(
    sort(fields),
    sort(c("package", "url", "branch", "subdir"))
  )
  if (!good_fields) {
    stop(
      "Custom JSON entry for package ",
      shQuote(name),
      " must have fields 'packages', 'url', 'branch', and 'subdir' ",
      "and no other fields.",
      call. = FALSE
    )
  }
  if (!identical(name, json$package)) {
    stop(
      "The 'packages' field disagrees with the package name ",
      shQuote(name),
      call. = FALSE
    )
  }
  if (!identical(json$branch, "*release")) {
    stop(
      "The 'branch' field of package ",
      shQuote(name),
      "is not \"*release\".",
      call. = FALSE
    )
  }
  for (field in names(json)) {
    assert_character_scalar(
      x = json[[field]],
      message = paste(
        "Invalid value in field",
        shQuote(field),
        "in the JSON entry for package name",
        shQuote(name)
      )
    )
    json[[field]] <- trimws(json[[field]])
  }
  message <- assert_package_lite(name = json$package, url = json$url)
  if (!is.null(message)) {
    stop(message, call. = FALSE)
  }
  as.data.frame(json)
}

decide_release_exceptions <- function(json, release_exceptions) {
  if (dirname(trim_url(json$url)) %in% trim_url(release_exceptions)) {
    json$branch <- NULL
  }
  json
}
