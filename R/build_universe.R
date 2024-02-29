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
build_universe <- function(input = getwd(), output = "packages.json") {
  assert_character_scalar(input, "invalid input")
  assert_character_scalar(output, "invalid output")
  assert_file(input)
  packages <- list.files(input, all.files = FALSE, full.names = TRUE)
  entries <- lapply(X = packages, FUN = read_package_entry)
  aggregated <- do.call(what = vctrs::vec_rbind, args = entries)
  if (!file.exists(dirname(output))) {
    dir.create(dirname(output))
  }
  jsonlite::write_json(x = aggregated, path = output)
  invisible()
}

read_package_entry <- function(package) {
  name <- trimws(basename(package))
  lines <- readLines(con = package, warn = FALSE)
  out <- try(jsonlite::parse_json(lines), silent = TRUE)
  if (inherits(out, "try-error")) {
    package_entry_url(name = name, url = lines)
  } else {
    package_entry_json(name = name, json = out)
  }
}

package_entry_url <- function(name, url) {
  message <- assert_package(name = name, url = url)
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
  if (!all(c("package", "url", "branch") %in% names(json))) {
    stop(
      "Custom JSON entry for package ",
      shQuote(name),
      " must have fields 'package', 'url', and 'branch'.",
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
  as.data.frame(json)
}
