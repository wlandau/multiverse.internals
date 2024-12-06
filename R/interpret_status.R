#' @title Interpret the status of a package
#' @export
#' @family status
#' @description Summarize the issues of a package in human-readable text.
#' @return A character string summarizing the issues of a package in prose.
#' @param package Character string, name of the package.
#' @param issues A list with one issue per package. Obtained by
#'   reading the results of [record_issues()].
interpret_status <- function(package, issues) {
  if (is.null(issues[[package]])) {
    return(paste("Package", package, "has no recorded issues."))
  }
  issue <- issues[[package]]
  out <- paste(
    c(
      interpret_title(issue, package),
      interpret_advisories(issue),
      interpret_licenses(issue, package),
      interpret_checks(issue),
      interpret_dependencies(issue, package),
      interpret_remotes(issue),
      interpret_versions(issue)
    ),
    collapse = ""
  )
  trimws(out)
}

interpret_title <- function(issue, package) {
  title <- paste0(
    "R-multiverse found issues with package ",
    package
  )
  if (is.character(issue$version)) {
    title <- paste(title, "version", issue$version)
  }
  if (is.character(issue$remote_hash)) {
    title <- paste(title, "remote hash", issue$remote_hash)
  }
  paste0(title, " since ", issue$date, ".")
}

interpret_advisories <- function(issue) {
  advisories <- issue$descriptions$advisories
  if (is.null(advisories)) {
    return(character(0L))
  }
  paste0(
    "<br><br>Found the following advisories in the ",
    "R Consortium Advisory Database:<br><br>",
    as.character(yaml_html(advisories))
  )
}

interpret_checks <- function(issue) {
  checks <- issue$checks
  if (is.null(checks)) {
    return(character(0L))
  }
  checks$url <- sprintf("<a href=\"%s\">%s</a>", checks$url, checks$url)
  paste0(
    "<br><br>Not all checks succeeded on R-universe. ",
    "The following output shows the check status on each enforced platform ",
    "and version of R. The GitHub Actions URL links to the check logs ",
    "on all platforms that R-universe runs.",
    "Visit that URL to see specific details ",
    "on the check failures.<br><br>",
    as.character(yaml_html(checks))
  )
}

interpret_dependencies <- function(issue, package) {
  dependencies <- issue$dependencies
  if (is.null(dependencies)) {
    return(character(0L))
  }
  direct <- names(dependencies)[lengths(dependencies) < 1L]
  indirect <- setdiff(names(dependencies), direct)
  text <- paste0(
    "<br><br>One or more dependencies have issues. Packages ",
    paste(names(dependencies), collapse = ", "),
    " are causing problems upstream. "
  )
  if (length(direct)) {
    text <- paste0(
      text,
      ifelse(length(direct) == 1L, "Dependency ", "Dependencies "),
      paste(direct, collapse = ", "),
      ifelse(length(direct) == 1L, " is ", " are "),
      "explicitly mentioned in 'Depends:', 'Imports:', or 'LinkingTo:' ",
      "in the DESCRIPTION of ",
      package,
      ". "
    )
  }
  if (length(indirect)) {
    text <- paste0(
      text,
      ifelse(length(indirect) == 1L, "Package ", "Packages "),
      paste(indirect, collapse = ", "),
      ifelse(length(indirect) == 1L, " is ", " are "),
      "not part of 'Depends:', 'Imports:', or 'LinkingTo:' ",
      "in the DESCRIPTION of ",
      package,
      ", but ",
      ifelse(length(indirect) == 1L, "it is", "they are"),
      " upstream of one or more direct dependencies:<br><br>",
      as.character(yaml_html(dependencies[indirect]))
    )
  }
  text
}

interpret_licenses <- function(issue, package) {
  license <- issue$descriptions$license
  if (is.null(license)) {
    return(character(0L))
  }
  paste(
    "<br><br>Package",
    package,
    "declares license",
    shQuote(license),
    "in its DESCRIPTION file. R-multiverse cannot verify that",
    "this license is a valid free and open-source license",
    "(c.f. https://en.wikipedia.org/wiki/Free_and_open-source_software).",
    "Each package contributed to R-multiverse must have a valid",
    "open-source license to protect the intellectual property",
    "rights of the package owners."
  )
}

interpret_remotes <- function(issue) {
  remotes <- issue$descriptions$remotes
  if (is.null(remotes)) {
    return(character(0L))
  }
  paste0(
    "<br><br>Package releases should not use the 'Remotes:' field. Found:",
    as.character(yaml_html(remotes))
  )
}

interpret_versions <- function(issue) {
  versions <- issue$versions
  if (is.null(versions)) {
    return(character(0L))
  }
  paste0(
    "<br><br>The version number of the current release ",
    "should be highest version of all the releases so far. ",
    "Here is the current version of the package, ",
    "the highest version number ever recorded by R-multiverse, ",
    "and the latest remote hash of each:<br><br>",
    as.character(yaml_html(versions))
  )
}

yaml_html <- function(x) {
  out <- yaml::as.yaml(x, line.sep = "\n")
  gsub(pattern= "\n", replacement = "<br>", x = out)
}
