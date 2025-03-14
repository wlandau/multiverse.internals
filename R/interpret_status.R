#' @title Interpret the status of a package
#' @export
#' @family status
#' @description Summarize the status of a package in human-readable text.
#' @return A character string summarizing the status of a package in prose.
#' @param package Character string, name of the package.
#' @param status A list with one status entry per package. Obtained by
#'   reading the results of [record_status()].
interpret_status <- function(package, status) {
  status <- status[[package]]
  if (is.null(status)) {
    return(interpret_null(package))
  }
  if (isTRUE(status$success)) {
    return(interpret_success(status, package))
  }
  out <- paste(
    c(
      interpret_title(status, package),
      interpret_advisories(status),
      interpret_licenses(status, package),
      interpret_checks(status),
      interpret_dependencies(status, package),
      interpret_remotes(status),
      interpert_version_conflicts(status),
      interpret_versions(status)
    ),
    collapse = ""
  )
  trimws(out)
}

interpret_null <- function(package) {
  paste0("Data not found on package ", package, ".")
}

interpret_success <- function(status, package) {
  out <- paste0(
    "R-multiverse checks passed for package ",
    package
  )
  if (is.character(status$version)) {
    out <- paste(out, "version", status$version)
  }
  if (is.character(status$remote_hash)) {
    out <- paste(out, "remote hash", status$remote_hash)
  }
  paste0(
    out,
    " (last published at ",
    status$published,
    ")."
  )
}

interpret_title <- function(status, package) {
  title <- paste0(
    "R-multiverse found issues with package ",
    package
  )
  if (is.character(status$version)) {
    title <- paste(title, "version", status$version)
  }
  if (is.character(status$remote_hash)) {
    title <- paste(title, "remote hash", status$remote_hash)
  }
  paste0(title, " (last published at ", status$published, ").<br><br>")
}

interpret_advisories <- function(status) {
  advisories <- status$descriptions$advisories
  if (is.null(advisories)) {
    return(character(0L))
  }
  paste0(
    "Found the following advisories in the ",
    "R Consortium Advisory Database:<br>",
    as.character(yaml_html(advisories))
  )
}

interpret_checks <- function(status) {
  checks <- status$checks
  if (is.null(checks)) {
    return(character(0L))
  }
  checks$url <- sprintf("<a href=\"%s\">%s</a>", checks$url, checks$url)
  paste0(
    "Not all checks succeeded on R-universe. ",
    "The following output shows the check status on each enforced platform ",
    "and version of R. The GitHub Actions URL links to the check logs ",
    "on all platforms that R-universe runs.",
    "Visit that URL to see specific details ",
    "on the check failures.<br>",
    as.character(yaml_html(checks))
  )
}

interpret_dependencies <- function(status, package) {
  dependencies <- status$dependencies
  if (is.null(dependencies)) {
    return(character(0L))
  }
  direct <- names(dependencies)[lengths(dependencies) < 1L]
  indirect <- setdiff(names(dependencies), direct)
  text <- paste0(
    "One or more strong R package dependencies have issues: ",
    paste(names(dependencies), collapse = ", "),
    "."
  )
  if (length(direct)) {
    text <- paste0(
      text,
      ifelse(length(direct) == 1L, " Dependency ", " Dependencies "),
      paste(direct, collapse = ", "),
      ifelse(length(direct) == 1L, " is ", " are "),
      "explicitly mentioned in 'Depends:', 'Imports:', or 'LinkingTo:' ",
      "in the DESCRIPTION file of ",
      package,
      "."
    )
  }
  if (length(indirect)) {
    text <- paste0(
      text,
      ifelse(length(indirect) == 1L, " Dependency ", " Dependencies "),
      paste(indirect, collapse = ", "),
      ifelse(length(indirect) == 1L, " is ", " are "),
      "not part of 'Depends:', 'Imports:', or 'LinkingTo:' ",
      "in the DESCRIPTION file of ",
      package,
      ", but ",
      ifelse(length(indirect) == 1L, "it is", "they are"),
      " upstream of one or more strong direct dependencies:<br>",
      as.character(yaml_html(dependencies[indirect]))
    )
  } else {
    text <- paste0(text, "<br><br>")
  }
  text
}

interpret_licenses <- function(status, package) {
  license <- status$descriptions$license
  if (is.null(license)) {
    return(character(0L))
  }
  paste(
    "Package",
    package,
    "declares license",
    shQuote(license),
    "in its DESCRIPTION file. R-multiverse cannot verify that",
    "this license is a valid free and open-source license",
    "(c.f. https://en.wikipedia.org/wiki/Free_and_open-source_software).",
    "Each package contributed to R-multiverse must have a valid",
    "open-source license to protect the intellectual property",
    "rights of the package owners.<br><br>"
  )
}

interpret_remotes <- function(status) {
  remotes <- status$descriptions$remotes
  if (is.null(remotes)) {
    return(character(0L))
  }
  paste0(
    "Package releases should not use the 'Remotes:' field. Found:<br>",
    as.character(yaml_html(remotes))
  )
}

interpret_versions <- function(status) {
  versions <- status$versions
  if (is.null(versions)) {
    return(character(0L))
  }
  paste0(
    "The version number of the current release ",
    "should be highest version of all the releases so far. ",
    "Here is the current version of the package, ",
    "the highest version number ever recorded by R-multiverse, ",
    "and the latest remote hash of each:<br>",
    as.character(yaml_html(versions))
  )
}

interpert_version_conflicts <- function(status) {
  out <- character(0L)
  if (!is.null(status$descriptions$cran)) {
    out <- paste(
      out,
      "On CRAN, this package had version",
      status$descriptions$cran,
      "during the first day of the most recent R-multiverse Staging period.",
      "The version on R-multiverse is lower,",
      "which causes install.packages() to prefer CRAN.",
      "If you have not already done so, please ensure the latest",
      "GitHub/GitLab release has a suitably recent version",
      "in the DESCRIPTION file.<br><br>"
    )
  }
  trimws(out)
}

yaml_html <- function(x) {
  out <- yaml::as.yaml(x, line.sep = "\n")
  out <- sprintf("<pre>%s</pre>", out)
}
