#' @title Assert package `DESCRIPTION`
#' @keywords internal
#' @description Run basic assertions on the `DESCRIPTION` file of a package
#'   on GitHub or GitLab.
#' @return A character string with an informative message if a problem
#'   was found. Otherwise, `NULL` if there are no issues.
#' @param name Character string, name of the package listing contribution
#'   to R-multiverse.
#' @param url Character string, URL of the package on GitHub or GitLab.
assert_package_description <- function(name, url) {
  text <- try(get_repo_file(url, "DESCRIPTION"), silent = TRUE)
  if (inherits(text, "try-error")) {
    return(
      paste(
        "Could not read a DESCRIPTION file at the top-level of",
        url,
        sprintf("(error: %s)", conditionMessage(attr(text, "condition")))
      )
    )
  }
  assert_local_description(name, text)
}

assert_local_description <- function(name, text) {
  description <- try(
    desc::description$new(text = text),
    silent = TRUE
  )
  if (inherits(description, "try-error")) {
    return(
      paste(
        "DESCRPTION file could not be parsed. Original error:",
        conditionMessage(attr(description, "condition"))
      )
    )
  }
  assert_parsed_description(name, description)
}

assert_parsed_description <- function(name, description) {
  if (!identical(name, as.character(description$get("Package")))) {
    return(
      paste(
        "R-multiverse listing name",
        shQuote(name),
        "is different from the package name in the DESCRIPTION FILE:",
        shQuote(description$get("Package"))
      )
    )
  }
  authors <- c(
    description$get("Authors@R"),
    description$get("Author"),
    description$get("Maintainer")
  )
  if (!length(authors) || all(is.na(authors))) {
    return(
      paste(
        "DESCRIPTION of package",
        name,
        "does not list an author or maintainer.",
        "Each package contributed to R-multiverse must correctly attribute",
        "authorship and ownership to protect the intellectual property",
        "rights of the package owners."
      )
    )
  }
  license <- description$get("License")
  if (!(license %in% trusted_licenses)) {
    return(
      paste(
        "Detected license",
        shQuote(license),
        "which requires review by a moderator.",
        "Each package contributed to R-multiverse must have a valid",
        "open-source license to protect the intellectual property",
        "rights of the package owners."
      )
    )
  }
}

trusted_licenses <- c(
  "Apache License (== 2.0)",
  "Apache License (>= 2.0)",
  "Artistic-2.0",
  "Artistic License 2.0",
  "BSD_2_clause + file LICENCE",
  "BSD_3_clause + file LICENCE",
  "BSD_2_clause + file LICENSE",
  "BSD_3_clause + file LICENSE",
  "GPL-2",
  "GPL-3",
  "GPL (== 2)",
  "GPL (== 2)",
  "GPL (== 2.0)",
  "GPL (== 3)",
  "GPL (== 3.0)",
  "GPL (>= 2)",
  "GPL (>= 2)",
  "GPL (>= 2.0)",
  "GPL (>= 3)",
  "GPL (>= 3.0)",
  "LGPL-2",
  "LGPL-3",
  "LGPL (== 2)",
  "LGPL (== 2.1)",
  "LGPL (== 3)",
  "LGPL (>= 2)",
  "LGPL (>= 2.1)",
  "LGPL (>= 3)",
  "MIT + file LICENCE",
  "MIT + file LICENSE"
)
