#' @title Report `DESCRIPTION`-level status.
#' @export
#' @family status
#' @description Report issues with `DESCRIPTION`-level metadata of packages.
#' @details [status_descriptions()] reports specific issues in the
#'   `DESCRIPTION`-level metadata of packages:
#'   1. Security advisories at
#'     <https://github.com/RConsortium/r-advisory-database>.
#'   2. Licenses that cannot be verified as free and open-source.
#'   3. The presence of a `"Remotes"` field.
#'   4. A lower version number on R-multiverse than on CRAN,
#'     if the package is also published there.
#' @inheritSection record_status Package status
#' @return A named list of information about packages which do not comply
#'   with `DESCRPTION` checks. Each name is a package name,
#'   and each element contains specific information about
#'   non-compliance.
#' @param meta A data frame with R-universe package check results
#'   returned by [meta_packages()].
#' @examples
#'   meta <- meta_packages(repo = "https://wlandau.r-universe.dev")
#'   status <- status_descriptions(meta = meta)
#'   str(status)
status_descriptions <- function(meta = meta_packages()) {
  meta$issue <- F
  meta <- status_advisories(meta)
  meta <- status_licenses(meta)
  meta <- status_remotes(meta)
  meta <- status_version_conflict(meta, repo = "cran")
  meta <- meta[meta$issue,, drop = FALSE] # nolint
  fields <- c(
    "package",
    "advisories",
    "license",
    "remotes",
    "cran"
  )
  status_list(meta[, fields])
}


status_licenses <- function(meta) {
  meta$license[is.na(meta$license)] <- "unknown"
  status_list(meta[!meta$foss, c("package", "license")])
}

status_remotes <- function(meta) {
  meta[["remotes"]] <- meta[["remotes"]] %||% replicate(nrow(meta), NULL)
  meta$remotes <- lapply(meta$remotes, function(x) x[nzchar(x)])
  which <- vapply(meta$remotes, length, integer(1L)) > 0L
  status_list(meta[which, c("package", "remotes"), drop = FALSE])
}

status_version_conflicts <- function(meta, repo) {
  # NA is always lower than any version number.
  conflict <- vapply(
    X = seq_len(nrow(meta)),
    FUN = function(index) {
      utils::compareVersion(
        a = meta$version[index],
        b = meta[[repo]][index]
      ) < 0L
    },
    FUN.VALUE = logical(1L)
  )
  status_list(meta[conflict, c("package", repo)])
}
