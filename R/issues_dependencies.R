#' @title Report package dependency issues
#' @export
#' @family status
#' @description Flag packages which have issues in their strong dependencies
#'   (`Imports:`, `Depends:`, and `LinkingTo:` in the `DESCRIPTION`.)
#'   These include indirect/upstream dependencies, as well, not just
#'   the explicit mentions in the `DESCRIPTION` file.
#' @inheritSection record_status Package status
#' @return A nested list of problems triggered by dependencies.
#'
#'   To illustrate the structure of this list, suppose
#'   Package `tarchetypes` depends on package `targets`, and packages
#'   `jagstargets` and `stantargets` depend on `tarchetypes`.
#'   In addition, package `targets` has a problem in `R CMD check`
#'   which might cause problems in `tarchetypes` and packages downstream.
#'
#'   `status_dependencies()` represents this information in the
#'   following list:
#'
#'   ```
#'   list(
#'     jagstargets = list(targets = "tarchetypes"),
#'     tarchetypes = list(targets = character(0)),
#'     stantargets = list(targets = "tarchetypes")
#'   )
#'   ```
#'
#'   In general, the returned list is of the form:
#'
#'   ```
#'   list(
#'     impacted_reverse_dependency = list(
#'       upstream_culprit = c("direct_dependency_1", "direct_dependency_2")
#'     )
#'   )
#'   ```
#'
#'   where `upstream_culprit` causes problems in `impacted_reverse_dependency`
#'   through direct dependencies `direct_dependency_1` and
#'   `direct_dependency_2`.
#' @param packages Character vector of names of packages with other issues.
#' @param meta A data frame with R-universe package check results
#'   returned by [meta_packages()].
#' @param verbose `TRUE` to print progress while checking
#'   dependency status, `FALSE` otherwise.
#' @examples
#'   meta <- meta_packages(repo = "https://wlandau.r-universe.dev")
#'   issues_dependencies(packages = character(0L), meta = meta)
#'   issues_dependencies(packages = "crew.aws.batch", meta = meta)
#'   issues_dependencies(packages = "nanonext", meta = meta)
#'   issues_dependencies(packages = "crew", meta = meta)
#'   issues_dependencies(packages = c("crew", "mirai"), meta = meta)
issues_dependencies <- function(
  packages,
  meta = meta_packages(),
  verbose = FALSE
) {
  if (verbose) message("Constructing the package dependency graph")
  graph <- status_dependencies_graph(meta)
  vertices <- names(igraph::V(graph))
  edges <- igraph::as_long_data_frame(graph)
  from <- tapply(
    X = as.character(edges$from_name),
    INDEX = as.character(edges$to_name),
    FUN = identity,
    simplify = FALSE
  )
  status <- list()
  for (package in intersect(packages, vertices)) {
    if (verbose) message("Flagging reverse dependencies of ", package)
    revdeps <- names(igraph::subcomponent(graph, v = package, mode = "out"))
    revdeps <- setdiff(revdeps, package)
    for (revdep in revdeps) {
      neighbors <- from[[revdep]]
      keep <- match(neighbors, revdeps, nomatch = 0L) > 0L
      status[[revdep]][[package]] <- neighbors[keep]
    }
  }
  out <- data.frame(package = names(status))
  out$dependencies <- unname(status)
  out
}

status_dependencies_graph <- function(meta) {
  repo_packages <- meta$package
  repo_dependencies <- meta$dependencies
  from <- list()
  to <- list()
  for (index in seq_len(nrow(meta))) {
    package <- .subset2(repo_packages, index)
    all <- .subset2(repo_dependencies, index)
    packages <- .subset2(all, "package")
    role <- .subset2(all, "role")
    strong <- packages[role %in% c("Depends", "Imports", "LinkingTo")]
    strong <- setdiff(unique(strong), "R")
    from[[index]] <- strong
    to[[index]] <- rep(package, length(strong))
  }
  from <- unlist(from, recursive = FALSE, use.names = FALSE)
  to <- unlist(to, recursive = FALSE, use.names = FALSE)
  keep <- match(from, repo_packages, nomatch = 0L) > 0L
  from <- from[keep]
  to <- to[keep]
  edges <- as.character(rbind(from, to))
  igraph::make_graph(edges = edges)
}
