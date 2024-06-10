#' @title Report package dependency issues
#' @export
#' @family issues
#' @description Flag packages which have issues in their strong dependencies
#'   (`Imports:`, `Depends:`, and `LinkingTo:` in the `DESCRIPTION`.)
#'   These include indirect/upstream dependencies, as well, not just
#'   the explicit mentions in the `DESCRIPTION` file.
#' @inheritSection record_issues Package issues
#' @return A nested list of problems triggered by dependencies.
#'   The names of top-level elements are packages affected downstream.
#'   The value of each top-level element is a list whose names are
#    packages at the source of a problem upstream.
#'   Each element of this inner list is a character
#'   vector of relevant dependencies of the downstream package.
#'
#'   For example, consider a linear dependency graph where `crew.cluster`
#'   depends on `crew`, `crew` depends on `mirai`, and
#'   `mirai` depends on `nanonext`. We represent the graph like this:
#'      `nanonext -> mirai -> crew -> crew.cluster`.
#'   If `nanonext` has an issue, then [issues_dependencies()] returns
#'   `list(crew.cluster = list(nanonext = "crew"), ...)`, where `...`
#'   stands for additional named list entries. From this list, we deduce
#'   that `nanonext` is causing an issue affecting `crew.cluster` through
#'   the direct dependency on `crew`.
#'
#'   The choice in output format from [issues_dependencies()] allows package
#'   maintainers to more easily figure out which direct dependencies
#'   are contributing issues and drop those direct dependencies if necessary.
#' @param names Names of packages with other issues.
#' @param meta A data frame with R-universe package check results
#'   returned by [meta_checks()].
#' @examples
#'   meta <- meta_packages(repo = "https://wlandau.r-universe.dev")
#'   issues_dependencies(packages = character(0L), meta = meta)
#'   issues_dependencies(packages = "crew.aws.batch", meta = meta)
#'   issues_dependencies(packages = "nanonext", meta = meta)
#'   issues_dependencies(packages = "crew", meta = meta)
#'   issues_dependencies(packages = c("crew", "mirai"), meta = meta)
issues_dependencies <- function(packages, meta) {
  graph <- issues_dependencies_graph(meta)
  vertices <- names(igraph::V(graph))
  issues <- list()
  for (package in intersect(packages, vertices)) {
    revdeps <- names(igraph::subcomponent(graph, v = package, mode = "out"))
    revdeps <- setdiff(revdeps, package)
    for (revdep in revdeps) {
      neighbors <- names(igraph::neighbors(graph, v = revdep, mode = "in"))
      issues[[revdep]][[package]] <- intersect(neighbors, revdeps)
    }
  }
  issues
}

issues_dependencies_graph <- function(meta) {
  repo_packages <- meta$package
  repo_dependencies <- meta[["_dependencies"]]
  edge_list <- list()
  for (index in seq_len(nrow(meta))) {
    package <- .subset2(repo_packages, index)
    all <- .subset2(repo_dependencies, index)
    packages <- .subset2(all, "package")
    role <- .subset2(all, "role")
    strong <- packages[role %in% c("Depends", "Imports", "LinkingTo")]
    strong <- intersect(setdiff(unique(strong), "R"), repo_packages)
    edges <- rep(strong, each = 2L)
    if (length(edges) >= 2L) {
      edges[seq(from = 2L, to = length(edges), by = 2L)] <- package
      edge_list[[index]] <- edges
    }
  }
  igraph::graph(edges = unlist(edge_list))
}
