% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/issues_dependencies.R
\name{issues_dependencies}
\alias{issues_dependencies}
\title{Report package dependency issues}
\usage{
issues_dependencies(packages, meta = meta_packages(), verbose = FALSE)
}
\arguments{
\item{packages}{Character vector of names of packages with other issues.}

\item{meta}{A data frame with R-universe package check results
returned by \code{\link[=meta_checks]{meta_checks()}}.}

\item{verbose}{\code{TRUE} to print progress while checking issues with
dependencies, \code{FALSE} otherwise.}
}
\value{
A nested list of problems triggered by dependencies.
The names of top-level elements are packages affected downstream.
The value of each top-level element is a list whose names are
Each element of this inner list is a character
vector of relevant dependencies of the downstream package.

For example, consider a linear dependency graph where \code{crew.cluster}
depends on \code{crew}, \code{crew} depends on \code{mirai}, and
\code{mirai} depends on \code{nanonext}. We represent the graph like this:
\code{nanonext -> mirai -> crew -> crew.cluster}.
If \code{nanonext} has an issue, then \code{\link[=issues_dependencies]{issues_dependencies()}} returns
\code{list(crew.cluster = list(nanonext = "crew"), ...)}, where \code{...}
stands for additional named list entries. From this list, we deduce
that \code{nanonext} is causing an issue affecting \code{crew.cluster} through
the direct dependency on \code{crew}.

The choice in output format from \code{\link[=issues_dependencies]{issues_dependencies()}} allows package
maintainers to more easily figure out which direct dependencies
are contributing issues and drop those direct dependencies if necessary.
}
\description{
Flag packages which have issues in their strong dependencies
(\verb{Imports:}, \verb{Depends:}, and \verb{LinkingTo:} in the \code{DESCRIPTION}.)
These include indirect/upstream dependencies, as well, not just
the explicit mentions in the \code{DESCRIPTION} file.
}
\section{Package issues}{

Functions like \code{\link[=issues_versions]{issues_versions()}} and \code{\link[=issues_descriptions]{issues_descriptions()}}
perform health checks for all packages in R-multiverse.
For a complete list of checks, see
the \verb{issues_*()} functions listed at
\url{https://r-multiverse.org/multiverse.internals/reference/index.html}.
\code{\link[=record_versions]{record_versions()}} updates the version number history
of releases in R-multiverse, and \code{\link[=record_issues]{record_issues()}} gathers
together all the issues about R-multiverse packages.
}

\examples{
  meta <- meta_packages(repo = "https://wlandau.r-universe.dev")
  issues_dependencies(packages = character(0L), meta = meta)
  issues_dependencies(packages = "crew.aws.batch", meta = meta)
  issues_dependencies(packages = "nanonext", meta = meta)
  issues_dependencies(packages = "crew", meta = meta)
  issues_dependencies(packages = c("crew", "mirai"), meta = meta)
}
\seealso{
Other issues: 
\code{\link{issues_checks}()},
\code{\link{issues_descriptions}()},
\code{\link{issues_versions}()}
}
\concept{issues}
