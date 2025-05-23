% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/review_pull_request.R
\name{review_pull_request}
\alias{review_pull_request}
\title{Review an R-multiverse contribution}
\usage{
review_pull_request(
  owner = "r-multiverse",
  repo = "contributions",
  number,
  advisories = NULL,
  organizations = NULL
)
}
\arguments{
\item{owner}{Character of length 1, name of the package repository owner.}

\item{repo}{URL of the repository to query.}

\item{number}{Positive integer of length 1, index of the pull request
in the repo.}

\item{advisories}{Character vector of names of packages with advisories
in the R Consortium Advisory Database.
If \code{NULL}, the function reads the database.}

\item{organizations}{Character vector of names of GitHub organizations.
Pull requests from authors who are not members of at least one of
these organizations will be flagged for manual review.
If \code{NULL}, the function reads the list of trusted organizations.}
}
\value{
\code{NULL} (invisibly).
}
\description{
Review a pull request to add packages to R-multiverse.
}
\seealso{
Other pull request reviews: 
\code{\link{review_pull_requests}()}
}
\concept{pull request reviews}
