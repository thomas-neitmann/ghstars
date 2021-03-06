% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_stars.R
\name{get_repo_metrics}
\alias{get_repo_metrics}
\alias{get_pkg_metrics}
\title{Retrieve Metrics of a GitHub Repo}
\usage{
get_repo_metrics(repo)

get_pkg_metrics(pkg)
}
\arguments{
\item{repo}{\code{character}. Repository name(s) in the form \code{user/reponame}}

\item{pkg}{\code{character}. Name of an \code{R} package.}
}
\value{
An object of class \code{c("ghmetrics_tbl", "data.frame")} with 2 columns:
\itemize{
\item \code{repo}        Name of the repository
\item \code{stars}       Number of stars
\item \code{forks}       Number of forks
\item \code{watcher}     Number of watchers
\item \code{open_issues} Number of open issue
}
}
\description{
Retrieve the current number of stars, forks, watchers and open issues of a
GitHub repository
}
\details{
\code{get_pkg_metrics()} is a shortcut for retrieving the metrics of an \code{R} package.
The function tries to find the GitHub repository of the package. If it
succeeds it continues calling \code{get_repo_metrics()}. If it fails either a warning
or an error is thrown depending on whether a GitHub repo couldn't be
found for some or all \code{pkg}, respectively.
}
\examples{
\dontrun{
get_repo_metrics("thomas-neitmann/mdthemes")

get_pkg_metrics(c("Rcpp", "scales"))
}

}
\author{
Thomas Neitmann
}
