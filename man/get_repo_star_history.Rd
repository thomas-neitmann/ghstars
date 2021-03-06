% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_stars.R
\name{get_repo_star_history}
\alias{get_repo_star_history}
\alias{get_pkg_star_history}
\title{Retrieve GitHub Repo Star Hisotry}
\usage{
get_repo_star_history(repo)

get_pkg_star_history(pkg)
}
\arguments{
\item{repo}{\code{character}. Repository name(s) in the form \code{user/reponame}}

\item{pkg}{\code{character}. Name of an \code{R} package.}
}
\value{
An object of class \code{c("ghstars_history_tbl", "data.frame")} with 5 columns:
\itemize{
\item \code{repo}: Name of the repository
\item \code{date}: Date repository was starred
\item \code{day}: Number of days since first star
\item \code{stars}: Number of stars given at the current \code{date}
\item \code{cumulative_stars}: Cumulative number of stars up to current \code{date}
}
}
\description{
Retrieve the star history of any GitHub repository
}
\details{
\code{get_pkg_star_history()} is a shortcut for retrieving the star history of an
\code{R} package. The function tries to find the GitHub repository of the package.
If it succeeds it continues calling \code{get_repo_star_history()}. If it fails either
a warning or an error is thrown depending on whether a GitHub repo couldn't be
found for some or all \code{pkg}, respectively.
}
\examples{
\dontrun{
get_repo_star_history("thomas-neitmann/mdthemes")
}

}
\author{
Thomas Neitmann
}
