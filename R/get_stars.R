#' Retrieve GitHub Repo Star Hisotry
#'
#' Retrieve the star history of any GitHub repository
#'
#' @param repo `character`. Repository name(s) in the form `user/reponame`
#' @param pkg `character`. Name of an `R` package.
#'
#' @details
#' `get_pkg_star_history()` is a shortcut for retrieving the star history of an
#' `R` package. The function tries to find the GitHub repository of the package.
#' If it succeeds it continues calling `get_repo_star_history()`. If it fails either
#' a warning or an error is thrown depending on whether a GitHub repo couldn't be
#' found for some or all `pkg`, respectively.
#'
#' @return
#' An object of class `c("ghstars_history_tbl", "data.frame")` with 5 columns:
#' * `repo`: Name of the repository
#' * `date`: Date repository was starred
#' * `day`: Number of days since first star
#' * `stars`: Number of stars given at the current `date`
#' * `cumulative_stars`: Cumulative number of stars up to current `date`
#'
#' @author Thomas Neitmann
#'
#' @examples
#' \dontrun{
#' get_repo_star_history("thomas-neitmann/mdthemes")
#' }
#'
#' @export
get_repo_star_history <- function(repo) {
  if (length(repo) == 1L) {
    return(get_repo_star_history_single(repo))
  }
  ghstars_list <- lapply(repo, get_repo_star_history_single)
  do.call(rbind, ghstars_list)
}

#' @rdname get_repo_star_history
#' @export
get_pkg_star_history <- function(pkg) {
  repo <- get_pkg_repo(pkg)
  get_repo_star_history(repo)
}

get_repo_star_history_single <- memoise::memoise(function(repo) {
  stars <- gh::gh(
    endpoint = paste0("GET /repos/", repo, "/stargazers"),
    .accept = "application/vnd.github.v3.star+json",
    .limit = Inf
  )
  starred_at <- vapply(stars, `[[`, "", "starred_at")
  date_time <- strsplit(starred_at, "T")
  date_chr <- vapply(date_time, `[`, "", 1L)
  stars <- table(date_chr)
  date <- as.Date(names(stars))
  structure(
    data.frame(
      repo = repo,
      date = date,
      day = as.integer(date - date[1L]),
      stars = c(stars),
      cumulative_stars = cumsum(stars),
      row.names = NULL,
      stringsAsFactors = FALSE
    ),
    class = c("ghstars_history_tbl", "data.frame")
  )
})

#' Retrieve Metrics of a GitHub Repo
#'
#' Retrieve the current number of stars, forks, watchers and open issues of a
#' GitHub repository
#'
#' @param repo `character`. Repository name(s) in the form `user/reponame`
#' @param pkg `character`. Name of an `R` package.
#'
#' @details
#' `get_pkg_metrics()` is a shortcut for retrieving the metrics of an `R` package.
#' The function tries to find the GitHub repository of the package. If it
#' succeeds it continues calling `get_repo_metrics()`. If it fails either a warning
#' or an error is thrown depending on whether a GitHub repo couldn't be
#' found for some or all `pkg`, respectively.
#'
#' @return
#' An object of class `c("ghmetrics_tbl", "data.frame")` with 2 columns:
#' * `repo`        Name of the repository
#' * `stars`       Number of stars
#' * `forks`       Number of forks
#' * `watcher`     Number of watchers
#' * `open_issues` Number of open issue
#'
#' @author Thomas Neitmann
#'
#' @examples
#' \dontrun{
#' get_repo_metrics("thomas-neitmann/mdthemes")
#'
#' get_pkg_metrics(c("Rcpp", "scales"))
#' }
#'
#' @export
get_repo_metrics <- function(repo) {
  metrics <- lapply(repo, get_repo_metrics_single)
  do.call(rbind, metrics)
}

#' @rdname get_repo_metrics
#' @export
get_pkg_metrics <- function(pkg) {
  repo <- get_pkg_repo(pkg)
  get_repo_metrics(repo)
}

get_repo_metrics_single <- memoise::memoise(function(repo) {
  which <- c(
    stars = "stargazers_count",
    forks = "forks_count",
    watchers = "subscribers_count",
    open_issues = "open_issues_count"
  )

  repo_info <- gh::gh(endpoint = paste0("GET /repos/", repo))
  metrics <- data.frame(repo, repo_info[which], stringsAsFactors = FALSE)
  colnames(metrics) <- c("repo", names(which))
  class(metrics) <- c("ghmetrics_tbl", "data.frame")

  metrics
})
