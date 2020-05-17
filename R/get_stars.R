#' @export
get_repo_star_history <- function(repo) {
  if (length(repo) == 1L) {
    return(get_repo_star_history_single(repo))
  }
  ghstars_list <- lapply(repo, get_repo_star_history_single)
  do.call(rbind, ghstars_list)
}

get_repo_star_history_single <- function(repo) {
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
      day = as.integer(date - date[1L] + 1L),
      stars = c(stars),
      cumulative_stars = cumsum(stars),
      row.names = NULL,
      stringsAsFactors = FALSE
    ),
    class = c("ghstars_tbl", "data.frame")
  )
}

#' @export
get_pkg_star_history <- function(pkg) {
  repo <- get_pkg_repo(pkg)
  get_repo_star_history(repo)
}
